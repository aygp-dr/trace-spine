(ns api.specs-test
  "Generative checks for every pure s/fdef'd fn, plus data-spec sanity.
  Per https://clojure.org/guides/spec (Testing)."
  (:require [api.core :as core]
            [api.specs :as specs]
            [clojure.spec.alpha :as s]
            [clojure.spec.gen.alpha :as sgen]
            [clojure.spec.test.alpha :as stest]
            [clojure.test :refer [deftest is testing use-fixtures]]
            [clojure.test.check :as tc]
            [clojure.test.check.properties :as prop]
            [clojure.tools.logging :as log]
            [clojure.tools.logging.impl :as log-impl]
            [trace-spine.specs :as ts]))

(use-fixtures :once
  (fn [f] (binding [log/*logger-factory* log-impl/disabled-logger-factory] (f))))

(def ^:private check-opts {:clojure.spec.test.check/opts {:num-tests 50}})

;; Side-effecting fns: fdef'd for instrumentation, never generatively checked.
(def ^:private side-effecting
  #{`core/start-server! `core/stop-server! `core/-main})   ; Jetty

(defn- checkable []
  (remove side-effecting (stest/enumerate-namespace 'api.core)))

(deftest fdefs-hold-under-generative-testing
  (let [results (stest/check (checkable) check-opts)]
    (is (seq results) "expected at least one fdef'd fn to check")
    (doseq [r results]
      (testing (str (:sym r))
        (is (nil? (:failure r))
            (pr-str (stest/abbrev-result r)))))))

(deftest data-specs-generate-and-conform
  (doseq [k [::specs/trace-opts ::specs/request ::specs/response]]
    (testing (str k)
      (is (every? (fn [[v _]] (s/valid? k v)) (s/exercise k 10))))))

(defn- holds? [property]
  (let [result (tc/quick-check 100 property)]
    (is (:pass? result) (pr-str (select-keys result [:seed :shrunk])))))

(deftest gateway-always-answers-with-a-traceparent
  ;; The gateway is the ingress (ADR-0003): whatever traceparent a client
  ;; sends, well-formed or not, the response carries a well-formed one, and
  ;; a well-formed incoming trace is continued rather than replaced.
  (holds? (prop/for-all [tp (s/gen ::ts/traceparent-candidate)
                         ingress? (sgen/boolean)]
                        (let [app (core/wrap-trace-context (fn [_] {:status 200 :headers {}})
                                                           {:is-ingress? ingress?})
                              response (app {:headers (cond-> {} tp (assoc "traceparent" tp))})
                              out (get-in response [:headers "traceparent"])]
                          (and (s/valid? ::ts/traceparent out)
                               (= (get-in response [:headers "X-Trace-Id"])
                                  (:trace-id (ts/traceparent-fields out)))
                               (or (not (s/valid? ::ts/traceparent tp))
                                   (= (:trace-id (ts/traceparent-fields tp))
                                      (:trace-id (ts/traceparent-fields out)))))))))

(deftest real-values-conform
  (let [ctx {:trace-id "4bf92f3577b34da6a3ce929d0e0e4736" :span-id "00f067aa0ba902b7" :flags "01"}]
    (is (= {:status 200 :headers {} :body {:data [] :pagination {:page 1 :limit 20 :total 0 :total_pages 0}
                                           :meta {:trace_id (:trace-id ctx)}}}
           (core/list-products-handler {:query-params {} :trace-context ctx})))
    (is (= 401 (:status ((core/auth-required (fn [_] {:status 200})) {:headers {}}))))))
