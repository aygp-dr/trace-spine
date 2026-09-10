(ns trace-spine.properties-test
  "L2 properties (spec/L2-properties.org) for the Clojure adapter, run as
  test.check properties over the trace-spine.specs generators. A failure
  prints the seed and the shrunk counterexample: the seed is the bug report."
  (:require [clojure.spec.alpha :as s]
            [clojure.spec.gen.alpha :as sgen]
            [clojure.spec.test.alpha :as stest]
            [clojure.test :refer [deftest is testing use-fixtures]]
            [clojure.test.check :as tc]
            [clojure.test.check.properties :as prop]
            [clojure.tools.logging :as log]
            [clojure.tools.logging.impl :as log-impl]
            [trace-spine.core :as core]
            [trace-spine.middleware :as mw]
            [trace-spine.specs :as specs]))

;; Exercise every s/fdef :args spec while the properties run; silence the
;; "malformed traceparent" warnings extract logs on purpose.
(use-fixtures :once
  (fn [f]
    (stest/instrument)
    (try
      (binding [log/*logger-factory* log-impl/disabled-logger-factory] (f))
      (finally (stest/unstrument)))))

(defn- holds? [property]
  (let [result (tc/quick-check 100 property)]
    (is (:pass? result)
        (pr-str (select-keys result [:seed :shrunk])))))

(defn- ids [ctx] (select-keys ctx [:trace-id :span-id :flags]))

(def ^:private gen-carrier-without-traceparent
  (sgen/such-that #(nil? (core/extract %)) (s/gen ::specs/carrier) 100))

(deftest roundtrip
  ;; extract(after(inject(ctx, carrier), carrier)) == Some(ctx)
  (holds? (prop/for-all [ctx (s/gen ::specs/trace-context)]
                        (= (ids ctx) (ids (core/extract (core/inject ctx {})))))))

(deftest malformed-headers-are-rejected
  ;; L1 parse: returns None iff s is not well-formed
  (holds? (prop/for-all [tp (specs/gen-malformed-traceparent)]
                        (and (nil? (core/parse-traceparent tp))
                             (not (core/valid-traceparent? tp))
                             (nil? (core/extract {"traceparent" tp}))
                             (nil? (core/continue-trace tp))))))

(deftest no-fabrication
  ;; I-spine-no-fabrication / ADR-0003: an internal service handed no valid
  ;; traceparent raises a programmer error and generates no trace id
  (holds? (prop/for-all [carrier gen-carrier-without-traceparent]
                        (= :programmer-error
                           (try (core/continue-or-start carrier false)
                                nil
                                (catch clojure.lang.ExceptionInfo e (:type (ex-data e)))))))
  (testing "an ingress starts a fresh, sampled root trace instead"
    (holds? (prop/for-all [carrier gen-carrier-without-traceparent]
                          (let [ctx (core/continue-or-start carrier true)]
                            (and (s/valid? ::specs/trace-context ctx)
                                 (nil? (:parent-span-id ctx))
                                 (core/sampled? ctx)))))))

(deftest continuation
  ;; every downstream call made while handling a request keeps its trace id
  (holds? (prop/for-all [tp (specs/gen-traceparent)
                         ingress? (sgen/boolean)]
                        (let [outbound (atom nil)
                              call (mw/wrap-http-client (fn [_url opts] (reset! outbound opts) {:status 200}))
                              app (mw/wrap-trace-context
                                   (fn [req] (call (:trace-context req) "http://wallet/balance") {:status 200 :headers {}})
                                   {:is-ingress? ingress?})
                              response (app {:headers {"traceparent" tp}})
                              incoming (core/parse-traceparent tp)
                              downstream (core/extract (:headers @outbound))
                              echoed (core/extract (:headers response))]
                          (and (= (:trace-id incoming) (:trace-id downstream) (:trace-id echoed))
                               (= (:flags incoming) (:flags downstream))
                               (not= (:span-id incoming) (:span-id downstream)))))))

(deftest internal-call-without-traceparent
  (let [app (mw/wrap-trace-context (fn [_] {:status 200 :headers {}})
                                   {:is-ingress? false :strict-mode? true})]
    (testing "strict mode rejects it"
      (is (= :programmer-error
             (try (app {:headers {}}) nil
                  (catch clojure.lang.ExceptionInfo e (:type (ex-data e))))))))
  (testing "non-strict mode fails open with a fresh trace"
    (let [app (mw/wrap-trace-context (fn [_] {:status 200 :headers {}})
                                     {:is-ingress? false})]
      (is (s/valid? ::specs/traceparent (get-in (app {:headers {}}) [:headers "traceparent"]))))))
