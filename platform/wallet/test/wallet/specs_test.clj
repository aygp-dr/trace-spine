(ns wallet.specs-test
  "Generative checks for every pure s/fdef'd fn, plus data-spec sanity.
  Per https://clojure.org/guides/spec (Testing)."
  (:require [clojure.spec.alpha :as s]
            [clojure.spec.test.alpha :as stest]
            [clojure.test :refer [deftest is testing use-fixtures]]
            [trace-spine.specs :as ts]
            [wallet.core :as core]
            [wallet.specs :as specs]
            [wallet.trace :as trace]))

(def ^:private check-opts {:clojure.spec.test.check/opts {:num-tests 50}})

;; Side-effecting fns: fdef'd for instrumentation, never generatively checked.
(def ^:private side-effecting
  #{`core/apply-credits `core/refund-credits `core/validate-gift-card ; PostgreSQL
    `core/redeem-gift-card `core/earn-loyalty-points
    `core/-main})                                                      ; blocks forever

(defn- checkable []
  (remove side-effecting (stest/enumerate-namespace '[wallet.trace wallet.core])))

(deftest fdefs-hold-under-generative-testing
  (let [results (stest/check (checkable) check-opts)]
    (is (seq results) "expected at least one fdef'd fn to check")
    (doseq [r results]
      (testing (str (:sym r))
        (is (nil? (:failure r))
            (pr-str (stest/abbrev-result r)))))))

(deftest data-specs-generate-and-conform
  (doseq [k [::specs/traceparent-fields ::specs/trace-context ::specs/headers
             ::specs/apply-credits-opts ::specs/refund-credits-opts
             ::specs/earn-loyalty-opts ::specs/gift-card-validation]]
    (testing (str k)
      (is (every? (fn [[v _]] (s/valid? k v)) (s/exercise k 10))))))

(def ^:private example "00-4bf92f3577b34da6a3ce929d0e0e4736-00f067aa0ba902b7-01")

;; The examples from the wallet.trace comment block, run with instrumentation.
(use-fixtures :once
  (fn [f] (stest/instrument) (try (f) (finally (stest/unstrument)))))

(deftest real-values-conform
  (testing "wallet.trace comment examples"
    (is (= {:version "00" :trace-id "4bf92f3577b34da6a3ce929d0e0e4736"
            :span-id "00f067aa0ba902b7" :flags "01" :sampled? true}
           (trace/parse-traceparent example)))
    (is (= "4bf92f3577b34da6a3ce929d0e0e4736"
           (:trace-id (ts/traceparent-fields (trace/child-traceparent example)))))
    (is (trace/valid-traceparent? example))
    (is (not (trace/valid-traceparent?
              "00-00000000000000000000000000000000-00f067aa0ba902b7-01"))))
  (testing "wrap-trace-context continues the incoming trace"
    (let [seen (atom nil)
          app (trace/wrap-trace-context (fn [req] (reset! seen req) {:status 200}))]
      (app {:headers {"traceparent" example}})
      (is (s/valid? ::specs/trace-context (:trace-context @seen)))
      (is (= "4bf92f3577b34da6a3ce929d0e0e4736"
             (trace/extract-trace-id (get-in @seen [:trace-context :traceparent]))))))
  (testing "the apply-credits call from the wallet.core comment block"
    (is (s/valid? ::specs/apply-credits-opts
                  {:order-id "order-123"
                   :requested-amount 50.00M
                   :idempotency-key "order-123-wallet-apply-v1"}))))
