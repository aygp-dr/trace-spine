(ns fraud.specs-test
  "Generative checks for every pure s/fdef'd fn, plus data-spec sanity.
  Per https://clojure.org/guides/spec (Testing)."
  (:require [clojure.spec.alpha :as s]
            [clojure.spec.test.alpha :as stest]
            [clojure.test :refer [deftest is testing use-fixtures]]
            [clojure.tools.logging :as log]
            [clojure.tools.logging.impl :as log-impl]
            [fraud.core :as core]
            [fraud.specs :as specs]))

(use-fixtures :once
  (fn [f] (binding [log/*logger-factory* log-impl/disabled-logger-factory] (f))))

(def ^:private check-opts {:clojure.spec.test.check/opts {:num-tests 50}})

;; Side-effecting fns: fdef'd for instrumentation, never generatively checked.
;; With Redis and PostgreSQL unset (the default), the scorers run their
;; no-infrastructure paths, which is what stest/check exercises.
(def ^:private side-effecting
  #{`core/set-config! `core/init-redis! `core/init-db!   ; replace the global config/connections
    `core/start-server! `core/-main})                    ; Jetty

(defn- checkable []
  (remove side-effecting (stest/enumerate-namespace 'fraud.core)))

(deftest fdefs-hold-under-generative-testing
  (let [results (stest/check (checkable) check-opts)]
    (is (seq results) "expected at least one fdef'd fn to check")
    (doseq [r results]
      (testing (str (:sym r))
        (is (nil? (:failure r))
            (pr-str (stest/abbrev-result r)))))))

(deftest data-specs-generate-and-conform
  (doseq [k [::specs/transaction ::specs/condition ::specs/rules-signal ::specs/ml-signal]]
    (testing (str k)
      (is (every? (fn [[v _]] (s/valid? k v)) (s/exercise k 10))))))

(def ^:private example-tx
  "The transaction from the fraud.core comment block."
  {:transaction_id "txn_test123"
   :user_id "user_456"
   :amount_cents 9999
   :currency "USD"
   :ip_address "203.0.113.42"
   :email "test@example.com"
   :device_fingerprint "fp_xyz789"
   :billing_address {:country "US" :postal_code "94102"}
   :payment_method {:type "card" :bin "424242" :last_four "4242"}
   :metadata {:channel "web" :session_duration_ms 45000}})

(deftest real-values-conform
  (is (s/valid? ::specs/transaction example-tx))
  (let [response (core/score-transaction
                  example-tx "00-4bf92f3577b34da6a3ce929d0e0e4736-00f067aa0ba902b7-01")]
    (is (s/valid? ::specs/score-response response))
    (is (= "4bf92f3577b34da6a3ce929d0e0e4736" (:trace_id response)))))

(deftest rule-engine
  (let [saved @core/rules-cache]
    (try
      (reset! core/rules-cache
              {:rules [{:name "high-amount"
                        :condition "{\"gt\": [\"amount_cents\", 5000]}"
                        :score_delta 0.4}
                       {:name "eur-or-gbp"
                        :condition "{\"in\": [\"currency\", [\"EUR\", \"GBP\"]]}"
                        :score_delta 0.3}
                       {:name "not-test-email"
                        :condition "{\"not\": {\"match\": [\"email\", \".*@example\\\\.com\"]}}"
                        :score_delta 0.2}]
               :expires-at Long/MAX_VALUE})
      (is (= {:score 0.4 :triggered-rules ["high-amount"]}
             (core/evaluate-rules example-tx)))
      (finally (reset! core/rules-cache saved)))))
