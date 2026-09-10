(ns payments.specs-test
  "Generative checks for every pure s/fdef'd fn, plus data-spec sanity.
  Per https://clojure.org/guides/spec (Testing)."
  (:require [clojure.spec.alpha :as s]
            [clojure.spec.test.alpha :as stest]
            [clojure.test :refer [deftest is testing use-fixtures]]
            [clojure.tools.logging :as log]
            [clojure.tools.logging.impl :as log-impl]
            [jsonista.core :as json]
            [payments.circuit-breaker]
            [payments.core :as core]
            [payments.idempotency]
            [payments.outbox :as outbox]
            [payments.retry]
            [payments.specs :as specs]))

(use-fixtures :once
  (fn [f] (binding [log/*logger-factory* log-impl/disabled-logger-factory] (f))))

(def ^:private check-opts {:clojure.spec.test.check/opts {:num-tests 50}})

(def ^:private api-nses
  '[payments.core payments.retry payments.circuit-breaker payments.outbox payments.idempotency])

;; Not generatively checked:
(def ^:private excluded
  #{`core/configure! `payments.circuit-breaker/configure!         ; replace global config
    `core/orchestrate-payment `core/-main                          ; PostgreSQL + processors; prints
    `payments.retry/with-retry                                     ; sleeps; see properties-test
    `payments.retry/with-retry-async                               ; not implemented (throws)
    `payments.circuit-breaker/with-circuit-breaker                 ; global circuits; see properties-test
    `payments.circuit-breaker/reset! `payments.circuit-breaker/reset-all!
    `outbox/ensure-table! `outbox/publish `outbox/publish-batch    ; PostgreSQL
    `outbox/fetch-unpublished `outbox/mark-published
    `outbox/increment-retry-count `outbox/move-to-dead-letter
    `payments.idempotency/check `payments.idempotency/mark-in-progress
    `payments.idempotency/mark-complete `payments.idempotency/mark-complete-in-tx
    `payments.idempotency/mark-failed `payments.idempotency/cleanup-expired
    `payments.idempotency/ensure-table!})

(defn- checkable []
  (remove excluded (stest/enumerate-namespace api-nses)))

(deftest fdefs-hold-under-generative-testing
  (let [results (stest/check (checkable) check-opts)]
    (is (seq results) "expected at least one fdef'd fn to check")
    (doseq [r results]
      (testing (str (:sym r))
        (is (nil? (:failure r))
            (pr-str (stest/abbrev-result r)))))))

(deftest data-specs-generate-and-conform
  (doseq [k [::specs/retry-policy ::specs/circuit-config ::specs/payment-request
             ::specs/payment-result ::specs/outbox-event ::specs/payment-completed-event]]
    (testing (str k)
      (is (every? (fn [[v _]] (s/valid? k v)) (s/exercise k 10))))))

(def ^:private request
  "The orchestrate-payment call from the payments.core comment block."
  {:idempotency-key "test-key-1" :amount 10000 :currency "usd"
   :customer-id "cust_123" :order-id "ord_456" :source "tok_visa"
   :apply-wallet-credit true})

(use-fixtures :each
  (fn [f] (stest/instrument) (try (f) (finally (stest/unstrument)))))

(deftest real-values-conform
  (is (s/valid? ::specs/payment-request request))
  (is (s/valid? ::specs/payment-completed-event
                (outbox/make-payment-completed-event "pay_1" "ord_456" "ch_1" 10000 "stripe")))
  (testing "an internal call without a traceparent is refused before any I/O (ADR-0003)"
    (let [response ((core/create-app {:dbtype "postgresql"})
                    {:request-method :post :uri "/charge" :body request :headers {}})]
      (is (= 500 (:status response)))
      (is (= "Trace context required"
             (get-in (json/read-value (:body response) json/keyword-keys-object-mapper)
                     [:error :message])))))
  (testing "unknown routes are a 404"
    (is (= 404 (:status ((core/create-app {}) {:request-method :get :uri "/nope"}))))))
