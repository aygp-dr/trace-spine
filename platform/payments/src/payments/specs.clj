(ns payments.specs
  "Data specs for the Payment Service (https://clojure.org/guides/spec).

  The traceparent/tracestate headers come from trace-spine.specs (lib/clj),
  the monorepo's single definition of the W3C wire format. Function specs
  (s/fdef) live next to each defn in payments.core, payments.retry,
  payments.circuit-breaker, payments.outbox and payments.idempotency."
  (:require [clojure.spec.alpha :as s]
            [clojure.spec.gen.alpha :as gen]
            [payments.circuit :as-alias circuit]
            [payments.event :as-alias event]
            [payments.policy :as-alias policy]
            [payments.request :as-alias req]
            [payments.result :as-alias result]
            [trace-spine.specs :as ts]))

(defn- gen-id [prefix]
  (gen/fmap #(str prefix %) (gen/large-integer* {:min 1 :max 9999999})))

(s/def ::db-spec
  (s/with-gen some?
    #(gen/return {:dbtype "postgresql" :dbname "payments"})))

;; =============================================================================
;; Retry policy (payments.retry: every key optional, with documented defaults)
;; =============================================================================

(s/def ::policy/max-attempts (s/int-in 1 11))
(s/def ::policy/initial-delay-ms (s/int-in 0 1001))
(s/def ::policy/max-delay-ms (s/int-in 0 60001))
(s/def ::policy/multiplier (s/double-in :min 1.0 :max 3.0 :NaN? false :infinite? false))
(s/def ::policy/jitter-factor (s/double-in :min 0.0 :max 1.0 :NaN? false :infinite? false))
(s/def ::retry-policy
  (s/keys :opt-un [::policy/max-attempts ::policy/initial-delay-ms ::policy/max-delay-ms
                   ::policy/multiplier ::policy/jitter-factor]))

(s/def ::attempt (s/int-in 0 20))

;; Error :type values payments.retry classifies.
(s/def ::retryable-type
  #{:network-timeout :connection-refused :connection-reset :rate-limited
    :service-unavailable :gateway-timeout :lock-timeout :temporary-failure})
(s/def ::non-retryable-type
  #{:card-declined :insufficient-funds :invalid-card :expired-card :fraud-detected
    :invalid-request :authentication-failed :not-found :conflict})

;; =============================================================================
;; Circuit breaker
;; =============================================================================

(s/def ::circuit-key (s/with-gen keyword? #(gen/elements [:stripe :paypal :wallet :fraud])))
(s/def ::circuit-state #{:closed :open :half-open})

(s/def ::circuit/failure-threshold (s/int-in 1 20))
(s/def ::circuit/success-threshold (s/int-in 1 10))
(s/def ::circuit/reset-timeout-ms nat-int?)
(s/def ::circuit/half-open-max-calls (s/int-in 1 10))
(s/def ::circuit-config
  (s/keys :opt-un [::circuit/failure-threshold ::circuit/success-threshold
                   ::circuit/reset-timeout-ms ::circuit/half-open-max-calls]))

(s/def ::circuit/total nat-int?)
(s/def ::circuit/successful nat-int?)
(s/def ::circuit/failed nat-int?)
(s/def ::circuit/rejected nat-int?)
(s/def ::circuit/state ::circuit-state)
(s/def ::circuit-metrics
  (s/keys :req-un [::circuit/total ::circuit/successful ::circuit/failed
                   ::circuit/rejected ::circuit/state]))

;; =============================================================================
;; Payment requests and results (payments.core/orchestrate-payment)
;; =============================================================================

(s/def ::req/idempotency-key (s/with-gen (s/and string? seq) #(gen-id "idem-")))
(s/def ::req/amount (s/int-in 1 10000000))
(s/def ::req/currency #{"usd" "eur" "gbp"})
(s/def ::req/customer-id (s/with-gen (s/and string? seq) #(gen-id "cust_")))
(s/def ::req/order-id (s/with-gen (s/and string? seq) #(gen-id "ord_")))
(s/def ::req/source #{"tok_visa" "tok_mastercard" "pm_card_visa"})
(s/def ::req/processor #{:stripe :paypal})
(s/def ::req/apply-wallet-credit boolean?)
(s/def ::payment-request
  (s/keys :req-un [::req/idempotency-key ::req/amount ::req/currency
                   ::req/customer-id ::req/order-id ::req/source]
          :opt-un [::req/processor ::req/apply-wallet-credit]))

(s/def ::result/status #{:success :declined :error})
(s/def ::result/payment-id string?)
(s/def ::result/charge-id (s/nilable string?))
(s/def ::result/amount-charged nat-int?)
(s/def ::result/wallet-applied number?)
(s/def ::result/processor #{:stripe :paypal})
(s/def ::payment-result
  (s/keys :req-un [::result/status]
          :opt-un [::result/payment-id ::result/charge-id ::result/amount-charged
                   ::result/wallet-applied ::result/processor]))

(s/def ::service-config map?)

;; =============================================================================
;; Idempotency keys (payments.idempotency)
;; =============================================================================

(s/def ::idempotency-status #{:new :duplicate :in-progress :failed})
(s/def ::status ::idempotency-status)
(s/def ::idempotency-check (s/keys :req-un [::status]))

;; =============================================================================
;; Outbox events (payments.outbox, spec/L1-wire.org outbox table)
;; =============================================================================

(defn- iso-instant? [s]
  (try (some? (java.time.Instant/parse s)) (catch Exception _ false)))

(s/def ::event/payment-id (s/with-gen (s/and string? seq) #(gen/fmap str (gen/uuid))))
(s/def ::event/order-id ::req/order-id)
(s/def ::event/charge-id (s/nilable (s/with-gen string? #(gen-id "ch_"))))
(s/def ::event/amount-cents nat-int?)
(s/def ::event/processor #{"stripe" "paypal"})
(s/def ::event/error-code (s/nilable string?))
(s/def ::event/error-message (s/nilable string?))
(s/def ::event/failure-count nat-int?)
(s/def ::iso-instant
  (s/with-gen (s/and string? iso-instant?)
    #(gen/fmap (fn [secs] (str (java.time.Instant/ofEpochSecond secs)))
               (gen/large-integer* {:min 1600000000 :max 1900000000}))))
(s/def ::event/completed-at ::iso-instant)
(s/def ::event/failed-at ::iso-instant)
(s/def ::event/opened-at ::iso-instant)

(s/def ::payment-completed-event
  (s/keys :req-un [::event/payment-id ::event/order-id ::event/charge-id
                   ::event/amount-cents ::event/processor ::event/completed-at]))
(s/def ::payment-failed-event
  (s/keys :req-un [::event/payment-id ::event/order-id ::event/error-code
                   ::event/error-message ::event/processor ::event/failed-at]))
(s/def ::circuit-opened-event
  (s/keys :req-un [::event/processor ::event/failure-count ::event/opened-at]))

(s/def ::event/aggregate-id (s/and string? seq))
(s/def ::event/event-type
  #{"payment.initiated" "payment.completed" "payment.failed" "payment.refunded"
    "payment.partially_refunded" "payment.disputed" "payment.dispute_resolved"
    "circuit.opened" "circuit.closed" "circuit.half_open"})
(s/def ::event/payload map?)
(s/def ::event/traceparent ::ts/traceparent)
(s/def ::event/tracestate (s/nilable ::ts/tracestate))
(s/def ::outbox-event
  (s/keys :req-un [::event/aggregate-id ::event/event-type ::event/payload ::event/traceparent]
          :opt-un [::event/tracestate]))
