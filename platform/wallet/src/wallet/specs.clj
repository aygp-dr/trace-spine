(ns wallet.specs
  "Data specs for the Wallet Service (https://clojure.org/guides/spec).

  Every traceparent, tracestate and id shape is reused from trace-spine.specs
  (lib/clj), the monorepo's single definition of the W3C wire format. Function
  specs (s/fdef) live next to each defn in wallet.trace and wallet.core."
  (:require [clojure.spec.alpha :as s]
            [clojure.spec.gen.alpha :as gen]
            [trace-spine.specs :as ts]
            [wallet.trace-context :as-alias tc]
            [wallet.traceparent :as-alias tp]))

;; =============================================================================
;; wallet.trace
;; =============================================================================

;; What wallet.trace/parse-traceparent returns.
(s/def ::tp/version ::ts/version)
(s/def ::tp/trace-id ::ts/trace-id)
(s/def ::tp/span-id ::ts/span-id)
(s/def ::tp/flags ::ts/flags)
(s/def ::tp/sampled? boolean?)
(s/def ::traceparent-fields
  (s/keys :req-un [::tp/version ::tp/trace-id ::tp/span-id ::tp/flags ::tp/sampled?]))

;; {:traceparent :tracestate}, as bound in wallet.core/*trace-context* and
;; attached to requests by wrap-trace-context.
(s/def ::tc/traceparent ::ts/traceparent)
(s/def ::tc/tracestate (s/nilable ::ts/tracestate))
(s/def ::trace-context
  (s/keys :req-un [::tc/traceparent] :opt-un [::tc/tracestate]))

;; Incoming headers: Ring's lowercase strings, or keywords.
(s/def ::headers
  (s/with-gen (s/nilable (s/map-of (s/or :name string? :key keyword?) string?))
    #(gen/one-of [(s/gen ::ts/carrier)
                  (gen/fmap (fn [m] (update-keys m keyword)) (s/gen ::ts/carrier))
                  (gen/return nil)])))

;; =============================================================================
;; wallet.core
;; =============================================================================

(s/def ::datasource
  (s/with-gen some?
    #(gen/return {:jdbcUrl "jdbc:postgresql://localhost:5432/wallet"})))

(s/def ::customer-id
  (s/with-gen (s/or :uuid uuid? :id (s/and string? seq))
    #(gen/uuid)))

(s/def ::order-id
  (s/with-gen (s/and string? seq)
    #(gen/fmap (fn [n] (str "order-" n)) (gen/large-integer* {:min 1 :max 999999}))))

(s/def ::idempotency-key (s/and string? seq))

(defn- gen-money []
  (gen/fmap #(bigdec (/ % 100)) (gen/large-integer* {:min 1 :max 10000000})))

(s/def ::requested-amount (s/with-gen (s/and number? pos?) gen-money))
(s/def ::order-total (s/with-gen (s/and number? (complement neg?)) gen-money))
(s/def ::currency #{"USD" "EUR" "GBP" "CAD"})
(s/def ::instrument #{:store-credit :gift-card :loyalty-points})
(s/def ::instrument-priority (s/coll-of ::instrument :kind vector? :distinct true))
(s/def ::max-loyalty-percentage
  (s/with-gen (s/and number? #(<= 0 % 1))
    #(gen/fmap (fn [n] (bigdec (/ n 100))) (gen/choose 0 100))))
(s/def ::multiplier
  (s/with-gen (s/and number? pos?)
    #(gen/elements [1.0 1.5 2.0 3])))

(s/def ::apply-credits-opts
  (s/keys :req-un [::order-id ::requested-amount ::idempotency-key]
          :opt-un [::currency ::instrument-priority ::max-loyalty-percentage]))

(s/def ::original-transaction-ids (s/coll-of ::customer-id :kind sequential?))
(s/def ::refund-as ::instrument)
(s/def ::refund-credits-opts
  (s/keys :req-un [::order-id ::idempotency-key]
          :opt-un [::original-transaction-ids ::refund-as]))

(s/def ::earn-loyalty-opts
  (s/keys :req-un [::order-id ::order-total] :opt-un [::multiplier]))

(s/def ::gift-card-code (s/and string? seq))

;; --- results (as documented in wallet.core) ---

(s/def ::valid boolean?)
(s/def ::reason #{"not_found" "inactive" "expired" "already_redeemed"})
(s/def ::balance decimal?)
(s/def ::gift-card-validation
  (s/or :invalid (s/keys :req-un [::valid ::reason])
        :valid (s/keys :req-un [::valid ::balance])))

(s/def ::total number?)
(s/def ::breakdown (s/coll-of map? :kind vector?))
(s/def ::applied (s/keys :req-un [::total ::breakdown]))
(s/def ::remaining-balance map?)
(s/def ::apply-credits-result
  (s/keys :req-un [::applied ::remaining-balance ::idempotency-key]))
