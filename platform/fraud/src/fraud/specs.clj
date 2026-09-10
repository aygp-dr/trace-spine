(ns fraud.specs
  "Data specs for the Fraud Service (https://clojure.org/guides/spec).

  The traceparent header and trace ids come from trace-spine.specs (lib/clj),
  the monorepo's single definition of the W3C wire format. Function specs
  (s/fdef) live next to each defn in fraud.core."
  (:require [clojure.spec.alpha :as s]
            [clojure.spec.gen.alpha :as gen]
            [fraud.address :as-alias addr]
            [fraud.metadata :as-alias md]
            [fraud.payment-method :as-alias pm]
            [fraud.response :as-alias resp]
            [fraud.signal :as-alias sig]
            [fraud.transaction :as-alias tx]
            [trace-spine.specs :as ts]))

;; =============================================================================
;; Transactions (README: POST /v1/score)
;; =============================================================================

(defn- gen-id [prefix]
  (gen/fmap #(str prefix %) (gen/large-integer* {:min 1 :max 9999999})))

(s/def ::tx/transaction_id (s/with-gen (s/and string? seq) #(gen-id "txn_")))
(s/def ::tx/user_id (s/with-gen (s/and string? seq) #(gen-id "user_")))
(s/def ::tx/amount_cents nat-int?)
(s/def ::tx/currency #{"USD" "EUR" "GBP" "CAD"})
(s/def ::tx/ip_address
  (s/with-gen string? #(gen/elements ["203.0.113.42" "198.51.100.7" "2001:db8::1"])))
(s/def ::tx/email
  (s/with-gen string? #(gen/elements ["test@example.com" "Alice@Example.org" "bob@example.net"])))
(s/def ::tx/device_fingerprint (s/with-gen string? #(gen-id "fp_")))

(s/def ::addr/country #{"US" "CA" "GB" "DE"})
(s/def ::addr/postal_code string?)
(s/def ::tx/billing_address (s/keys :opt-un [::addr/country ::addr/postal_code]))

(s/def ::pm/type #{"card" "wallet"})
(s/def ::pm/bin (s/with-gen (s/and string? #(re-matches #"\d{6}" %)) #(gen/elements ["424242" "555555" "378282"])))
(s/def ::pm/last_four (s/with-gen (s/and string? #(re-matches #"\d{4}" %)) #(gen/elements ["4242" "4444" "0005"])))
(s/def ::tx/payment_method (s/keys :opt-un [::pm/type ::pm/bin ::pm/last_four]))

(s/def ::md/channel #{"web" "ios" "android"})
(s/def ::md/session_duration_ms nat-int?)
(s/def ::tx/metadata (s/keys :opt-un [::md/channel ::md/session_duration_ms]))

(s/def ::transaction
  (s/keys :req-un [::tx/transaction_id ::tx/user_id ::tx/amount_cents]
          :opt-un [::tx/currency ::tx/ip_address ::tx/email ::tx/device_fingerprint
                   ::tx/billing_address ::tx/payment_method ::tx/metadata]))

;; =============================================================================
;; Rule conditions (fraud_rules.condition: JSON, decoded with string keys)
;;   {"gt" ["amount_cents" 10000]}   {"in" ["currency" ["EUR" "GBP"]]}
;;   {"and" [c1 c2]}   {"or" [c1 c2]}   {"not" c}
;; =============================================================================

(s/def ::scalar (s/nonconforming (s/or :string string? :number number?)))

(s/def ::field
  (s/with-gen string? #(gen/elements ["amount_cents" "currency" "email" "ip_address" "user_id"])))
(s/def ::numeric-field (s/with-gen string? #(gen/return "amount_cents")))
(s/def ::regex
  (s/with-gen (s/and string? #(try (some? (re-pattern %)) (catch Exception _ false)))
    #(gen/elements [".*@example\\.com" "^203\\." "user_[0-9]+" "EUR|GBP"])))

(defmulti condition-op (fn [c] (when (and (map? c) (= 1 (count c))) (key (first c)))))
(defmethod condition-op "eq" [_] (s/map-of #{"eq"} (s/tuple ::field ::scalar) :count 1))
(defmethod condition-op "gt" [_] (s/map-of #{"gt"} (s/tuple ::numeric-field number?) :count 1))
(defmethod condition-op "lt" [_] (s/map-of #{"lt"} (s/tuple ::numeric-field number?) :count 1))
(defmethod condition-op "in" [_]
  (s/map-of #{"in"} (s/tuple ::field (s/coll-of ::scalar :kind vector? :gen-max 3)) :count 1))
(defmethod condition-op "match" [_] (s/map-of #{"match"} (s/tuple ::field ::regex) :count 1))
(defmethod condition-op "and" [_]
  (s/map-of #{"and"} (s/coll-of ::condition :kind vector? :gen-max 3) :count 1))
(defmethod condition-op "or" [_]
  (s/map-of #{"or"} (s/coll-of ::condition :kind vector? :gen-max 3) :count 1))
(defmethod condition-op "not" [_] (s/map-of #{"not"} ::condition :count 1))

(s/def ::condition (s/multi-spec condition-op (fn [c _tag] c)))

(defn condition-holds?
  "Reference semantics of a rule condition: the oracle for
  fraud.core/evaluate-condition. A condition is a one-entry map from an
  operator to its arguments."
  [tx condition]
  (let [[op args] (first condition)
        value (fn [field] (get tx (keyword field)))]
    (case op
      "eq" (= (value (first args)) (second args))
      "gt" (> (or (value (first args)) 0) (second args))
      "lt" (< (or (value (first args)) 0) (second args))
      "in" (contains? (set (second args)) (value (first args)))
      "match" (boolean (re-matches (re-pattern (second args)) (str (value (first args)))))
      "and" (every? #(condition-holds? tx %) args)
      "or" (boolean (some #(condition-holds? tx %) args))
      "not" (not (condition-holds? tx args)))))

;; =============================================================================
;; Signals and the score response (README: Score Response)
;; =============================================================================

(s/def ::score (s/and number? #(<= 0 % 1)))

(s/def ::sig/triggered-rules (s/coll-of string? :kind vector?))
(s/def ::rules-signal (s/keys :req-un [::score ::sig/triggered-rules]))

(s/def ::sig/checks map?)
(s/def ::velocity-signal (s/keys :req-un [::score ::sig/checks]))

(s/def ::sig/matches (s/coll-of #{:ip :email :device :bin} :kind vector?))
(s/def ::blocklist-signal (s/keys :req-un [::score ::sig/matches]))

(s/def ::sig/model-version string?)
(s/def ::sig/features-computed boolean?)
(s/def ::ml-signal (s/keys :req-un [::score ::sig/model-version ::sig/features-computed]))

(s/def ::resp/transaction_id ::tx/transaction_id)
(s/def ::resp/trace_id (s/nilable ::ts/trace-id))
(s/def ::resp/decision #{"allow" "review" "block"})
(s/def ::resp/signals map?)
(s/def ::resp/latency_ms nat-int?)
(s/def ::resp/evaluated_at string?)
(s/def ::score-response
  (s/keys :req-un [::resp/transaction_id ::resp/trace_id ::score ::resp/decision
                   ::resp/signals ::resp/latency_ms ::resp/evaluated_at]))

;; =============================================================================
;; Configuration
;; =============================================================================

(s/def ::allow-threshold ::score)
(s/def ::block-threshold ::score)
(s/def ::config (s/keys :opt-un [::allow-threshold ::block-threshold]))
(s/def ::port (s/int-in 1 65536))
(s/def ::redis-url (s/nilable string?))
(s/def ::postgres-url (s/nilable string?))
(s/def ::server-opts (s/keys :opt-un [::port ::redis-url ::postgres-url]))
