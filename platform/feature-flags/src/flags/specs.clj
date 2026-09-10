(ns flags.specs
  "Data specs for the Feature Flags Service (https://clojure.org/guides/spec).

  The traceparent header and trace ids come from trace-spine.specs (lib/clj),
  the monorepo's single definition of the W3C wire format. Function specs
  (s/fdef) live next to each defn in flags.core."
  (:require [clojure.spec.alpha :as s]
            [clojure.spec.gen.alpha :as gen]
            [flags.clause :as-alias clause]
            [flags.clause.instant :as-alias instant]
            [flags.clause.member :as-alias member]
            [flags.clause.number :as-alias number]
            [flags.clause.regex :as-alias regex]
            [flags.clause.string :as-alias string]
            [flags.context :as-alias context]
            [flags.evaluation :as-alias evaluation]
            [flags.flag :as-alias flag]
            [flags.rollout :as-alias rollout]
            [flags.rollout-result :as-alias rollout-result]
            [flags.rule :as-alias rule]
            [flags.rule-result :as-alias rule-result]
            [flags.traceparent :as-alias tp]
            [flags.user :as-alias user]
            [flags.variation :as-alias variation]
            [trace-spine.specs :as ts]))

;; =============================================================================
;; Trace context
;; =============================================================================

;; What flags.core/parse-traceparent returns.
(s/def ::tp/trace-id ::ts/trace-id)
(s/def ::tp/parent-id ::ts/parent-id)
(s/def ::tp/flags ::ts/flags-byte)
(s/def ::tp/sampled? boolean?)
(s/def ::traceparent-fields
  (s/keys :req-un [::tp/trace-id ::tp/parent-id ::tp/flags ::tp/sampled?]))

;; =============================================================================
;; Values
;; =============================================================================

;; A flag value, rule variation or context attribute. Nonconforming, so :fn
;; relations see the raw value rather than a tagged pair.
(s/def ::scalar
  (s/nonconforming
   (s/or :string string? :number number? :boolean boolean? :keyword keyword?)))

(defn- gen-instant []
  (gen/fmap #(java.time.Instant/ofEpochSecond %)
            (gen/large-integer* {:min 1600000000 :max 1900000000})))

;; =============================================================================
;; Evaluation context (user/device/session attributes)
;; =============================================================================

(s/def ::user/id
  (s/with-gen (s/and string? seq)
    #(gen/fmap (fn [n] (str "usr_" n)) (gen/large-integer* {:min 1 :max 99999}))))
(s/def ::user/tier #{"alpha" "beta" "standard" "premium"})
(s/def ::user/country #{"US" "CA" "GB" "DE"})
(s/def ::user/age (s/int-in 13 100))
(s/def ::user/signup-at (s/with-gen inst? gen-instant))
(s/def ::context/user
  (s/keys :opt-un [::user/id ::user/tier ::user/country ::user/age ::user/signup-at]))

(s/def ::context/trace-id (s/with-gen string? #(s/gen ::ts/trace-id)))
(s/def ::context/traceparent ::ts/traceparent-candidate)
(s/def ::context/key (s/nilable string?))

(s/def ::context
  (s/keys :opt-un [::context/user ::context/trace-id ::context/traceparent ::context/key]))

;; =============================================================================
;; Targeting clauses: {:attribute "user.tier" :op :in :values [...]}
;; The values each operator can compare are spec'd per operator; generators
;; pick attributes of the matching type.
;; =============================================================================

(s/def ::attribute-path (s/and string? seq))

(defn- valid-regex? [s]
  (try (some? (re-pattern s)) (catch Exception _ false)))

(defn- iso-instant? [s]
  (try (some? (java.time.Instant/parse s)) (catch Exception _ false)))

(s/def ::op (s/nonconforming (s/or :keyword keyword? :string string?)))

(s/def ::clause/member-attribute
  (s/with-gen ::attribute-path #(gen/elements ["user.id" "user.tier" "user.country" "user.age"])))
(s/def ::clause/string-attribute
  (s/with-gen ::attribute-path #(gen/elements ["user.id" "user.tier" "user.country"])))
(s/def ::clause/number-attribute
  (s/with-gen ::attribute-path #(gen/return "user.age")))
(s/def ::clause/instant-attribute
  (s/with-gen ::attribute-path #(gen/return "user.signup-at")))

(s/def ::clause/member-values
  (s/with-gen (s/coll-of ::scalar :kind vector?)
    #(gen/vector (gen/one-of [(s/gen ::user/tier) (s/gen ::user/country) (s/gen ::user/age)]) 0 4)))
(s/def ::clause/string-values
  (s/with-gen (s/coll-of string? :kind vector?)
    #(gen/vector (gen/elements ["usr_" "1" "US" "be" "ta" ""]) 0 3)))
(s/def ::clause/number-values (s/coll-of number? :kind vector? :gen-max 3))
(s/def ::clause/regex-values
  (s/coll-of (s/with-gen (s/and string? valid-regex?)
               #(gen/elements ["^usr_" "beta|alpha" "^[A-Z]{2}$" "\\d+" ""]))
             :kind vector? :gen-max 3))
(s/def ::clause/instant-values
  (s/coll-of (s/with-gen (s/and string? iso-instant?) #(gen/fmap str (gen-instant)))
             :kind vector? :gen-max 3))

(defmacro ^:private clause-keys [attribute values]
  `(s/keys :req-un [~attribute ::op ~values]))

;; :req-un keys drop the namespace, so rename the per-operator specs to the
;; :attribute/:values keys the clause maps actually use.
(s/def ::member/attribute ::clause/member-attribute)
(s/def ::member/values ::clause/member-values)
(s/def ::string/attribute ::clause/string-attribute)
(s/def ::string/values ::clause/string-values)
(s/def ::number/attribute ::clause/number-attribute)
(s/def ::number/values ::clause/number-values)
(s/def ::regex/attribute ::clause/string-attribute)
(s/def ::regex/values ::clause/regex-values)
(s/def ::instant/attribute ::clause/instant-attribute)
(s/def ::instant/values ::clause/instant-values)

(defmulti clause-spec (fn [clause] (keyword (:op clause))))
(defmethod clause-spec :eq [_] (clause-keys ::member/attribute ::member/values))
(defmethod clause-spec :neq [_] (clause-keys ::member/attribute ::member/values))
(defmethod clause-spec :in [_] (clause-keys ::member/attribute ::member/values))
(defmethod clause-spec :not-in [_] (clause-keys ::member/attribute ::member/values))
(defmethod clause-spec :contains [_] (clause-keys ::string/attribute ::string/values))
(defmethod clause-spec :starts-with [_] (clause-keys ::string/attribute ::string/values))
(defmethod clause-spec :ends-with [_] (clause-keys ::string/attribute ::string/values))
(defmethod clause-spec :gt [_] (clause-keys ::number/attribute ::number/values))
(defmethod clause-spec :gte [_] (clause-keys ::number/attribute ::number/values))
(defmethod clause-spec :lt [_] (clause-keys ::number/attribute ::number/values))
(defmethod clause-spec :lte [_] (clause-keys ::number/attribute ::number/values))
(defmethod clause-spec :regex [_] (clause-keys ::regex/attribute ::regex/values))
(defmethod clause-spec :before [_] (clause-keys ::instant/attribute ::instant/values))
(defmethod clause-spec :after [_] (clause-keys ::instant/attribute ::instant/values))

(s/def ::clause (s/multi-spec clause-spec (fn [c tag] (assoc c :op tag))))

;; =============================================================================
;; Rules, rollouts, flags
;; =============================================================================

(s/def ::rule/id (s/and string? seq))
(s/def ::rule/description (s/nilable string?))
(s/def ::rule/clauses (s/coll-of ::clause :kind vector? :gen-max 3))
(s/def ::rule/match #{:all :any "all" "any"})
(s/def ::rule/variation ::scalar)
(s/def ::rule/weight (s/nilable nat-int?))
(s/def ::rule
  (s/keys :req-un [::rule/clauses ::rule/variation]
          :opt-un [::rule/id ::rule/description ::rule/match ::rule/weight]))
(s/def ::rule-opts ::rule)

(s/def ::rule-result/matched true?)
(s/def ::rule-result/rule-id (s/nilable string?))
(s/def ::rule-result/value ::scalar)
(s/def ::rule-result
  (s/keys :req-un [::rule-result/matched ::rule-result/rule-id ::rule-result/value]))

(s/def ::rollout/enabled boolean?)
(s/def ::rollout/percentage (s/int-in 0 101))
(s/def ::rollout/bucket-by (s/with-gen ::attribute-path #(gen/elements ["user.id" "user.tier"])))
(s/def ::rollout/seed (s/nilable string?))
(s/def ::variation/value ::scalar)
(s/def ::variation/weight (s/int-in 0 101))
(s/def ::rollout/variations
  (s/nilable (s/coll-of (s/keys :req-un [::variation/value ::variation/weight])
                        :kind vector? :gen-max 3)))
(s/def ::rollout
  (s/keys :req-un [::rollout/enabled ::rollout/percentage ::rollout/bucket-by]
          :opt-un [::rollout/seed ::rollout/variations]))
(s/def ::rollout-opts
  (s/keys :opt-un [::rollout/enabled ::rollout/percentage ::rollout/bucket-by
                   ::rollout/seed ::rollout/variations]))

(s/def ::rollout-result/value (s/nilable ::scalar))
(s/def ::rollout-result/reason #{:rollout})
(s/def ::rollout-result/bucket (s/int-in 0 100))
(s/def ::rollout-result
  (s/keys :req-un [::rollout-result/value ::rollout-result/reason ::rollout-result/bucket]))

(s/def ::flag/key
  (s/with-gen (s/and string? seq)
    #(gen/elements ["checkout-v2" "new-search" "dark-mode"])))
(s/def ::flag/name (s/nilable string?))
(s/def ::flag/description (s/nilable string?))
(s/def ::flag/kind #{:boolean :string :number :json})
(s/def ::flag/default (s/nilable ::scalar))
(s/def ::flag/enabled boolean?)
(s/def ::flag/rules (s/coll-of ::rule :kind vector? :gen-max 3))
(s/def ::flag/rollout (s/nilable ::rollout))
(s/def ::flag
  (s/keys :req-un [::flag/key ::flag/enabled]
          :opt-un [::flag/name ::flag/description ::flag/kind ::flag/default
                   ::flag/rules ::flag/rollout]))
(s/def ::flag-opts
  (s/keys :req-un [::flag/key]
          :opt-un [::flag/name ::flag/description ::flag/kind ::flag/default
                   ::flag/enabled ::flag/rules ::flag/rollout]))

;; =============================================================================
;; Evaluation results
;; =============================================================================

;; evaluate-flag keys its result's flag key as :flag/key.
(s/def :flag/key (s/nilable string?))
(s/def ::evaluation/value (s/nilable ::scalar))
(s/def ::evaluation/reason #{:not-found :disabled :rule-match :rollout :default})
(s/def ::evaluation/trace-id (s/nilable string?))
(s/def ::evaluation/evaluation-ms nat-int?)
(s/def ::evaluation/rule-id (s/nilable string?))
(s/def ::evaluation/cached boolean?)
(s/def ::evaluation
  (s/keys :req [:flag/key]
          :req-un [::evaluation/value ::evaluation/reason ::evaluation/trace-id
                   ::evaluation/evaluation-ms]
          :opt-un [::evaluation/rule-id ::evaluation/cached]))

(s/def ::evaluation/flag-key (s/nilable string?))
(s/def ::eval-opts (s/keys :opt-un [::evaluation/flag-key ::evaluation/cached]))

(s/def ::redis-uri (s/nilable string?))
(s/def ::init-opts (s/keys :opt-un [::redis-uri]))
