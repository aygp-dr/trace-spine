(ns trace-spine.core
  "trace-spine core library for W3C Trace Context propagation.

   This library implements the contracts defined in spec/L1-contracts.org:
   - extract: Extract TraceContext from carrier
   - inject: Inject TraceContext into carrier
   - continue-or-start: Continue existing trace or start new one
   - create-child: Create child span from parent context
   - format/parse: Serialize/deserialize traceparent

   Wire format per spec/L1-wire.org:
   version \"-\" trace-id \"-\" parent-id \"-\" flags
   00-{32 hex}-{16 hex}-{2 hex}

   Example: 00-4bf92f3577b34da6a3ce929d0e0e4736-00f067aa0ba902b7-01"
  (:require [clojure.spec.alpha :as s]
            [clojure.spec.gen.alpha :as gen]
            [clojure.string :as str]
            [clojure.tools.logging :as log]
            [trace-spine.specs :as specs])
  (:import [java.security SecureRandom]))

;; =============================================================================
;; Constants
;; =============================================================================

(def ^:const version "00")
(def ^:const traceparent-regex #"^00-[0-9a-f]{32}-[0-9a-f]{16}-[0-9a-f]{2}$")
(def ^:const invalid-trace-id "00000000000000000000000000000000")
(def ^:const invalid-span-id "0000000000000000")
(def ^:const sampled-flag "01")
(def ^:const not-sampled-flag "00")

;; =============================================================================
;; Random Generation
;; =============================================================================

(def ^:private rng (SecureRandom.))

(defn- random-hex
  "Generate a random hex string of specified byte length"
  [num-bytes]
  (let [bytes (byte-array num-bytes)]
    (.nextBytes rng bytes)
    (apply str (map #(format "%02x" (bit-and % 0xff)) bytes))))

(defn generate-trace-id
  "Generate a new random trace ID (32 hex chars)"
  []
  (loop []
    (let [id (random-hex 16)]
      (if (= id invalid-trace-id)
        (recur)
        id))))

(s/fdef generate-trace-id
  :args (s/cat)
  :ret ::specs/trace-id)

(defn generate-span-id
  "Generate a new random span ID (16 hex chars)"
  []
  (loop []
    (let [id (random-hex 8)]
      (if (= id invalid-span-id)
        (recur)
        id))))

(s/fdef generate-span-id
  :args (s/cat)
  :ret ::specs/span-id)

;; =============================================================================
;; Trace Context Record
;; =============================================================================

(defrecord TraceContext [trace-id span-id flags parent-span-id tracestate])

(defn trace-context?
  "Check if value is a valid TraceContext"
  [x]
  (instance? TraceContext x))

(s/fdef trace-context?
  :args (s/cat :x any?)
  :ret boolean?)

;; =============================================================================
;; Parsing and Formatting
;; =============================================================================

(defn valid-traceparent?
  "Check if traceparent string matches W3C format"
  [s]
  (and (string? s)
       (re-matches traceparent-regex s)))

(s/fdef valid-traceparent?
  :args (s/cat :traceparent ::specs/traceparent-candidate)
  :ret boolean?
  :fn (fn [{{tp :traceparent} :args ret :ret}]
        (= ret (s/valid? ::specs/traceparent tp))))

(defn parse-traceparent
  "Parse a traceparent string into TraceContext.

   Returns nil if string is malformed (never throws).
   Logs warning on malformed input per L1 contracts."
  [s]
  (when (valid-traceparent? s)
    (let [[_ trace-id span-id flags] (re-matches #"^(\d{2})-([0-9a-f]{32})-([0-9a-f]{16})-([0-9a-f]{2})$" s)]
      (when (and trace-id
                 (not= trace-id invalid-trace-id)
                 (not= span-id invalid-span-id))
        (->TraceContext trace-id span-id flags nil nil)))))

(s/fdef parse-traceparent
  :args (s/cat :traceparent ::specs/traceparent-candidate)
  :ret (s/nilable ::specs/trace-context)
  ;; returns None iff the header is not well-formed; otherwise its fields
  :fn (fn [{{tp :traceparent} :args ret :ret}]
        (if (s/valid? ::specs/traceparent tp)
          (= tp (str version "-" (:trace-id ret) "-" (:span-id ret) "-" (:flags ret)))
          (nil? ret))))

(defn format-traceparent
  "Format a TraceContext as traceparent string.

   Per L1 contracts: result matches regex ^00-[0-9a-f]{32}-[0-9a-f]{16}-[0-9a-f]{2}$"
  [{:keys [trace-id span-id flags]}]
  (str version "-" trace-id "-" span-id "-" (or flags sampled-flag)))

(s/fdef format-traceparent
  :args (s/cat :ctx ::specs/wire-context)
  :ret ::specs/traceparent
  ;; parse(format(ctx)) == Some(ctx)
  :fn (fn [{{:keys [ctx]} :args ret :ret}]
        (= (select-keys (parse-traceparent ret) [:trace-id :span-id :flags])
           {:trace-id (:trace-id ctx)
            :span-id (:span-id ctx)
            :flags (or (:flags ctx) sampled-flag)})))

;; =============================================================================
;; Core Operations (per L1 contracts)
;; =============================================================================

(defn extract
  "Extract TraceContext from carrier (headers map).

   Per L1 contracts:
   - returns nil iff carrier has no valid traceparent
   - returns TraceContext when valid
   - never throws on malformed input; logs warning and returns nil
   - pure function: no side effects on carrier"
  [carrier]
  (when-let [traceparent (get carrier "traceparent")]
    (if-let [ctx (parse-traceparent traceparent)]
      (assoc ctx :tracestate (get carrier "tracestate"))
      (do
        (log/warn "Malformed traceparent header" {:traceparent traceparent})
        nil))))

(s/fdef extract
  :args (s/cat :carrier ::specs/carrier)
  :ret (s/nilable ::specs/trace-context)
  :fn (fn [{{:keys [carrier]} :args ret :ret}]
        (let [tp (get carrier "traceparent")]
          (if (s/valid? ::specs/traceparent tp)
            (and (= tp (format-traceparent ret))
                 (= (get carrier "tracestate") (:tracestate ret)))
            (nil? ret)))))

(defn inject
  "Inject TraceContext into carrier (headers map).

   Per L1 contracts:
   - carrier[\"traceparent\"] is set to formatted value
   - idempotent: inject(ctx, inject(ctx, carrier)) == inject(ctx, carrier)
   - extract(carrier) after inject yields equivalent context"
  [ctx carrier]
  (let [carrier' (assoc carrier "traceparent" (format-traceparent ctx))]
    (if-let [ts (:tracestate ctx)]
      (assoc carrier' "tracestate" ts)
      carrier')))

(s/fdef inject
  :args (s/cat :ctx ::specs/trace-context :carrier ::specs/carrier)
  :ret ::specs/carrier
  :fn (fn [{{:keys [ctx carrier]} :args ret :ret}]
        (and (= (get ret "traceparent") (format-traceparent ctx))
             ;; idempotent
             (= ret (inject ctx ret))
             ;; extract after inject yields the context's ids
             (= (select-keys (extract ret) [:trace-id :span-id :flags])
                (select-keys ctx [:trace-id :span-id :flags]))
             ;; every other header is left alone
             (= (dissoc ret "traceparent" "tracestate")
                (dissoc carrier "traceparent" "tracestate")))))

(defn create-child
  "Create a child TraceContext from parent.

   Per L1 contracts:
   - child.trace-id == parent.trace-id
   - child.span-id is fresh
   - child.flags == parent.flags (sampling decision preserved)"
  [parent]
  (->TraceContext
   (:trace-id parent)
   (generate-span-id)
   (:flags parent)
   (:span-id parent)  ; parent becomes parent-span-id
   (:tracestate parent)))

(s/fdef create-child
  :args (s/cat :parent ::specs/trace-context)
  :ret ::specs/trace-context
  :fn (fn [{{:keys [parent]} :args child :ret}]
        (and (= (:trace-id child) (:trace-id parent))
             (= (:flags child) (:flags parent))
             (= (:parent-span-id child) (:span-id parent))
             (= (:tracestate child) (:tracestate parent))
             (not= (:span-id child) (:span-id parent)))))

(defn start-trace
  "Start a new trace with fresh trace-id and span-id.

   Default flags: sampled (01)"
  ([]
   (start-trace {:sampled? true}))
  ([{:keys [sampled?] :or {sampled? true}}]
   (->TraceContext
    (generate-trace-id)
    (generate-span-id)
    (if sampled? sampled-flag not-sampled-flag)
    nil
    nil)))

(s/fdef start-trace
  :args (s/cat :opts (s/? ::specs/trace-opts))
  :ret ::specs/trace-context
  :fn (fn [{{:keys [opts]} :args ret :ret}]
        (and (= (:flags ret) (if (false? (:sampled? opts)) not-sampled-flag sampled-flag))
             (nil? (:parent-span-id ret))
             (nil? (:tracestate ret)))))

(defn continue-trace
  "Continue an existing trace from traceparent string.

   Creates a child span preserving trace-id.
   Returns nil if traceparent is invalid."
  ([traceparent]
   (continue-trace traceparent nil))
  ([traceparent tracestate]
   (when-let [parent (parse-traceparent traceparent)]
     (-> (create-child parent)
         (assoc :tracestate tracestate)))))

(s/fdef continue-trace
  :args (s/cat :traceparent ::specs/traceparent-candidate
               :tracestate (s/? (s/nilable ::specs/tracestate)))
  :ret (s/nilable ::specs/trace-context)
  :fn (fn [{{tp :traceparent ts :tracestate} :args ret :ret}]
        (if (s/valid? ::specs/traceparent tp)
          (let [{:keys [trace-id parent-id flags]} (specs/traceparent-fields tp)]
            (and (= trace-id (:trace-id ret))
                 (= parent-id (:parent-span-id ret))
                 (= flags (:flags ret))
                 (= ts (:tracestate ret))))
          (nil? ret))))

(defn continue-or-start
  "Continue existing trace or start new one.

   Per L1 contracts:
   - if extract(carrier) returns context: creates child span
   - if extract(carrier) returns nil AND is-ingress?: starts new trace
   - if extract(carrier) returns nil AND NOT is-ingress?: raises error

   Args:
     carrier    - headers map
     is-ingress - true if this is an ingress boundary (e.g., API gateway)"
  [carrier is-ingress?]
  (if-let [parent (extract carrier)]
    (create-child parent)
    (if is-ingress?
      (start-trace)
      (throw (ex-info "Internal service must not originate trace (missing traceparent)"
                      {:type :programmer-error
                       :carrier carrier})))))

;; Throws for an internal call without a traceparent, so it is not checked by
;; stest/check; see properties-test/no-fabrication.
(s/fdef continue-or-start
  :args (s/cat :carrier ::specs/carrier :is-ingress? boolean?)
  :ret ::specs/trace-context)

;; =============================================================================
;; Sampling
;; =============================================================================

(defn sampled?
  "Check if trace is sampled"
  [{:keys [flags]}]
  (= flags sampled-flag))

(s/fdef sampled?
  :args (s/cat :ctx ::specs/trace-context)
  :ret boolean?
  ;; L1-wire: bit 0 of the flags byte is `sampled`
  :fn (fn [{{:keys [ctx]} :args ret :ret}]
        (= ret (odd? (Long/parseLong (:flags ctx) 16)))))

(defn set-sampled
  "Set sampling flag on context"
  [ctx sampled?]
  (assoc ctx :flags (if sampled? sampled-flag not-sampled-flag)))

(s/fdef set-sampled
  :args (s/cat :ctx ::specs/trace-context :sampled? boolean?)
  :ret ::specs/trace-context
  :fn (fn [{{ctx :ctx on? :sampled?} :args ret :ret}]
        (and (= on? (sampled? ret))
             (= (dissoc ret :flags) (dissoc ctx :flags)))))

;; =============================================================================
;; Convenience
;; =============================================================================

(def ^:dynamic *trace-context*
  "Dynamic var holding current trace context"
  nil)

(defn with-trace
  "Execute function with trace context in dynamic scope.

   Binds *trace-context* for use by downstream code."
  [ctx f]
  (binding [*trace-context* ctx]
    (f)))

(s/fdef with-trace
  :args (s/cat :ctx ::specs/trace-context
               :f (s/with-gen ifn? #(gen/return (fn [] *trace-context*))))
  :ret any?
  :fn (fn [{{:keys [ctx]} :args ret :ret}]
        (= ctx ret)))

(defn current-context
  "Get current trace context from dynamic scope"
  []
  *trace-context*)

(s/fdef current-context
  :args (s/cat)
  :ret (s/nilable ::specs/trace-context))
