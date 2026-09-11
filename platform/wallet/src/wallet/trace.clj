(ns wallet.trace
  "Trace context utilities for W3C Trace Context propagation.

   This namespace provides trace context extraction, injection, and generation
   for the Wallet Service. All operations must propagate traceparent for
   payment-wallet coordination visibility."
  (:require
   [clojure.spec.alpha :as s]
   [clojure.string :as str]
   [trace-spine.specs :as ts]
   [wallet.specs :as specs])
  (:import
   [java.security SecureRandom]))

;; =============================================================================
;; Constants
;; =============================================================================

(def ^:const version "00")
(def ^:const sampled-flag "01")
(def ^:const not-sampled-flag "00")
(def ^:const invalid-trace-id (apply str (repeat 32 "0")))
(def ^:const invalid-span-id (apply str (repeat 16 "0")))

;; =============================================================================
;; Validation
;; =============================================================================

(def ^:private traceparent-pattern
  "Regex pattern for valid W3C traceparent header."
  #"^00-[0-9a-f]{32}-[0-9a-f]{16}-[0-9a-f]{2}$")

(defn valid-traceparent?
  "Returns true if the traceparent string is valid per W3C spec."
  [s]
  (and (string? s)
       (re-matches traceparent-pattern s)
       (not (str/includes? s invalid-trace-id))
       (let [[_ _ span-id _] (str/split s #"-")]
         (not= span-id invalid-span-id))))

(s/fdef valid-traceparent?
  :args (s/cat :s ::ts/traceparent-candidate)
  :ret (s/nilable boolean?)
  :fn (fn [{{tp :s} :args ret :ret}]
        (= (boolean ret) (s/valid? ::ts/traceparent tp))))

;; =============================================================================
;; Generation
;; =============================================================================

(def ^:private secure-random (SecureRandom.))

(defn- random-hex
  "Generates n random hex characters."
  [n]
  (let [bytes (byte-array (/ n 2))]
    (.nextBytes secure-random bytes)
    (apply str (map #(format "%02x" (bit-and % 0xff)) bytes))))

(defn generate-trace-id
  "Generates a new random 32-character hex trace ID."
  []
  (random-hex 32))

(s/fdef generate-trace-id
  :args (s/cat)
  :ret ::ts/trace-id)

(defn generate-span-id
  "Generates a new random 16-character hex span ID."
  []
  (random-hex 16))

(s/fdef generate-span-id
  :args (s/cat)
  :ret ::ts/span-id)

(defn generate-traceparent
  "Generates a new traceparent with sampled flag."
  ([]
   (generate-traceparent (generate-trace-id)))
  ([trace-id]
   (str version "-" trace-id "-" (generate-span-id) "-" sampled-flag)))

(s/fdef generate-traceparent
  :args (s/cat :trace-id (s/? ::ts/trace-id))
  :ret ::ts/traceparent
  :fn (fn [{{:keys [trace-id]} :args ret :ret}]
        (let [fields (ts/traceparent-fields ret)]
          (and (= sampled-flag (:flags fields))
               (or (nil? trace-id) (= trace-id (:trace-id fields)))))))

;; =============================================================================
;; Parsing
;; =============================================================================

(defn parse-traceparent
  "Parses a traceparent string into components.

   Returns:
     {:version \"00\"
      :trace-id \"...\"
      :span-id \"...\"
      :flags \"01\"
      :sampled? true}

   Returns nil if invalid."
  [s]
  (when (valid-traceparent? s)
    (let [[ver trace-id span-id flags] (str/split s #"-")]
      {:version ver
       :trace-id trace-id
       :span-id span-id
       :flags flags
       ;; L1-wire: bit 0 of the flags byte is `sampled`
       :sampled? (odd? (Integer/parseInt flags 16))})))

(s/fdef parse-traceparent
  :args (s/cat :s ::ts/traceparent-candidate)
  :ret (s/nilable ::specs/traceparent-fields)
  :fn (fn [{{tp :s} :args ret :ret}]
        (if (s/valid? ::ts/traceparent tp)
          (let [fields (ts/traceparent-fields tp)]
            (and (= (:trace-id fields) (:trace-id ret))
                 (= (:parent-id fields) (:span-id ret))
                 (= (:flags fields) (:flags ret))
                 ;; L1-wire: bit 0 of the flags byte is `sampled`
                 (= (:sampled? ret) (odd? (Long/parseLong (:flags fields) 16)))))
          (nil? ret))))

;; =============================================================================
;; Child Span Generation
;; =============================================================================

(defn child-traceparent
  "Creates a child span traceparent, preserving trace-id.

   Takes an existing traceparent and generates a new span-id
   while preserving the trace-id for correlation."
  [parent-traceparent]
  (if-let [parsed (parse-traceparent parent-traceparent)]
    (str version "-" (:trace-id parsed) "-" (generate-span-id) "-" (:flags parsed))
    (generate-traceparent)))

(s/fdef child-traceparent
  :args (s/cat :parent-traceparent ::ts/traceparent-candidate)
  :ret ::ts/traceparent
  :fn (fn [{{parent :parent-traceparent} :args ret :ret}]
        (let [child (ts/traceparent-fields ret)]
          (if (s/valid? ::ts/traceparent parent)
            (let [p (ts/traceparent-fields parent)]
              (and (= (:trace-id p) (:trace-id child))
                   (= (:flags p) (:flags child))
                   (not= (:parent-id p) (:parent-id child))))
            ;; no usable parent: a fresh, sampled trace
            (= sampled-flag (:flags child))))))

;; =============================================================================
;; HTTP Header Extraction/Injection
;; =============================================================================

(defn extract-trace-context
  "Extracts trace context from HTTP headers map.

   Headers are expected in lowercase (Ring convention).

   Returns:
     {:traceparent \"00-...\" :tracestate \"...\"}"
  [headers]
  (let [traceparent (or (get headers "traceparent")
                        (get headers :traceparent))
        tracestate (or (get headers "tracestate")
                       (get headers :tracestate))]
    (when (valid-traceparent? traceparent)
      {:traceparent traceparent
       :tracestate tracestate})))

(s/fdef extract-trace-context
  :args (s/cat :headers ::specs/headers)
  :ret (s/nilable ::specs/trace-context)
  :fn (fn [{{:keys [headers]} :args ret :ret}]
        (let [tp (or (get headers "traceparent") (get headers :traceparent))]
          (= (some? ret) (s/valid? ::ts/traceparent tp)))))

(defn inject-trace-context
  "Injects trace context into headers map for outgoing requests.

   Creates a child span and adds traceparent/tracestate headers."
  [headers trace-context]
  (if-let [parent (:traceparent trace-context)]
    (cond-> (assoc headers "traceparent" (child-traceparent parent))
      (:tracestate trace-context) (assoc "tracestate" (:tracestate trace-context)))
    (assoc headers
           "traceparent" (generate-traceparent))))

(s/fdef inject-trace-context
  :args (s/cat :headers ::ts/carrier :trace-context (s/nilable ::specs/trace-context))
  :ret ::ts/carrier
  :fn (fn [{{:keys [trace-context]} :args ret :ret}]
        (let [out (ts/traceparent-fields (get ret "traceparent"))]
          (if-let [parent (some-> trace-context :traceparent ts/traceparent-fields)]
            (and (= (:trace-id parent) (:trace-id out))
                 (not= (:parent-id parent) (:parent-id out)))
            (some? out)))))

;; =============================================================================
;; Ring Middleware
;; =============================================================================

(defn wrap-trace-context
  "Ring middleware that extracts trace context and binds it for the request.

   Adds :trace-context to the request map and generates a child span
   for this service's portion of the trace."
  [handler]
  (fn [request]
    (let [extracted (extract-trace-context (:headers request))
          trace-context (if extracted
                          (assoc extracted
                                 :traceparent (child-traceparent (:traceparent extracted)))
                          {:traceparent (generate-traceparent)
                           :tracestate nil})]
      (handler (assoc request :trace-context trace-context)))))

(s/fdef wrap-trace-context
  :args (s/cat :handler ::ts/handler)
  :ret fn?)

;; =============================================================================
;; Trace ID Extraction
;; =============================================================================

(defn extract-trace-id
  "Extracts just the trace-id from a traceparent string.

   Useful for logging and correlation queries."
  [traceparent]
  (when-let [parsed (parse-traceparent traceparent)]
    (:trace-id parsed)))

(s/fdef extract-trace-id
  :args (s/cat :traceparent ::ts/traceparent-candidate)
  :ret (s/nilable ::ts/trace-id)
  :fn (fn [{{tp :traceparent} :args ret :ret}]
        (= ret (when (s/valid? ::ts/traceparent tp)
                 (:trace-id (ts/traceparent-fields tp))))))

(comment
  ;; Examples

  ;; Generate new traceparent
  (generate-traceparent)
  ;; => "00-a1b2c3d4e5f67890a1b2c3d4e5f67890-1234567890abcdef-01"

  ;; Parse existing
  (parse-traceparent "00-4bf92f3577b34da6a3ce929d0e0e4736-00f067aa0ba902b7-01")
  ;; => {:version "00" :trace-id "..." :span-id "..." :flags "01" :sampled? true}

  ;; Create child span
  (child-traceparent "00-4bf92f3577b34da6a3ce929d0e0e4736-00f067aa0ba902b7-01")
  ;; => "00-4bf92f3577b34da6a3ce929d0e0e4736-{new-span-id}-01"

  ;; Validate
  (valid-traceparent? "00-4bf92f3577b34da6a3ce929d0e0e4736-00f067aa0ba902b7-01")
  ;; => true

  (valid-traceparent? "00-00000000000000000000000000000000-00f067aa0ba902b7-01")
  ;; => false (invalid trace-id)

  ;;
  )
