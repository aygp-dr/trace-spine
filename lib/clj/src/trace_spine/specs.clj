(ns trace-spine.specs
  "clojure.spec for the W3C Trace Context wire format (spec/L1-wire.org,
  ADR-0002) and the trace-spine data model (spec/L1-contracts.org).

  This is the one definition of a valid traceparent in the monorepo. The
  platform services require this namespace instead of re-deriving the rules.

    traceparent = version \"-\" trace-id \"-\" parent-id \"-\" flags
                  00        32 hex      16 hex      2 hex     (lowercase hex)

  trace-id and parent-id must not be all zeros (L1-wire, Forbidden values).
  Every spec here generates. `gen-malformed-traceparent` produces near-miss
  headers that must be rejected, for testing parsers on both sides of the
  boundary. Function specs (s/fdef) live next to each defn."
  (:require [clojure.spec.alpha :as s]
            [clojure.spec.gen.alpha :as gen]
            [clojure.string :as str]
            [trace-spine.context :as-alias ctx]
            [trace-spine.ring :as-alias ring]))

;; =============================================================================
;; Wire constants (L1-wire, ADR-0002)
;; =============================================================================

(def version
  "The only traceparent version trace-spine accepts (ADR-0002)."
  "00")

(def invalid-trace-id
  "Forbidden trace-id (L1-wire, Forbidden values)."
  "00000000000000000000000000000000")

(def invalid-parent-id
  "Forbidden parent-id, i.e. span-id (L1-wire, Forbidden values)."
  "0000000000000000")

(def traceparent-regex
  "The L1-wire validation regex, with a capture group per field. It checks
  shape only; ::traceparent also rejects the forbidden all-zero ids."
  #"^00-([0-9a-f]{32})-([0-9a-f]{16})-([0-9a-f]{2})$")

(def ^:private hex-digits "0123456789abcdef")
(def ^:private hex-digit? (set hex-digits))

(defn- lower-hex?
  "True iff s is a string of exactly n lowercase hex digits."
  [n s]
  (and (string? s) (= n (count s)) (every? hex-digit? s)))

(defn- gen-hex [n]
  (gen/fmap str/join (gen/vector (gen/elements hex-digits) n)))

(defn- gen-id
  "n lowercase hex digits, never the forbidden all-zero value. A fifth of the
  samples are mostly zeros: the edge right next to the forbidden value."
  [n]
  (gen/such-that (fn [id] (some #(not= \0 %) id))
                 (gen/frequency [[4 (gen-hex n)]
                                 [1 (gen/fmap #(format (str "%0" n "x") %)
                                              (gen/large-integer* {:min 1 :max 0xffff}))]])))

;; =============================================================================
;; traceparent fields
;; =============================================================================

(s/def ::version #{version})

(s/def ::trace-id
  (s/with-gen (s/and #(lower-hex? 32 %) #(not= invalid-trace-id %))
    #(gen-id 32)))

(s/def ::span-id
  (s/with-gen (s/and #(lower-hex? 16 %) #(not= invalid-parent-id %))
    #(gen-id 16)))

;; W3C calls the span-id carried on the wire the parent-id.
(s/def ::parent-id ::span-id)

;; trace-flags as the two hex digits on the wire. Bit 0 is `sampled`; the
;; other bits are reserved, but any lowercase hex byte is well-formed.
(s/def ::flags
  (s/with-gen #(lower-hex? 2 %)
    #(gen/frequency [[4 (gen/elements ["00" "01"])] [1 (gen-hex 2)]])))

;; The same byte as an integer, as wms/metrics/feature-flags carry it.
(s/def ::flags-byte (s/int-in 0 256))

;; =============================================================================
;; traceparent header
;; =============================================================================

(defn traceparent-fields
  "Splits s into {:version :trace-id :parent-id :flags} when it has the
  L1-wire shape, else nil. Shape only: the forbidden ids still split."
  [s]
  (when (string? s)
    (when-let [[_ trace-id parent-id flags] (re-matches traceparent-regex s)]
      {:version version :trace-id trace-id :parent-id parent-id :flags flags})))

(defn- well-formed-traceparent? [s]
  (let [{:keys [trace-id parent-id]} (traceparent-fields s)]
    (boolean (and trace-id
                  (not= invalid-trace-id trace-id)
                  (not= invalid-parent-id parent-id)))))

(defn gen-traceparent
  "Generator of well-formed traceparent headers."
  []
  (gen/fmap (fn [[t p f]] (str version "-" t "-" p "-" f))
            (gen/tuple (s/gen ::trace-id) (s/gen ::parent-id) (s/gen ::flags))))

;; version-traceid-parentid-flags: lowercase hex, fixed lengths, version 00,
;; neither id all zeros.
(s/def ::traceparent
  (s/with-gen well-formed-traceparent? gen-traceparent))

(s/def ::traceparent-fields
  (s/keys :req-un [::version ::trace-id ::parent-id ::flags]))

(defn gen-malformed-traceparent
  "Generator of near-miss traceparent headers, every one of which must be
  rejected: forbidden all-zero ids, uppercase hex, other versions, a
  character dropped or inserted, surrounding whitespace or extra fields, and
  plain junk."
  []
  (let [tp (gen-traceparent)]
    (gen/one-of
     [(gen/fmap (fn [[p f]] (str version "-" invalid-trace-id "-" p "-" f))
                (gen/tuple (s/gen ::parent-id) (s/gen ::flags)))
      (gen/fmap (fn [[t f]] (str version "-" t "-" invalid-parent-id "-" f))
                (gen/tuple (s/gen ::trace-id) (s/gen ::flags)))
      (gen/fmap str/upper-case (gen/such-that #(re-find #"[a-f]" %) tp))
      (gen/fmap (fn [[v s]] (str v (subs s 2)))
                (gen/tuple (gen/elements ["01" "02" "fe" "ff" "0" "000" "0x" "  "]) tp))
      (gen/fmap (fn [[s i]] (str (subs s 0 i) (subs s (inc i))))
                (gen/tuple tp (gen/choose 0 54)))
      (gen/fmap (fn [[s i c]] (str (subs s 0 i) c (subs s i)))
                (gen/tuple tp (gen/choose 0 55) (gen/elements (str hex-digits "-"))))
      (gen/fmap (fn [[pre s post]] (str pre s post))
                (gen/tuple (gen/elements ["" " " "\t"]) tp
                           (gen/elements [" " "\n" "-" "-00" ","])))
      (gen/such-that (complement well-formed-traceparent?) (gen/string-ascii))])))

;; What a parser is handed: a header that may be absent, well-formed or not.
(s/def ::traceparent-candidate
  (s/with-gen (s/nilable string?)
    #(gen/frequency [[5 (gen-traceparent)]
                     [5 (gen-malformed-traceparent)]
                     [1 (gen/return nil)]])))

;; =============================================================================
;; tracestate header (L1-wire: vendor=value or tenant@vendor=value list
;; members, comma-separated, at most 512 characters; W3C key/value grammar)
;; =============================================================================

(def ^:private tracestate-member-regex
  #"(?:[a-z][a-z0-9_\-*/]{0,255}|[a-z0-9][a-z0-9_\-*/]{0,240}@[a-z][a-z0-9_\-*/]{0,13})=[\x20-\x2b\x2d-\x3c\x3e-\x7e]{0,255}[\x21-\x2b\x2d-\x3c\x3e-\x7e]")

(defn- valid-tracestate? [s]
  (boolean
   (and (string? s)
        (<= (count s) 512)
        (let [members (->> (str/split s #",") (map str/trim) (remove str/blank?))]
          (and (<= 1 (count members) 32)
               (every? #(re-matches tracestate-member-regex %) members))))))

(defn- gen-tracestate-member []
  (gen/fmap (fn [[tenant vendor value]]
              (str (when tenant (str tenant "@")) vendor "=" value))
            (gen/tuple (gen/one-of [(gen/return nil) (gen/elements ["t61" "acme" "tenant1"])])
                       (gen/elements ["rojo" "congo" "dd" "ot" "vendor-x" "a_b"])
                       (gen/fmap str/join
                                 (gen/vector (gen/elements "abcXYZ0123456789:;/_-.") 1 16)))))

(s/def ::tracestate
  (s/with-gen valid-tracestate?
    #(gen/fmap (partial str/join ",") (gen/vector (gen-tracestate-member) 1 4))))

;; =============================================================================
;; TraceContext (spec/L1-contracts.org, core types)
;; =============================================================================

(s/def ::ctx/trace-id ::trace-id)
(s/def ::ctx/span-id ::span-id)
(s/def ::ctx/flags ::flags)
(s/def ::ctx/parent-span-id (s/nilable ::span-id))
(s/def ::ctx/tracestate (s/nilable ::tracestate))

;; The trace-spine.core/TraceContext record (plain maps with the same keys
;; conform too).
(s/def ::trace-context
  (s/keys :req-un [::ctx/trace-id ::ctx/span-id ::ctx/flags]
          :opt-un [::ctx/parent-span-id ::ctx/tracestate]))

;; What format-traceparent needs: ids, and flags unless the default applies.
(s/def ::wire-context
  (s/keys :req-un [::ctx/trace-id ::ctx/span-id]
          :opt-un [::ctx/flags]))

(s/def ::sampled? boolean?)
(s/def ::trace-opts (s/keys :opt-un [::sampled?]))

;; Carrier = Map<String, String> (HTTP headers, Kafka headers, ...). The
;; generator adds a traceparent candidate and sometimes a tracestate.
(s/def ::carrier
  (s/with-gen (s/map-of string? string?)
    #(gen/fmap (fn [[other tp ts]]
                 (cond-> other
                   tp (assoc "traceparent" tp)
                   ts (assoc "tracestate" ts)))
               (gen/tuple (gen/map (gen/elements ["content-type" "accept" "x-request-id"])
                                   (gen/string-alphanumeric))
                          (s/gen ::traceparent-candidate)
                          (gen/one-of [(gen/return nil) (s/gen ::tracestate)])))))

;; =============================================================================
;; Ring (trace-spine.middleware)
;; =============================================================================

(s/def ::ring/headers ::carrier)
(s/def ::ring/status (s/int-in 100 600))
(s/def ::ring/body any?)
(s/def ::ring/uri
  (s/with-gen (s/and string? #(str/starts-with? % "/"))
    #(gen/elements ["/" "/api/checkout" "/health" "/v1/score"])))
(s/def ::ring/request-method #{:get :post :put :patch :delete :head :options})

(s/def ::ring-request
  (s/keys :req-un [::ring/headers] :opt-un [::ring/uri ::ring/request-method]))
(s/def ::ring-response
  (s/keys :req-un [::ring/status] :opt-un [::ring/headers ::ring/body]))

(s/def ::handler
  (s/with-gen ifn?
    #(gen/elements [(fn [_] {:status 200 :headers {} :body "ok"})
                    (fn [_] {:status 204 :headers {"x-served-by" "test"}})])))

(s/def ::async-handler
  (s/with-gen ifn?
    #(gen/return (fn [_request respond _raise] (respond {:status 200 :headers {}})))))

(s/def ::client-fn
  (s/with-gen ifn?
    #(gen/return (fn [url opts] {:status 200 :url url :opts opts}))))

(s/def ::callback
  (s/with-gen ifn? #(gen/return (fn [& _] nil))))

(s/def ::is-ingress? boolean?)
(s/def ::strict-mode? boolean?)
(s/def ::on-start ::callback)
(s/def ::on-end ::callback)
(s/def ::middleware-opts
  (s/keys :opt-un [::is-ingress? ::strict-mode? ::on-start ::on-end]))
