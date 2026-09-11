(ns metrics.specs
  "Data specs for the Metrics Platform (https://clojure.org/guides/spec).

  Trace ids and the traceparent header come from trace-spine.specs (lib/clj),
  the monorepo's single definition of the W3C wire format; metrics carries the
  flags byte as an integer. Function specs (s/fdef) live next to each defn in
  metrics.core."
  (:require [clojure.core.async]
            [clojure.spec.alpha :as s]
            [clojure.spec.gen.alpha :as gen]
            [clojure.string :as str]
            [metrics.context :as-alias ctx]
            [metrics.event :as-alias ev]
            [trace-spine.specs :as ts])
  (:import [clojure.core.async.impl.channels ManyToManyChannel]))

;; =============================================================================
;; Trace context ({:trace-id :span-id :flags <int>})
;; =============================================================================

(s/def ::ctx/trace-id ::ts/trace-id)
(s/def ::ctx/span-id ::ts/span-id)
(s/def ::ctx/flags ::ts/flags-byte)

(s/def ::trace-context
  (s/keys :req-un [::ctx/trace-id ::ctx/span-id ::ctx/flags]))

;; What format-traceparent accepts: flags default to 1 (sampled).
(s/def ::wire-context
  (s/keys :req-un [::ctx/trace-id ::ctx/span-id] :opt-un [::ctx/flags]))

;; =============================================================================
;; Events (validate-event's schema)
;; =============================================================================

(def event-type-names #{"impression" "click" "add_to_cart" "purchase" "experiment" "error"})

(s/def ::ev/type
  (s/or :name event-type-names
        :key (set (map keyword event-type-names))))

(defn- gen-id [prefix]
  (gen/fmap #(str prefix %) (gen/not-empty (gen/string-alphanumeric))))

(s/def ::ev/event_id (s/with-gen (s/and string? (complement str/blank?)) #(gen-id "evt_")))
(s/def ::ev/session_id (s/with-gen (s/and string? (complement str/blank?)) #(gen-id "sess_")))
(s/def ::ev/timestamp
  (s/with-gen some?
    #(gen/elements ["2026-09-10T08:00:00Z" "2026-09-10T08:00:01.250Z"])))
(s/def ::ev/properties (s/map-of keyword? (s/or :s string? :n number?) :gen-max 3))

;; A valid event.
(s/def ::event
  (s/keys :req-un [::ev/type ::ev/event_id ::ev/session_id ::ev/timestamp]
          :opt-un [::ev/properties]))

;; Whatever a client sends: validate-event must classify it, not throw.
(s/def ::raw-event
  (s/with-gen map?
    #(gen/one-of [(s/gen ::event)
                  (gen/map (gen/elements [:type :event_id :session_id :timestamp])
                           (gen/one-of [(gen/string-alphanumeric) (gen/return "  ")
                                        (gen/large-integer) (gen/keyword) (gen/return nil)]))])))

(s/def ::valid? boolean?)
(s/def ::field #{:type :event_id :session_id :timestamp})
(s/def ::error string?)
(s/def ::errors (s/coll-of (s/keys :req-un [::field ::error]) :kind vector?))
(s/def ::validation (s/keys :req-un [::valid? ::errors]))

;; =============================================================================
;; Event collector
;; =============================================================================

(s/def ::sink some?)                    ; a metrics.core/EventSink
(s/def ::buffer-size pos-int?)
(s/def ::batch-size pos-int?)
(s/def ::collector-opts (s/keys :req-un [::sink] :opt-un [::buffer-size ::batch-size]))
(s/def ::event-chan #(instance? ManyToManyChannel %))
(s/def ::collector (s/keys :req-un [::sink ::event-chan ::batch-size]))

(s/def ::accepted nat-int?)
(s/def ::rejected nat-int?)
(s/def ::trace_id (s/nilable ::ts/trace-id))
(s/def ::collect-result (s/keys :req-un [::accepted ::rejected ::trace_id]))

;; =============================================================================
;; A/B experiments
;; =============================================================================

(s/def ::bucket (s/int-in 0 100))

;; An inclusive [lo hi] bucket range.
(s/def ::bucket-range
  (s/with-gen (s/and (s/tuple ::bucket ::bucket) (fn [[lo hi]] (<= lo hi)))
    #(gen/fmap (comp vec sort) (gen/tuple (s/gen ::bucket) (s/gen ::bucket)))))

(s/def ::allocation
  (s/with-gen (s/map-of keyword? ::bucket-range)
    #(gen/one-of [(gen/return {:control [0 49] :treatment [50 99]})
                  (gen/map (gen/elements [:control :treatment :variant-b])
                           (s/gen ::bucket-range))])))

(s/def ::experiment-id
  (s/with-gen (s/and string? seq)
    #(gen/elements ["checkout_flow_v2" "search_ranking" "pdp_layout"])))

(s/def ::bucketing-key
  (s/with-gen (s/and string? seq)
    #(gen-id "user_")))
