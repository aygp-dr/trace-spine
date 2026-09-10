(ns wms.specs
  "Data specs for the WMS Service (https://clojure.org/guides/spec).

  Trace ids and the traceparent header come from trace-spine.specs (lib/clj),
  the monorepo's single definition of the W3C wire format; WMS carries the
  flags byte as an integer. Function specs (s/fdef) live next to each defn
  in wms.core."
  (:require [clojure.spec.alpha :as s]
            [clojure.spec.gen.alpha :as gen]
            [trace-spine.specs :as ts]
            [wms.context :as-alias ctx])
  (:import [org.apache.kafka.common.header Header]
           [org.apache.kafka.common.header.internals RecordHeader]))

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
;; Kafka
;; =============================================================================

(defn- record-header [^String k ^String v]
  (RecordHeader. k (.getBytes v "UTF-8")))

(s/def ::kafka-header
  (s/with-gen #(instance? Header %)
    #(gen/fmap (fn [[k v]] (record-header k v))
               (gen/tuple (gen/elements ["traceparent" "tracestate" "content-type"])
                          (gen/one-of [(ts/gen-traceparent)
                                       (ts/gen-malformed-traceparent)
                                       (gen/string-alphanumeric)])))))

(s/def ::kafka-headers (s/nilable (s/coll-of ::kafka-header :max-count 4)))

(s/def ::bootstrap-servers
  (s/with-gen (s/and string? seq)
    #(gen/elements ["localhost:9092" "kafka-1:9092,kafka-2:9092"])))
(s/def ::group-id
  (s/with-gen (s/and string? seq)
    #(gen/elements ["wms" "wms-allocator"])))
(s/def ::kafka-config (s/keys :req-un [::bootstrap-servers ::group-id]))

;; =============================================================================
;; Orders, inventory, outbox
;; =============================================================================

(s/def ::db-spec
  (s/with-gen map?
    #(gen/return {:dbtype "postgresql" :dbname "wms"})))

(s/def ::sku
  (s/with-gen (s/and string? seq)
    #(gen/fmap (fn [n] (str "SKU-" n)) (gen/large-integer* {:min 1 :max 99999}))))
(s/def ::quantity pos-int?)
(s/def ::item (s/keys :req-un [::sku ::quantity]))
(s/def ::items (s/coll-of ::item :kind vector? :min-count 1 :gen-max 5))
(s/def ::warehouse-id (s/and string? seq))

(s/def ::order-id
  (s/with-gen (s/and string? seq)
    #(gen/fmap (fn [n] (str "ORD-" n)) (gen/large-integer* {:min 1 :max 999999}))))
(s/def ::shipping-address (s/nilable map?))
(s/def ::shipping-method (s/nilable string?))
(s/def ::order
  (s/keys :req-un [::order-id ::items] :opt-un [::shipping-address ::shipping-method]))

(s/def ::event-type
  #{"fulfillment.allocated" "fulfillment.shipped" "return.processed" "inventory.low_stock"})
(s/def ::aggregate-id (s/and string? seq))
(s/def ::payload map?)
(s/def ::traceparent ::ts/traceparent)
(s/def ::tracestate (s/nilable ::ts/tracestate))
(s/def ::outbox-event
  (s/keys :req-un [::event-type ::aggregate-id ::payload ::traceparent]
          :opt-un [::tracestate]))

(s/def ::outbox-fn
  (s/with-gen ifn?
    #(gen/return (fn [_tx _event] nil))))

(s/def ::system-config (s/keys :req-un [::db-spec] :opt-un [::kafka-config]))
