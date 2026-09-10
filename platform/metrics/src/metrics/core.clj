(ns metrics.core
  "Metrics Platform core functionality.

   Components:
   - Event Collector: Ingests impression/click events from SPA
   - A/B Engine: Deterministic experiment assignment
   - Feature Flag Integration: Variant evaluation via flags service

   All operations propagate trace-id for user journey correlation."
  (:require
   [clojure.core.async :as async :refer [<! >! go go-loop chan]]
   [clojure.spec.alpha :as s]
   [clojure.spec.gen.alpha :as gen]
   [clojure.string :as str]
   [clojure.tools.logging :as log]
   [java-time.api :as jt]
   [metrics.specs :as specs]
   [trace-spine.specs :as ts])
  (:import
   [com.google.common.hash Hashing]
   [java.nio.charset StandardCharsets]))

;; =============================================================================
;; Trace Context Handling
;; =============================================================================

(def ^:private traceparent-pattern
  "W3C Trace Context traceparent format: version-trace_id-span_id-flags"
  #"^00-([0-9a-f]{32})-([0-9a-f]{16})-([0-9a-f]{2})$")

(def ^:private invalid-trace-id
  "Forbidden trace-id per W3C spec"
  "00000000000000000000000000000000")

(def ^:private invalid-span-id
  "Forbidden span-id per W3C spec"
  "0000000000000000")

(defn parse-traceparent
  "Parse W3C traceparent header into trace context map.

   Returns {:trace-id <32-hex> :span-id <16-hex> :flags <int>}
   or nil if invalid."
  [traceparent]
  (when-let [match (and traceparent (re-matches traceparent-pattern traceparent))]
    (let [[_ trace-id span-id flags-hex] match]
      (when (and (not= trace-id invalid-trace-id)
                 (not= span-id invalid-span-id))
        {:trace-id trace-id
         :span-id  span-id
         :flags    (Integer/parseInt flags-hex 16)}))))

(s/fdef parse-traceparent
  :args (s/cat :traceparent ::ts/traceparent-candidate)
  :ret (s/nilable ::specs/trace-context)
  :fn (fn [{{tp :traceparent} :args ret :ret}]
        (if (s/valid? ::ts/traceparent tp)
          (let [fields (ts/traceparent-fields tp)]
            (= ret {:trace-id (:trace-id fields)
                    :span-id (:parent-id fields)
                    :flags (Integer/parseInt (:flags fields) 16)}))
          (nil? ret))))

(defn format-traceparent
  "Format trace context map as W3C traceparent header string."
  [{:keys [trace-id span-id flags]}]
  (format "00-%s-%s-%02x" trace-id span-id (or flags 1)))

(s/fdef format-traceparent
  :args (s/cat :ctx ::specs/wire-context)
  :ret ::ts/traceparent
  ;; parse(format(ctx)) == ctx, with flags defaulting to 1 (sampled)
  :fn (fn [{{:keys [ctx]} :args ret :ret}]
        (= (parse-traceparent ret)
           (merge {:flags 1} (select-keys ctx [:trace-id :span-id :flags])))))

(defn generate-span-id
  "Generate a new random 16-hex span-id."
  []
  (format "%016x" (rand-int Integer/MAX_VALUE)))

(s/fdef generate-span-id
  :args (s/cat)
  :ret ::ts/span-id)

(defn create-child-context
  "Create a child trace context, preserving trace-id and flags."
  [parent-ctx]
  (assoc parent-ctx :span-id (generate-span-id)))

(s/fdef create-child-context
  :args (s/cat :parent-ctx ::specs/trace-context)
  :ret ::specs/trace-context
  :fn (fn [{{parent :parent-ctx} :args child :ret}]
        (and (= (:trace-id child) (:trace-id parent))
             (= (:flags child) (:flags parent))
             (not= (:span-id child) (:span-id parent)))))

(defn extract-trace-context
  "Extract trace context from Ring request headers.

   Returns context map or nil if no valid traceparent."
  [request]
  (-> request
      (get-in [:headers "traceparent"])
      parse-traceparent))

(s/fdef extract-trace-context
  :args (s/cat :request ::ts/ring-request)
  :ret (s/nilable ::specs/trace-context)
  :fn (fn [{{:keys [request]} :args ret :ret}]
        (= (some? ret)
           (s/valid? ::ts/traceparent (get-in request [:headers "traceparent"])))))

;; =============================================================================
;; Event Schema & Validation
;; =============================================================================

(def event-types
  "Valid event types for metrics collection."
  #{:impression :click :add_to_cart :purchase :experiment :error})

(defn- blank-id?
  "True unless x is a non-blank string (ids arrive as arbitrary JSON)."
  [x]
  (not (and (string? x) (not (str/blank? x)))))

(defn validate-event
  "Validate event against schema. Returns {:valid? bool :errors [...]}"
  [event]
  (let [errors (cond-> []
                 (not (event-types (keyword (:type event))))
                 (conj {:field :type :error "invalid event type"})

                 (blank-id? (:event_id event))
                 (conj {:field :event_id :error "required"})

                 (blank-id? (:session_id event))
                 (conj {:field :session_id :error "required"})

                 (nil? (:timestamp event))
                 (conj {:field :timestamp :error "required"}))]
    {:valid? (empty? errors)
     :errors errors}))

(s/fdef validate-event
  :args (s/cat :event ::specs/raw-event)
  :ret ::specs/validation
  :fn (fn [{{:keys [event]} :args ret :ret}]
        (and (= (:valid? ret) (empty? (:errors ret)))
             (= (:valid? ret) (s/valid? ::specs/event event)))))

;; =============================================================================
;; Event Collector
;; =============================================================================

(defprotocol EventSink
  "Protocol for event sinks (Kafka, test mocks, etc.)"
  (publish! [this topic events trace-ctx]
    "Publish events to the sink with trace context."))

(defrecord KafkaEventSink [producer topic-prefix]
  EventSink
  (publish! [_this topic events trace-ctx]
    ;; TODO: Implement actual Kafka producer
    ;; This is a skeleton - real implementation would use kafka-clients
    (log/info "Publishing" (count events) "events to"
              (str topic-prefix "." topic)
              "trace-id:" (:trace-id trace-ctx))
    {:published (count events)
     :topic (str topic-prefix "." topic)
     :trace-id (:trace-id trace-ctx)}))

(defrecord InMemoryEventSink [events-atom]
  EventSink
  (publish! [_this topic events trace-ctx]
    (let [enriched (map #(assoc % :_topic topic :_trace-ctx trace-ctx) events)]
      (swap! events-atom concat enriched)
      {:published (count events)
       :topic topic
       :trace-id (:trace-id trace-ctx)})))

(defn create-event-collector
  "Create an event collector component.

   Options:
   - :sink - EventSink implementation
   - :buffer-size - async channel buffer size (default 1000)
   - :batch-size - max events per batch (default 50)"
  [{:keys [sink buffer-size batch-size]
    :or {buffer-size 1000 batch-size 50}}]
  (let [event-chan (chan buffer-size)]
    {:sink sink
     :event-chan event-chan
     :batch-size batch-size}))

(s/fdef create-event-collector
  :args (s/cat :opts (s/with-gen ::specs/collector-opts
                       #(gen/fmap (fn [[buffer-size batch-size]]
                                    {:sink (->InMemoryEventSink (atom []))
                                     :buffer-size buffer-size
                                     :batch-size batch-size})
                                  (gen/tuple (gen/choose 1 1000) (gen/choose 1 100)))))
  :ret ::specs/collector
  :fn (fn [{{:keys [opts]} :args ret :ret}]
        (and (identical? (:sink opts) (:sink ret))
             (= (:batch-size opts 50) (:batch-size ret)))))

(defn collect-events!
  "Collect events into the event channel for async processing.

   Returns {:accepted n :rejected m :trace-id ...}"
  [{:keys [event-chan batch-size]} events trace-ctx]
  (when-not trace-ctx
    (throw (ex-info "Missing trace context" {:code "E002"})))

  (when (> (count events) batch-size)
    (throw (ex-info "Batch too large"
                    {:code "E001"
                     :max-batch-size batch-size
                     :received (count events)})))

  (let [validation-results (map validate-event events)
        valid-events (->> (map vector events validation-results)
                          (filter (fn [[_ v]] (:valid? v)))
                          (map first))
        rejected-count (- (count events) (count valid-events))]

    ;; Enrich valid events with trace context and server timestamp
    (doseq [event valid-events]
      (let [enriched (-> event
                         (assoc :trace_id (:trace-id trace-ctx))
                         (assoc :span_id (:span-id trace-ctx))
                         (assoc :server_timestamp (jt/instant)))]
        (async/put! event-chan enriched)))

    {:accepted (count valid-events)
     :rejected rejected-count
     :trace_id (:trace-id trace-ctx)}))

(s/fdef collect-events!
  :args (s/cat :collector ::specs/collector
               :events (s/nilable (s/coll-of ::specs/raw-event))
               :trace-ctx (s/nilable ::specs/trace-context))
  :ret ::specs/collect-result)

(defn start-collector-worker!
  "Start background worker that drains event channel to sink.

   Returns a channel that closes when worker stops."
  [{:keys [sink event-chan]} {:keys [flush-interval-ms]
                              :or {flush-interval-ms 100}}]
  (let [done-chan (chan)]
    (go-loop [buffer []]
      (let [timeout-ch (async/timeout flush-interval-ms)
            [val port] (async/alts! [event-chan timeout-ch])]
        (cond
          ;; Channel closed - flush and exit
          (nil? val)
          (do
            (when (seq buffer)
              (publish! sink "events.validated" buffer (first buffer)))
            (async/close! done-chan))

          ;; New event - add to buffer
          (= port event-chan)
          (let [new-buffer (conj buffer val)]
            (if (>= (count new-buffer) 50)
              (do
                (publish! sink "events.validated" new-buffer
                          {:trace-id (:trace_id (first new-buffer))})
                (recur []))
              (recur new-buffer)))

          ;; Timeout - flush if buffer has items
          :else
          (do
            (when (seq buffer)
              (publish! sink "events.validated" buffer
                        {:trace-id (:trace_id (first buffer))}))
            (recur [])))))
    done-chan))

(s/fdef start-collector-worker!
  :args (s/cat :collector ::specs/collector
               :opts (s/keys :opt-un [::flush-interval-ms]))
  :ret ::specs/event-chan)

;; =============================================================================
;; A/B Experiment Engine
;; =============================================================================

(defn murmur3-hash
  "Compute murmur3 32-bit hash of input string."
  [^String s]
  (-> (Hashing/murmur3_32_fixed)
      (.hashString s StandardCharsets/UTF_8)
      .asInt
      Math/abs))

(s/fdef murmur3-hash
  :args (s/cat :s string?)
  :ret nat-int?)

(defn compute-bucket
  "Compute bucket (0-99) for experiment assignment.

   Uses murmur3 hash for deterministic, uniform distribution."
  [experiment-id bucketing-key]
  (let [hash-input (str experiment-id ":" bucketing-key)]
    (mod (murmur3-hash hash-input) 100)))

(s/fdef compute-bucket
  :args (s/cat :experiment-id ::specs/experiment-id :bucketing-key ::specs/bucketing-key)
  :ret ::specs/bucket
  ;; deterministic: the same user always lands in the same bucket
  :fn (fn [{{:keys [experiment-id bucketing-key]} :args ret :ret}]
        (= ret (compute-bucket experiment-id bucketing-key))))

(defn assign-variant
  "Assign variant based on bucket and allocation map.

   Allocation format: {:control [0 49] :treatment [50 99]}
   Returns variant keyword or nil if bucket not covered."
  [bucket allocation]
  (some (fn [[variant [lo hi]]]
          (when (<= lo bucket hi)
            variant))
        allocation))

(s/fdef assign-variant
  :args (s/cat :bucket ::specs/bucket :allocation ::specs/allocation)
  :ret (s/nilable keyword?)
  ;; a variant is returned iff one of the ranges covers the bucket
  :fn (fn [{{:keys [bucket allocation]} :args ret :ret}]
        (if ret
          (let [[lo hi] (get allocation ret)] (<= lo bucket hi))
          (not-any? (fn [[_ [lo hi]]] (<= lo bucket hi)) allocation))))

(defprotocol ExperimentStore
  "Protocol for experiment configuration storage."
  (get-experiment [this experiment-id]
    "Fetch experiment config by ID. Returns nil if not found.")
  (list-experiments [this filters]
    "List experiments matching filters.")
  (create-experiment! [this config]
    "Create new experiment.")
  (update-experiment! [this experiment-id updates]
    "Update experiment config."))

(defrecord InMemoryExperimentStore [experiments-atom]
  ExperimentStore
  (get-experiment [_this experiment-id]
    (get @experiments-atom experiment-id))

  (list-experiments [_this filters]
    (let [exps (vals @experiments-atom)]
      (cond->> exps
        (:status filters) (filter #(= (:status %) (:status filters)))
        true              (sort-by :created_at))))

  (create-experiment! [_this config]
    (let [now (jt/instant)
          full-config (assoc config
                             :created_at now
                             :status :scheduled)]
      (swap! experiments-atom assoc (:experiment_id config) full-config)
      full-config))

  (update-experiment! [_this experiment-id updates]
    (swap! experiments-atom update experiment-id merge updates)
    (get @experiments-atom experiment-id)))

(defprotocol FeatureFlagClient
  "Protocol for feature flag service integration."
  (evaluate-flag [this flag-id context]
    "Evaluate flag for context. Returns {:enabled bool :variant kw}"))

(defrecord MockFeatureFlagClient []
  FeatureFlagClient
  (evaluate-flag [_this flag-id context]
    ;; Mock implementation - always returns enabled
    (log/debug "Evaluating flag" flag-id "for context" context)
    {:enabled true
     :variant :default}))

(defn create-ab-engine
  "Create A/B experiment engine.

   Options:
   - :experiment-store - ExperimentStore implementation
   - :flag-client - FeatureFlagClient implementation
   - :event-sink - EventSink for exposure tracking"
  [{:keys [experiment-store flag-client event-sink]}]
  {:experiment-store experiment-store
   :flag-client flag-client
   :event-sink event-sink})

(s/fdef create-ab-engine
  :args (s/cat :opts (s/with-gen map?
                       #(gen/return {:experiment-store (->InMemoryExperimentStore (atom {}))
                                     :flag-client (->MockFeatureFlagClient)
                                     :event-sink (->InMemoryEventSink (atom []))})))
  :ret map?
  :fn (fn [{{:keys [opts]} :args ret :ret}]
        (= (select-keys opts [:experiment-store :flag-client :event-sink]) ret)))

(defn get-assignment
  "Get experiment variant assignment for bucketing key.

   Steps:
   1. Fetch experiment config
   2. Check if experiment is active (via feature flag)
   3. Compute bucket from hash
   4. Assign variant from allocation
   5. Record exposure event

   Returns {:experiment_id :variant :bucket :metadata}"
  [{:keys [experiment-store flag-client event-sink]}
   experiment-id
   bucketing-key
   trace-ctx]
  (let [experiment (get-experiment experiment-store experiment-id)]
    (when-not experiment
      (throw (ex-info "Experiment not found"
                      {:code "E005" :experiment-id experiment-id})))

    ;; Check feature flag for experiment activation
    (let [flag-result (when-let [flag-id (:feature_flag_id experiment)]
                        (evaluate-flag flag-client flag-id
                                       {:user_id bucketing-key}))
          active? (or (nil? (:feature_flag_id experiment))
                      (:enabled flag-result))
          bucket (compute-bucket experiment-id bucketing-key)
          variant (when active?
                    (assign-variant bucket (:allocation experiment)))]

      ;; Record exposure event
      (when (and variant event-sink)
        (publish! event-sink "experiments"
                  [{:type :experiment
                    :experiment_id experiment-id
                    :bucketing_key bucketing-key
                    :variant variant
                    :bucket bucket
                    :timestamp (jt/instant)}]
                  trace-ctx))

      {:experiment_id experiment-id
       :variant variant
       :bucketing_key bucketing-key
       :bucket bucket
       :metadata {:started_at (:start_date experiment)
                  :allocation (:allocation experiment)
                  :flag_enabled (:enabled flag-result)}})))

(s/fdef get-assignment
  :args (s/cat :ab-engine map?
               :experiment-id ::specs/experiment-id
               :bucketing-key ::specs/bucketing-key
               :trace-ctx (s/nilable ::specs/trace-context))
  :ret map?)

;; =============================================================================
;; HTTP Handlers (Ring)
;; =============================================================================

(defn wrap-trace-context
  "Ring middleware to extract and inject trace context."
  [handler]
  (fn [request]
    (if-let [trace-ctx (extract-trace-context request)]
      (let [child-ctx (create-child-context trace-ctx)
            response (handler (assoc request :trace-ctx child-ctx))]
        (-> response
            (assoc-in [:headers "traceparent"] (format-traceparent child-ctx))))
      ;; Missing traceparent - reject with error
      {:status 400
       :headers {"Content-Type" "application/json"}
       :body {:error {:code "E002"
                      :message "Missing traceparent header"
                      :trace_id nil
                      :details {:required_header "traceparent"
                                :format "00-{32 hex}-{16 hex}-{2 hex}"}}}})))

(s/fdef wrap-trace-context
  :args (s/cat :handler ::ts/handler)
  :ret fn?)

(defn events-batch-handler
  "Handler for POST /events/batch"
  [collector]
  (fn [request]
    (let [trace-ctx (:trace-ctx request)
          events (get-in request [:body-params :events])]
      (try
        (let [result (collect-events! collector events trace-ctx)]
          {:status 202
           :headers {"Content-Type" "application/json"}
           :body result})
        (catch clojure.lang.ExceptionInfo e
          (let [data (ex-data e)]
            {:status (case (:code data)
                       "E001" 400
                       "E002" 400
                       500)
             :headers {"Content-Type" "application/json"}
             :body {:error {:code (:code data)
                            :message (ex-message e)
                            :trace_id (:trace-id trace-ctx)}}}))))))

(s/fdef events-batch-handler
  :args (s/cat :collector (s/with-gen ::specs/collector
                            #(gen/return (create-event-collector
                                          {:sink (->InMemoryEventSink (atom []))}))))
  :ret fn?)

(defn experiment-assign-handler
  "Handler for GET /experiments/assign"
  [ab-engine]
  (fn [request]
    (let [trace-ctx (:trace-ctx request)
          experiment-id (get-in request [:query-params :experiment_id])
          bucketing-key (get-in request [:query-params :bucketing_key])]
      (try
        (let [result (get-assignment ab-engine experiment-id bucketing-key trace-ctx)]
          {:status 200
           :headers {"Content-Type" "application/json"}
           :body result})
        (catch clojure.lang.ExceptionInfo e
          (let [data (ex-data e)]
            {:status (case (:code data)
                       "E005" 404
                       500)
             :headers {"Content-Type" "application/json"}
             :body {:error {:code (:code data)
                            :message (ex-message e)
                            :trace_id (:trace-id trace-ctx)}}}))))))

(s/fdef experiment-assign-handler
  :args (s/cat :ab-engine (s/with-gen map?
                            #(gen/return (create-ab-engine
                                          {:experiment-store (->InMemoryExperimentStore (atom {}))
                                           :flag-client (->MockFeatureFlagClient)}))))
  :ret fn?)

;; =============================================================================
;; System Lifecycle
;; =============================================================================

(defn create-system
  "Create the metrics platform system map.

   Options:
   - :kafka-config - Kafka producer config
   - :clickhouse-config - ClickHouse connection config
   - :redis-config - Redis connection config
   - :feature-flags-url - Feature flags service URL"
  [_config]
  (let [;; Create sinks and stores
        events-atom (atom [])
        experiments-atom (atom {})

        event-sink (->InMemoryEventSink events-atom)
        experiment-store (->InMemoryExperimentStore experiments-atom)
        flag-client (->MockFeatureFlagClient)

        ;; Create components
        collector (create-event-collector {:sink event-sink})
        ab-engine (create-ab-engine {:experiment-store experiment-store
                                     :flag-client flag-client
                                     :event-sink event-sink})]

    {:collector collector
     :ab-engine ab-engine
     :event-sink event-sink
     :experiment-store experiment-store
     :flag-client flag-client
     ;; Atoms for inspection
     :_events-atom events-atom
     :_experiments-atom experiments-atom}))

(s/fdef create-system
  :args (s/cat :config (s/nilable map?))
  :ret map?
  :fn (fn [{system :ret}]
        (every? #(contains? system %)
                [:collector :ab-engine :event-sink :experiment-store :flag-client])))

(defn start-system!
  "Start the metrics platform system."
  [system]
  (log/info "Starting metrics platform")
  (let [worker-done (start-collector-worker! (:collector system) {})]
    (assoc system :_worker-done worker-done)))

(s/fdef start-system!
  :args (s/cat :system map?)
  :ret map?)

(defn stop-system!
  "Stop the metrics platform system."
  [system]
  (log/info "Stopping metrics platform")
  (when-let [event-chan (get-in system [:collector :event-chan])]
    (async/close! event-chan))
  (when-let [done-chan (:_worker-done system)]
    ;; Wait for worker to finish (with timeout)
    (async/alts!! [done-chan (async/timeout 5000)]))
  (dissoc system :_worker-done))

(s/fdef stop-system!
  :args (s/cat :system map?)
  :ret map?)

(comment
  ;; REPL usage examples

  ;; Create and start system
  (def sys (-> (create-system {})
               start-system!))

  ;; Seed an experiment
  (create-experiment! (:experiment-store sys)
                      {:experiment_id "checkout_flow_v2"
                       :name "Checkout Flow V2"
                       :allocation {:control [0 49]
                                    :treatment [50 99]}
                       :start_date (jt/instant)})

  ;; Test assignment
  (get-assignment (:ab-engine sys)
                  "checkout_flow_v2"
                  "user_123"
                  {:trace-id "4bf92f3577b34da6a3ce929d0e0e4736"
                   :span-id "00f067aa0ba902b7"
                   :flags 1})

  ;; Collect events
  (collect-events! (:collector sys)
                   [{:type "impression"
                     :event_id "evt_abc123"
                     :session_id "sess_xyz789"
                     :timestamp (str (jt/instant))
                     :properties {:product_id "prod_001"}}]
                   {:trace-id "4bf92f3577b34da6a3ce929d0e0e4736"
                    :span-id "00f067aa0ba902b7"
                    :flags 1})

  ;; Check collected events
  @(:_events-atom sys)

  ;; Stop system
  (stop-system! sys))
