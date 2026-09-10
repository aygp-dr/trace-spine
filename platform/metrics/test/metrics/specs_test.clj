(ns metrics.specs-test
  "Generative checks for every pure s/fdef'd fn, plus data-spec sanity.
  Per https://clojure.org/guides/spec (Testing)."
  (:require [clojure.core.async :as async]
            [clojure.spec.alpha :as s]
            [clojure.spec.test.alpha :as stest]
            [clojure.test :refer [deftest is testing use-fixtures]]
            [clojure.tools.logging :as log]
            [clojure.tools.logging.impl :as log-impl]
            [metrics.core :as core]
            [metrics.specs :as specs]
            [trace-spine.specs :as ts]))

(use-fixtures :once
  (fn [f] (binding [log/*logger-factory* log-impl/disabled-logger-factory] (f))))

(def ^:private check-opts {:clojure.spec.test.check/opts {:num-tests 50}})

;; Not generatively checked:
(def ^:private excluded
  #{`core/collect-events!          ; puts onto a channel; throws E001/E002 by contract (tested below)
    `core/get-assignment           ; publishes an exposure event; throws E005 by contract (tested below)
    `core/start-collector-worker!  ; go-loop lifecycle
    `core/start-system! `core/stop-system!})

(defn- checkable []
  (remove excluded (stest/enumerate-namespace 'metrics.core)))

(deftest fdefs-hold-under-generative-testing
  (let [results (stest/check (checkable) check-opts)]
    (is (seq results) "expected at least one fdef'd fn to check")
    (doseq [r results]
      (testing (str (:sym r))
        (is (nil? (:failure r))
            (pr-str (stest/abbrev-result r)))))))

(deftest data-specs-generate-and-conform
  (doseq [k [::specs/trace-context ::specs/wire-context ::specs/event ::specs/raw-event
             ::specs/validation ::specs/allocation ::specs/collect-result]]
    (testing (str k)
      (is (every? (fn [[v _]] (s/valid? k v)) (s/exercise k 10))))))

(def ^:private ctx
  "The trace context from the metrics.core comment block."
  {:trace-id "4bf92f3577b34da6a3ce929d0e0e4736" :span-id "00f067aa0ba902b7" :flags 1})

(def ^:private impression
  {:type "impression" :event_id "evt_abc123" :session_id "sess_xyz789"
   :timestamp "2026-09-10T08:00:00Z" :properties {:product_id "prod_001"}})

(use-fixtures :each
  (fn [f] (stest/instrument) (try (f) (finally (stest/unstrument)))))

(deftest collect-events
  (let [collector (core/create-event-collector {:sink (core/->InMemoryEventSink (atom []))
                                                :batch-size 3})]
    (testing "valid events are accepted and enriched with the trace id"
      (is (= {:accepted 1 :rejected 1 :trace_id (:trace-id ctx)}
             (core/collect-events! collector [impression (dissoc impression :session_id)] ctx)))
      (is (= (:trace-id ctx) (:trace_id (async/poll! (:event-chan collector))))))
    (testing "E002: no trace context"
      (is (= "E002" (try (core/collect-events! collector [impression] nil) nil
                         (catch clojure.lang.ExceptionInfo e (:code (ex-data e)))))))
    (testing "E001: batch larger than :batch-size"
      (is (= "E001" (try (core/collect-events! collector (repeat 4 impression) ctx) nil
                         (catch clojure.lang.ExceptionInfo e (:code (ex-data e)))))))))

(deftest assignment
  (let [store (core/->InMemoryExperimentStore (atom {}))
        sink (core/->InMemoryEventSink (atom []))
        engine (core/create-ab-engine {:experiment-store store
                                       :flag-client (core/->MockFeatureFlagClient)
                                       :event-sink sink})]
    (core/create-experiment! store {:experiment_id "checkout_flow_v2"
                                    :allocation {:control [0 49] :treatment [50 99]}})
    (testing "deterministic, and the exposure carries the trace context"
      (let [a (core/get-assignment engine "checkout_flow_v2" "user_123" ctx)]
        (is (= a (core/get-assignment engine "checkout_flow_v2" "user_123" ctx)))
        (is (#{:control :treatment} (:variant a)))
        (is (= ctx (:_trace-ctx (first @(:events-atom sink)))))))
    (testing "E005: unknown experiment"
      (is (= "E005" (try (core/get-assignment engine "nope" "user_123" ctx) nil
                         (catch clojure.lang.ExceptionInfo e (:code (ex-data e)))))))))

(deftest trace-middleware
  (let [app (core/wrap-trace-context (fn [req] {:status 202 :body (:trace-ctx req)}))]
    (testing "a valid traceparent is continued with a child span"
      (let [resp (app {:headers {"traceparent" "00-4bf92f3577b34da6a3ce929d0e0e4736-00f067aa0ba902b7-01"}})]
        (is (= (:trace-id ctx) (get-in resp [:body :trace-id])))
        (is (s/valid? ::ts/traceparent (get-in resp [:headers "traceparent"])))))
    (testing "a missing traceparent is rejected with E002"
      (is (= 400 (:status (app {:headers {}})))))))
