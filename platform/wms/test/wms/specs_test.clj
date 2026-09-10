(ns wms.specs-test
  "Generative checks for every pure s/fdef'd fn, plus data-spec sanity.
  Per https://clojure.org/guides/spec (Testing)."
  (:require [clojure.spec.alpha :as s]
            [clojure.spec.test.alpha :as stest]
            [clojure.test :refer [deftest is testing use-fixtures]]
            [clojure.tools.logging :as log]
            [clojure.tools.logging.impl :as log-impl]
            [trace-spine.specs :as ts]
            [wms.core :as core]
            [wms.specs :as specs])
  (:import [org.apache.kafka.common.header.internals RecordHeader]))

;; parse-traceparent logs a warning for every malformed header it is handed.
(use-fixtures :once
  (fn [f] (binding [log/*logger-factory* log-impl/disabled-logger-factory] (f))))

(def ^:private check-opts {:clojure.spec.test.check/opts {:num-tests 50}})

;; Not generatively checked:
(def ^:private excluded
  #{`core/continue-or-start          ; throws by contract (WMS is never ingress); tested below
    `core/select-warehouse `core/insert-outbox-event `core/drain-outbox   ; PostgreSQL
    `core/health-check
    `core/handle-order-created `core/handle-order-cancelled               ; PostgreSQL + Kafka records
    `core/start-kafka-consumer `core/stop-kafka-consumer                  ; go-loop lifecycle
    `core/start-system `core/stop-system})

(defn- checkable []
  (remove excluded (stest/enumerate-namespace 'wms.core)))

(deftest fdefs-hold-under-generative-testing
  (let [results (stest/check (checkable) check-opts)]
    (is (seq results) "expected at least one fdef'd fn to check")
    (doseq [r results]
      (testing (str (:sym r))
        (is (nil? (:failure r))
            (pr-str (stest/abbrev-result r)))))))

(deftest data-specs-generate-and-conform
  (doseq [k [::specs/trace-context ::specs/wire-context ::specs/kafka-headers
             ::specs/kafka-config ::specs/order ::specs/outbox-event ::specs/system-config]]
    (testing (str k)
      (is (every? (fn [[v _]] (s/valid? k v)) (s/exercise k 10))))))

(def ^:private example "00-4bf92f3577b34da6a3ce929d0e0e4736-00f067aa0ba902b7-01")

(deftest continue-or-start
  (testing "a valid upstream traceparent is continued"
    (let [ctx (core/continue-or-start example)]
      (is (s/valid? ::specs/trace-context ctx))
      (is (= "4bf92f3577b34da6a3ce929d0e0e4736" (:trace-id ctx)))
      (is (not= "00f067aa0ba902b7" (:span-id ctx)))))
  (testing "WMS never originates a trace (ADR-0003)"
    (doseq [tp [nil "" "garbage"
                "00-00000000000000000000000000000000-00f067aa0ba902b7-01"]]
      (is (= :programmer-error
             (try (core/continue-or-start tp) nil
                  (catch clojure.lang.ExceptionInfo e (:type (ex-data e)))))
          (pr-str tp)))))

(deftest real-values-conform
  (is (= {:trace-id "4bf92f3577b34da6a3ce929d0e0e4736" :span-id "00f067aa0ba902b7" :flags 1}
         (core/parse-traceparent example)))
  (is (= example (core/format-traceparent (core/parse-traceparent example))))
  (is (= example (core/extract-traceparent-from-headers
                  [(RecordHeader. "content-type" (.getBytes "application/json" "UTF-8"))
                   (RecordHeader. "traceparent" (.getBytes example "UTF-8"))])))
  (is (s/valid? ::ts/traceparent
                (core/format-traceparent (core/create-child-context (core/parse-traceparent example))))))
