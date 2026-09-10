(ns trace-spine.specs-test
  "Generative checks for every pure s/fdef'd fn, plus data-spec sanity.
  Per https://clojure.org/guides/spec (Testing)."
  (:require [clojure.spec.alpha :as s]
            [clojure.spec.gen.alpha :as gen]
            [clojure.spec.test.alpha :as stest]
            [clojure.test :refer [are deftest is testing use-fixtures]]
            [clojure.tools.logging :as log]
            [clojure.tools.logging.impl :as log-impl]
            [trace-spine.core :as core]
            [trace-spine.middleware]
            [trace-spine.specs :as specs]))

;; extract logs a warning for every malformed header it is handed.
(use-fixtures :once
  (fn [f] (binding [log/*logger-factory* log-impl/disabled-logger-factory] (f))))

(def ^:private check-opts {:clojure.spec.test.check/opts {:num-tests 50}})

;; Not generatively checked:
(def ^:private excluded
  ;; throws by contract for an internal call without a traceparent
  ;; (L1 continue_or_start); covered by properties-test/no-fabrication
  #{`core/continue-or-start})

(defn- checkable []
  (remove excluded (stest/enumerate-namespace '[trace-spine.core trace-spine.middleware])))

(deftest fdefs-hold-under-generative-testing
  (let [results (stest/check (checkable) check-opts)]
    (is (seq results) "expected at least one fdef'd fn to check")
    (doseq [r results]
      (testing (str (:sym r))
        (is (nil? (:failure r))
            (pr-str (stest/abbrev-result r)))))))

(deftest data-specs-generate-and-conform
  (doseq [k [::specs/trace-id ::specs/span-id ::specs/flags ::specs/traceparent
             ::specs/traceparent-fields ::specs/traceparent-candidate ::specs/tracestate
             ::specs/trace-context ::specs/wire-context ::specs/carrier
             ::specs/ring-request ::specs/ring-response ::specs/middleware-opts]]
    (testing (str k)
      (is (every? (fn [[v _]] (s/valid? k v)) (s/exercise k 10))))))

(deftest l1-wire-examples
  (testing "the L1-wire examples conform"
    (are [tp] (s/valid? ::specs/traceparent tp)
      "00-4bf92f3577b34da6a3ce929d0e0e4736-00f067aa0ba902b7-01"
      "00-0af7651916cd43dd8448eb211c80319c-b7ad6b7169203331-00"))
  (testing "forbidden values and near misses do not"
    (are [tp] (not (s/valid? ::specs/traceparent tp))
      "00-00000000000000000000000000000000-00f067aa0ba902b7-01" ; forbidden trace-id
      "00-4bf92f3577b34da6a3ce929d0e0e4736-0000000000000000-01" ; forbidden parent-id
      "00-4BF92F3577B34DA6A3CE929D0E0E4736-00F067AA0BA902B7-01" ; uppercase
      "01-4bf92f3577b34da6a3ce929d0e0e4736-00f067aa0ba902b7-01" ; version
      "ff-4bf92f3577b34da6a3ce929d0e0e4736-00f067aa0ba902b7-01"
      "00-4bf92f3577b34da6a3ce929d0e0e473-00f067aa0ba902b7-01"  ; 31-hex trace-id
      "00-4bf92f3577b34da6a3ce929d0e0e4736-00f067aa0ba902b-01"  ; 15-hex parent-id
      "00-4bf92f3577b34da6a3ce929d0e0e4736-00f067aa0ba902b7-1"  ; 1-hex flags
      "00-4bf92f3577b34da6a3ce929d0e0e4736-00f067aa0ba902b7-01-00"
      " 00-4bf92f3577b34da6a3ce929d0e0e4736-00f067aa0ba902b7-01"
      ""
      nil))
  (testing "every generated near miss is rejected"
    (is (not-any? #(s/valid? ::specs/traceparent %)
                  (gen/sample (specs/gen-malformed-traceparent) 500))))
  (testing "field split"
    (is (= {:version "00" :trace-id "4bf92f3577b34da6a3ce929d0e0e4736"
            :parent-id "00f067aa0ba902b7" :flags "01"}
           (specs/traceparent-fields
            "00-4bf92f3577b34da6a3ce929d0e0e4736-00f067aa0ba902b7-01")))))

(deftest l1-wire-tracestate
  (are [ts] (s/valid? ::specs/tracestate ts)
    "rojo=00f067aa0ba902b7,congo=t61rcWkgMzE"
    "t61@dd=1, ot=p:8;r:62")
  (are [ts] (not (s/valid? ::specs/tracestate ts))
    "Rojo=1"                    ; keys are lowercase
    "rojo"                      ; no value
    "rojo=a,b"                  ; second member has no '='
    (apply str "k=" (repeat 520 "v"))))

(deftest real-values-conform
  (is (s/valid? ::specs/trace-context (core/start-trace)))
  (is (s/valid? ::specs/trace-context (core/start-trace {:sampled? false})))
  (is (s/valid? ::specs/trace-context
                (core/create-child (core/parse-traceparent
                                    "00-4bf92f3577b34da6a3ce929d0e0e4736-00f067aa0ba902b7-01"))))
  (is (s/valid? ::specs/carrier (core/inject (core/start-trace) {"accept" "*/*"}))))
