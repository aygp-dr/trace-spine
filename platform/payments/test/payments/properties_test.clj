(ns payments.properties-test
  "test.check properties for the retry and circuit-breaker contracts, which
  sleep or mutate global state and so are not run through stest/check."
  (:require [clojure.spec.alpha :as s]
            [clojure.spec.gen.alpha :as gen]
            [clojure.test :refer [deftest is use-fixtures]]
            [clojure.test.check :as tc]
            [clojure.test.check.properties :as prop]
            [clojure.tools.logging :as log]
            [clojure.tools.logging.impl :as log-impl]
            [payments.circuit-breaker :as cb]
            [payments.retry :as retry]
            [payments.specs :as specs]))

(use-fixtures :once
  (fn [f] (binding [log/*logger-factory* log-impl/disabled-logger-factory] (f))))

(defn- holds? [n property]
  (let [result (tc/quick-check n property)]
    (is (:pass? result) (pr-str (select-keys result [:seed :shrunk])))))

;; Policies with millisecond delays, so the property does not sleep for long;
;; the remaining keys are optional, as with-retry documents.
(def ^:private gen-fast-policy
  (gen/fmap (fn [[base extra]] (merge extra base))
            (gen/tuple (gen/hash-map :initial-delay-ms (gen/choose 0 3)
                                     :max-delay-ms (gen/choose 0 5))
                       (s/gen (s/keys :opt-un [:payments.policy/max-attempts
                                               :payments.policy/multiplier
                                               :payments.policy/jitter-factor])))))

(deftest with-retry-attempts
  ;; f fails `failures` times with error `type`, then returns :ok.
  ;; Non-retryable errors are thrown at once; retryable ones are retried up
  ;; to :max-attempts (default 3) calls in total, then the last is thrown.
  (holds? 60
          (prop/for-all [policy gen-fast-policy
                         failures (gen/choose 0 5)
                         type (gen/one-of [(s/gen ::specs/retryable-type)
                                           (s/gen ::specs/non-retryable-type)])]
                        (let [calls (atom 0)
                              f (fn [] (if (<= (swap! calls inc) failures)
                                         (throw (ex-info "boom" {:type type}))
                                         :ok))
                              max-attempts (:max-attempts policy 3)
                              outcome (try (retry/with-retry policy f)
                                           (catch clojure.lang.ExceptionInfo e (:type (ex-data e))))]
                          (cond
                            (zero? failures) (and (= :ok outcome) (= 1 @calls))
                            (s/valid? ::specs/non-retryable-type type) (and (= type outcome) (= 1 @calls))
                            (< failures max-attempts) (and (= :ok outcome) (= (inc failures) @calls))
                            :else (and (= type outcome) (= max-attempts @calls)))))))

(deftest circuit-breaker-state-machine
  ;; closed --failure-threshold failures--> open (rejects without calling f)
  ;; --reset-timeout--> half-open --success-threshold successes--> closed
  (holds? 40
          (prop/for-all [failure-threshold (gen/choose 1 5)
                         success-threshold (gen/choose 1 3)]
                        (let [k (keyword (gensym "circuit-"))
                              _ (cb/configure! k {:failure-threshold failure-threshold
                                                  :success-threshold success-threshold
                                                  :half-open-max-calls success-threshold
                                                  :reset-timeout-ms 0})
                              fail (fn [] (throw (ex-info "down" {:type :service-unavailable})))
                              attempt (fn [f] (try (cb/with-circuit-breaker k f)
                                                   (catch clojure.lang.ExceptionInfo e (:type (ex-data e)))))
                              called (atom 0)]
                          (dotimes [_ failure-threshold] (attempt fail))
                          (let [opened (cb/state k)]
                            (Thread/sleep 2)
                            (let [recovered (vec (repeatedly success-threshold
                                                             #(attempt (fn [] (swap! called inc) :ok))))]
                              (and (= :open opened)
                                   (every? #{:ok} recovered)
                                   (= success-threshold @called)
                                   (= :closed (cb/state k))
                                   (do (cb/reset! k) (cb/closed? k)))))))))

(deftest open-circuit-rejects-without-calling
  (let [k (keyword (gensym "circuit-"))
        called (atom 0)]
    (cb/configure! k {:failure-threshold 1 :reset-timeout-ms 60000})
    (try (cb/with-circuit-breaker k #(throw (ex-info "down" {:type :service-unavailable})))
         (catch Exception _))
    (is (cb/open? k))
    (is (= :circuit-open
           (try (cb/with-circuit-breaker k #(swap! called inc)) nil
                (catch clojure.lang.ExceptionInfo e (:type (ex-data e))))))
    (is (zero? @called))
    (is (= {:total 1 :successful 0 :failed 1 :rejected 1 :state :open} (cb/metrics k)))))
