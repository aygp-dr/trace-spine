(ns payments.retry
  "Retry logic with exponential backoff and jitter.

   Implements retry policy for transient failures while respecting
   non-retryable error codes from payment processors."
  (:require
   [clojure.spec.alpha :as s]
   [clojure.spec.gen.alpha :as gen]
   [clojure.tools.logging :as log]
   [payments.specs :as specs])
  (:import
   [java.util Random]))

;;; ---------------------------------------------------------------------------
;;; Retry Classification
;;; ---------------------------------------------------------------------------

(def ^:private retryable-error-types
  "Error types that should trigger a retry."
  #{:network-timeout
    :connection-refused
    :connection-reset
    :rate-limited
    :service-unavailable
    :gateway-timeout
    :lock-timeout
    :temporary-failure})

(def ^:private non-retryable-error-types
  "Error types that should NOT trigger a retry."
  #{:card-declined
    :insufficient-funds
    :invalid-card
    :expired-card
    :fraud-detected
    :invalid-request
    :authentication-failed
    :not-found
    :conflict})

(defn- retryable?
  "Determine if an exception should trigger a retry."
  [e]
  (let [data (ex-data e)
        error-type (:type data)]
    (cond
      ;; Explicitly non-retryable
      (contains? non-retryable-error-types error-type)
      false

      ;; Explicitly retryable
      (contains? retryable-error-types error-type)
      true

      ;; HTTP status codes
      (when-let [status (:http-status data)]
        (or (= status 429)  ; Rate limited
            (= status 503)  ; Service unavailable
            (= status 504))) ; Gateway timeout
      true

      ;; Network exceptions
      (instance? java.net.SocketTimeoutException e)
      true

      (instance? java.net.ConnectException e)
      true

      ;; Default: don't retry unknown errors
      :else
      false)))

(s/fdef retryable?
  :args (s/cat :e (s/with-gen #(instance? Throwable %)
                    #(gen/one-of
                      [(gen/fmap (fn [t] (ex-info "error" {:type t}))
                                 (gen/elements (concat retryable-error-types
                                                       non-retryable-error-types
                                                       [:unknown nil])))
                       (gen/fmap (fn [st] (ex-info "http" {:http-status st}))
                                 (gen/elements [400 402 404 429 500 502 503 504]))
                       (gen/return (java.net.SocketTimeoutException. "timeout"))
                       (gen/return (java.net.ConnectException. "refused"))])))
  :ret boolean?
  :fn (fn [{{:keys [e]} :args ret :ret}]
        (let [{:keys [type http-status]} (ex-data e)]
          (= ret (boolean
                  (and (not (contains? non-retryable-error-types type))
                       (or (contains? retryable-error-types type)
                           (contains? #{429 503 504} http-status)
                           (instance? java.net.SocketTimeoutException e)
                           (instance? java.net.ConnectException e))))))))

;;; ---------------------------------------------------------------------------
;;; Backoff Calculation
;;; ---------------------------------------------------------------------------

(def ^:private random (Random.))

(defn- calculate-delay
  "Calculate delay with exponential backoff and jitter.

   delay = min(max_delay, initial_delay * (multiplier ^ attempt))
   jittered = delay * (1 + random(-jitter, +jitter))"
  [{:keys [initial-delay-ms max-delay-ms multiplier jitter-factor]} attempt]
  (let [exponential-delay (* initial-delay-ms (Math/pow multiplier attempt))
        capped-delay (min max-delay-ms exponential-delay)
        jitter (* capped-delay jitter-factor (.nextDouble random) 2)
        jitter-offset (- jitter (* capped-delay jitter-factor))]
    (long (+ capped-delay jitter-offset))))

(s/fdef calculate-delay
  :args (s/cat :policy ::specs/retry-policy :attempt ::specs/attempt)
  :ret nat-int?
  ;; delay = min(max-delay, initial * multiplier^attempt), jittered by
  ;; +/- jitter-factor, with the defaults documented in with-retry
  :fn (fn [{{:keys [policy attempt]} :args ret :ret}]
        (let [{:keys [initial-delay-ms max-delay-ms multiplier jitter-factor]}
              (merge {:initial-delay-ms 100 :max-delay-ms 30000
                      :multiplier 2.0 :jitter-factor 0.2}
                     policy)
              capped (min max-delay-ms (* initial-delay-ms (Math/pow multiplier attempt)))]
          (<= (long (* capped (- 1 jitter-factor))) ret (* capped (+ 1 jitter-factor))))))

;;; ---------------------------------------------------------------------------
;;; Retry Execution
;;; ---------------------------------------------------------------------------

(defn with-retry
  "Execute f with retry logic according to policy.

   Policy map keys:
   - :max-attempts - Maximum number of attempts (default: 3)
   - :initial-delay-ms - Initial delay in ms (default: 100)
   - :max-delay-ms - Maximum delay in ms (default: 30000)
   - :multiplier - Backoff multiplier (default: 2.0)
   - :jitter-factor - Jitter factor 0-1 (default: 0.2)

   Returns result of successful f call, or throws last exception."
  [policy f]
  (let [{:keys [max-attempts]
         :or {max-attempts 3}} policy]
    (loop [attempt 0
           last-exception nil]
      (if (>= attempt max-attempts)
        (do
          (log/error last-exception "All retry attempts exhausted"
                     {:attempts attempt
                      :policy policy})
          (throw last-exception))
        ;; recur is not allowed inside try/catch: the try yields an outcome
        ;; and the loop decides outside it.
        (let [outcome (try
                        {:result (f)}
                        (catch Exception e
                          (if (retryable? e)
                            {:retry e}
                            (do
                              (log/debug "Non-retryable error"
                                         {:error-type (:type (ex-data e))
                                          :error-message (ex-message e)})
                              (throw e)))))]
          (if-let [e (:retry outcome)]
            (let [delay-ms (calculate-delay policy attempt)]
              (log/warn "Retryable error, backing off"
                        {:attempt (inc attempt)
                         :max-attempts max-attempts
                         :delay-ms delay-ms
                         :error-type (:type (ex-data e))
                         :error-message (ex-message e)})
              (Thread/sleep delay-ms)
              (recur (inc attempt) e))
            (do
              (when (pos? attempt)
                (log/info "Retry succeeded"
                          {:attempt (inc attempt)
                           :total-attempts attempt}))
              (:result outcome))))))))

(s/fdef with-retry
  :args (s/cat :policy ::specs/retry-policy :f ifn?)
  :ret any?)

(defn with-retry-async
  "Async version of with-retry using core.async.
   Returns a channel that will receive the result or error."
  [policy f]
  ;; Placeholder for async implementation
  ;; Would use core.async channels and go blocks
  (throw (UnsupportedOperationException. "Async retry not yet implemented")))

(s/fdef with-retry-async
  :args (s/cat :policy ::specs/retry-policy :f ifn?)
  :ret any?)

;;; ---------------------------------------------------------------------------
;;; Default Policies
;;; ---------------------------------------------------------------------------

(def default-policy
  "Default retry policy for payment processing."
  {:max-attempts 5
   :initial-delay-ms 100
   :max-delay-ms 30000
   :multiplier 2.0
   :jitter-factor 0.2})

(def aggressive-policy
  "More aggressive retry policy for critical operations."
  {:max-attempts 10
   :initial-delay-ms 50
   :max-delay-ms 60000
   :multiplier 1.5
   :jitter-factor 0.3})

(def conservative-policy
  "Conservative retry policy for less critical operations."
  {:max-attempts 3
   :initial-delay-ms 200
   :max-delay-ms 10000
   :multiplier 2.0
   :jitter-factor 0.1})
