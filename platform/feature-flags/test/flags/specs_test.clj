(ns flags.specs-test
  "Generative checks for every pure s/fdef'd fn, plus data-spec sanity.
  Per https://clojure.org/guides/spec (Testing)."
  (:require [clojure.spec.alpha :as s]
            [clojure.spec.test.alpha :as stest]
            [clojure.test :refer [deftest is testing use-fixtures]]
            [flags.core :as core]
            [flags.specs :as specs]))

(def ^:private check-opts {:clojure.spec.test.check/opts {:num-tests 50}})

;; Side-effecting fns: fdef'd for instrumentation, never generatively checked.
(def ^:private side-effecting
  #{`core/get-cached-flag `core/set-cached-flag `core/invalidate-cached-flag ; Redis
    `core/evaluate `core/evaluate-batch                                      ; Redis cache + get-flag-fn
    `core/init!})                                                            ; alter-var-root

(defn- checkable []
  (remove side-effecting (stest/enumerate-namespace 'flags.core)))

(deftest fdefs-hold-under-generative-testing
  (let [results (stest/check (checkable) check-opts)]
    (is (seq results) "expected at least one fdef'd fn to check")
    (doseq [r results]
      (testing (str (:sym r))
        (is (nil? (:failure r))
            (pr-str (stest/abbrev-result r)))))))

(deftest data-specs-generate-and-conform
  (doseq [k [::specs/traceparent-fields ::specs/context ::specs/clause ::specs/rule
             ::specs/rollout ::specs/flag ::specs/evaluation]]
    (testing (str k)
      (is (every? (fn [[v _]] (s/valid? k v)) (s/exercise k 10))))))

(use-fixtures :once
  (fn [f] (stest/instrument) (try (f) (finally (stest/unstrument)))))

(def ^:private checkout-v2
  "The flag from the flags.core comment block."
  (core/make-flag
   {:key     "checkout-v2"
    :name    "Checkout V2 Redesign"
    :default false
    :enabled true
    :rules   [(core/make-rule
               {:description "Beta users"
                :clauses     [{:attribute "user.tier" :op :in :values ["beta" "alpha"]}]
                :variation   true})]
    :rollout (core/make-rollout
              {:enabled true :percentage 25 :bucket-by "user.id" :seed "checkout-v2-2026"})}))

(deftest real-values-conform
  (is (s/valid? ::specs/flag checkout-v2))
  (testing "a beta user matches the targeting rule"
    (let [result (core/evaluate-flag checkout-v2 {:user {:id "usr_123" :tier "beta"}
                                                  :trace-id "abc123"})]
      (is (= [true :rule-match "abc123"] ((juxt :value :reason :trace-id) result)))))
  (testing "a standard user falls through to the deterministic rollout"
    (let [ctx {:user {:id "usr_456" :tier "standard"}
               :traceparent "00-4bf92f3577b34da6a3ce929d0e0e4736-00f067aa0ba902b7-01"}
          result (core/evaluate-flag checkout-v2 ctx)]
      (is (= :rollout (:reason result)))
      (is (= result (assoc (core/evaluate-flag checkout-v2 ctx) :evaluation-ms (:evaluation-ms result))))
      (is (= "4bf92f3577b34da6a3ce929d0e0e4736" (:trace-id result)))))
  (testing "disabled and missing flags"
    (is (= [false :disabled] ((juxt :value :reason) (core/evaluate-flag (assoc checkout-v2 :enabled false) {}))))
    (is (= :not-found (:reason (core/evaluate-flag nil {} {:flag-key "nope"}))))))
