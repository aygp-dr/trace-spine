(ns user
  "REPL entry point (clj -M:dev). Loads the project and instruments every
  s/fdef'd fn so bad calls fail fast with explain-data."
  (:require [clojure.spec.test.alpha :as stest]
            [trace-spine.core]
            [trace-spine.middleware]))

(stest/instrument)
