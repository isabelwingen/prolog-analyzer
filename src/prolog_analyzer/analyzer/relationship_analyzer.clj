(ns prolog-analyzer.analyzer.relationship-analyzer
  (:require [ubergraph.core :as uber]))


(defn- same [env other-env]
  (= env env))


(defn- step [env]
  env)


(defn fixpoint-analysis [env]
  (loop [in env]
    (let [next (step in)]
      (if (same in next)
        next
        (recur next)))))
