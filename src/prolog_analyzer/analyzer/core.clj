(ns prolog-analyzer.analyzer.core
  (:require [prolog-analyzer.utils :as utils]
            [prolog-analyzer.record-utils :as i]
            [prolog-analyzer.records :as r]
            [prolog-analyzer.record-utils :as ru]
            [clojure.tools.logging :as log]
            [prolog-analyzer.analyzer.calculate-env :as calc]
            [prolog-analyzer.state :as state]))


(defn- get-pre-spec [data pred-id]
  (->> data
       (utils/get-pre-specs pred-id)
       (map r/->TupleSpec)
       set
       r/->OneOfSpec
       ru/simplify
       ))

(defn- get-post-specs [data pred-id]
  (if-let [pss (utils/get-post-specs pred-id data)]
    pss
    []))

(defn- subgoal-analyzer [data env {:keys [goal module arity arglist]}]
  (log/trace (utils/format-log env "Analysis of Subgoal " goal))
  (if (or
       (zero? arity)
       (= :or goal)
       (= :if goal))
    env
    (let [pred-id [module goal arity]
          pre-spec (get-pre-spec data pred-id)
          post-specs (get-post-specs data pred-id)]
      (calc/get-env-for-subgoal env pred-id arglist pre-spec post-specs))))

(defn- analyze-clause [data title {arglist :arglist body :body} pre-spec]
  (log/trace (utils/format-log title "Analyse clause"))
  (reduce
   (partial subgoal-analyzer data)
   (calc/get-env-for-head title arglist pre-spec)
   body))

(defn- build-tasks [data]
  (let [res (for [pred-id (utils/get-pred-identities data)
                  clause-number (utils/get-clause-identities-of-pred pred-id data)]
              [pred-id clause-number])]
    (log/debug "Number of tasks: " (count res))
    res))

(defn- execute-task [data [pred-id clause-number]]
  (log/debug (utils/format-log (vec (conj pred-id clause-number)) "Execute Analysis"))
  (analyze-clause
   data
   (conj pred-id clause-number)
   (utils/get-clause pred-id clause-number data)
   (get-pre-spec data pred-id)))

(defn complete-analysis [data]
  (let [tasks (build-tasks data)]
    (map (partial execute-task data) tasks)))
