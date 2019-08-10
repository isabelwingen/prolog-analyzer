(ns prolog-analyzer.analyzer.core
  (:require [prolog-analyzer.utils :as utils]
            [prolog-analyzer.record-utils :as i]
            [prolog-analyzer.records :as r]
            [prolog-analyzer.record-utils :as ru]
            [prolog-analyzer.analyzer.calculate-env :as calc]
            [prolog-analyzer.state :as state]
   ))

(def data (atom {}))

(defn- get-pre-spec [pred-id]
  (->> @data
       (utils/get-pre-specs pred-id)
       (map r/->TupleSpec)
       set
       r/->OneOfSpec
   ;    ru/simplify
       ))

(defn- get-post-specs [pred-id]
  (if-let [pss (utils/get-post-specs pred-id @data)]
    pss
    []))


(defn- subgoal-analyzer [env {:keys [goal module arity arglist]}]
  (if (zero? arity)
    env
    (let [pred-id [module goal arity]
          pre-spec (get-pre-spec pred-id)
          post-specs (get-post-specs pred-id)]
      (calc/get-env-for-subgoal env arglist pre-spec post-specs))))

(defn- analyze-clause [title {arglist :arglist body :body} pre-spec]
  (reduce
   subgoal-analyzer
   (calc/get-env-for-head title arglist pre-spec)
   body))

(defn build-tasks []
  (for [pred-id (utils/get-pred-identities @data)
        clause-number (utils/get-clause-identities-of-pred pred-id @data)]
    [pred-id clause-number]))

(defn execute-task [[pred-id clause-number]]
  (analyze-clause
   (conj pred-id clause-number)
   (utils/get-clause pred-id clause-number @data)
   (get-pre-spec pred-id)))

(defn complete-analysis [in-data]
  (reset! data in-data)
  (when (empty? (utils/get-pred-identities in-data))
    (println (pr-str "No predicates found")))
  (reset! state/user-typedefs (:specs in-data))
  (let [tasks (build-tasks)]
    (pmap execute-task tasks)))
