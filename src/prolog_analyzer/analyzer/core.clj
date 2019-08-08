(ns prolog-analyzer.analyzer.core
  (:require [prolog-analyzer.utils :as utils]
            [prolog-analyzer.record-utils :as i]
            [prolog-analyzer.records :as r]
            [prolog-analyzer.analyzer.calculate-env :as calc]
   ))


(defn- get-pre-spec [pred-id data]
  (i/simplify
   (->> data
        (utils/get-pre-specs pred-id)
        (map r/->TupleSpec)
        set
        r/->OneOfSpec)
   (:specs data)
   true))

(defn- get-post-specs [pred-id data]
  (if-let [pss (utils/get-post-specs pred-id data)]
    pss
    []))


(defn- subgoal-analyzer [{defs :specs :as data} env {goal-name :goal module :module arity :arity arglist :arglist}]
  (if (zero? arity)
    env
    (let [pred-id [module goal-name arity]
          pre-spec (get-pre-spec pred-id data)
          post-specs (get-post-specs pred-id data)]
      (calc/get-env-for-subgoal defs env arglist pre-spec post-specs))))

(defn- analyze-clause [data {arglist :arglist body :body} pre-spec]
  (reduce
   (partial subgoal-analyzer data)
   (calc/get-env-for-header (:specs data) arglist pre-spec)
   body))

(defn complete-analysis [data]
  (when (empty? (utils/get-pred-identities data))
    (println (pr-str "No predicates found")))
  (for [pred-id (utils/get-pred-identities data)
        clause-number (utils/get-clause-identities-of-pred pred-id data)]
    (analyze-clause
     data
     (utils/get-clause pred-id clause-number data)
     (get-pre-spec pred-id data))))
