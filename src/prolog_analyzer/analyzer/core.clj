(ns prolog-analyzer.analyzer.core
  (:require [clojure.tools.logging :as log]
            [clojure.spec.alpha :as s]
            [orchestra.core :refer [defn-spec]]
            [orchestra.spec.test :as stest]
            [prolog-analyzer.analyzer.calculate-env :as calc]
            [prolog-analyzer.record-utils :as ru]
            [prolog-analyzer.specs :as specs]
            [prolog-analyzer.records :as r]
            [prolog-analyzer.utils :as utils]))

(defn-spec ^:private create-spec-from-prespecs ::specs/spec
  [data ::specs/data, pred-id ::specs/pred-id]
  (let [arity (last pred-id)
        pre-specs (utils/get-pre-specs pred-id data)]
    (if (empty? pre-specs) ;;TODO: Check, why this is necessary?
      (r/->TupleSpec (vec (repeat arity (r/->AnySpec))))
      (->> pre-specs
           (map r/->TupleSpec)
           set
           r/->OneOfSpec
           (#(ru/simplify % false)) ;;TODO: change back to single
           ))))

(defn-spec ^:private get-post-specs ::specs/post-specs
  [data ::specs/data, pred-id ::specs/pred-id]
  (or (utils/get-post-specs pred-id data) []))

(defn-spec ^:private subgoal-analyzer ::specs/env
  [data ::specs/data, env ::specs/env, {:keys [goal module arity arglist]} ::specs/goal]
  (log/trace (utils/format-log env "Analysis of Subgoal " goal))
  (if (or
       (zero? arity)
       (= :or goal)
       (= :if goal)
       (= :not goal))
    env
    (let [pred-id [module goal arity]
          pre-spec (create-spec-from-prespecs data pred-id)
          post-specs (get-post-specs data pred-id)]
      (calc/get-env-for-subgoal env pred-id arglist pre-spec post-specs))))

(defn-spec ^:private analyze-clause ::specs/env
  [data ::specs/data, clause-id ::specs/clause-id, {arglist :arglist body :body} ::specs/clause, prespec-as-spec ::specs/spec]
  (log/trace (utils/format-log clause-id "Analyse clause"))
  (reduce
   (partial subgoal-analyzer data)
   (calc/get-env-for-head clause-id arglist prespec-as-spec)
   body))

(def total (atom 0))
(def process (agent 1))

(defn-spec ^:private log-start pos-int?
  [counter int?, pred-id ::specs/pred-id, clause-number int?]
  (log/debug (utils/format-log (vec (conj pred-id clause-number)) "Start - " counter "/" @total))
  (inc counter))

(defn-spec ^:private log-end nil?
  [pred-id ::specs/pred-id, clause-number int?]
  (log/debug (utils/format-log (vec (conj pred-id clause-number)) "End")))

(defn create-result [env]
  (let [[m n a _ :as clause-id] (utils/get-title env)]
    {:pred-id         [m n a]
     :clause-id       clause-id
     :conclusion      (->> env
                           utils/get-arguments
                           (map-indexed (fn [i elem] {:id i :type (ru/simplify (utils/get-dom-of-term env elem))}))
                           vec
                           hash-set)
     :errors         (utils/errors2 env)}))

(defn-spec ^:private execute-task ::specs/env
  [data ::specs/data, [pred-id clause-number] (s/tuple ::specs/pred-id ::specs/id)]
  (send process log-start pred-id clause-number)
  (let [clause-id (conj pred-id clause-number)
        clause (utils/get-clause pred-id clause-number data)
        pre-spec (create-spec-from-prespecs data pred-id)
        res (analyze-clause data clause-id clause pre-spec)]
    (log-end pred-id clause-number)
    (create-result res)))

(defn-spec ^:private build-tasks (s/coll-of (s/tuple ::specs/pred-id ::specs/id))
  [data ::specs/data]
  (let [res (for [pred-id (utils/get-pred-identities data)
                  clause-number (utils/get-clause-identities-of-pred pred-id data)]
              [pred-id clause-number])
        all (count res)]
    (reset! total all)
    res))

(defn-spec complete-analysis ::specs/envs
  "Analysis every clause in the data, and returns an environment for each"
  [data ::specs/data]
  (send process (fn [_] 1))
  (let [tasks (build-tasks data)]
    (pmap (partial execute-task data) tasks)))
