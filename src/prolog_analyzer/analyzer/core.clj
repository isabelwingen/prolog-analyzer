(ns prolog-analyzer.analyzer.core
  (:require [prolog-analyzer.utils :as utils]
            [prolog-analyzer.record-utils :as i]
            [prolog-analyzer.records :as r]
            [prolog-analyzer.record-utils :as ru]
            [clojure.tools.logging :as log]
            [prolog-analyzer.analyzer.calculate-env :as calc]
            [orchestra.spec.test :as stest]
            [clojure.spec.alpha :as s]
            [prolog-analyzer.specs :as specs]
            [prolog-analyzer.state :as state]))


(defn- get-pre-spec [data [_ _ arity :as pred-id]]
  (let [pre-specs (utils/get-pre-specs pred-id data)]
    (if (empty? pre-specs) ;;TODO: Check, why this is necessary?
      (r/->TupleSpec (vec (repeat arity (r/->AnySpec))))
      (->> pre-specs
           (map r/->TupleSpec)
           set
           r/->OneOfSpec
           (#(ru/simplify % false)) ;;TODO: change back to single
           ))))

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

(def total (atom 0))
(def process (agent 1))

(defn print-process [counter pred-id clause-number]
  (log/debug (utils/format-log (vec (conj pred-id clause-number)) "Execute Analysis: " counter "/" @total))
  (inc counter))

(defn- execute-task [data [pred-id clause-number]]
  (send process print-process pred-id clause-number)
  (let [clause-id (conj pred-id clause-number)
        clause (utils/get-clause pred-id clause-number data)
        pre-spec (get-pre-spec data pred-id)]
    (analyze-clause data clause-id clause pre-spec)))

(defn- build-tasks [data]
  (let [res (for [pred-id (utils/get-pred-identities data)
                  clause-number (utils/get-clause-identities-of-pred pred-id data)]
              [pred-id clause-number])
        all (count res)]
    (reset! total all)
    res))

(defn complete-analysis [data]
  (send process (fn [x] 1))
  (let [tasks (build-tasks data)]
    (pmap (partial execute-task data) tasks)))
