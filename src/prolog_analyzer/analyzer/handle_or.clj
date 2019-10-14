(ns prolog-analyzer.analyzer.handle-or
  (:require [prolog-analyzer.record-utils :as ru]
            [prolog-analyzer.records :as r]
            [prolog-analyzer.utils :as utils]
            [prolog-analyzer.parser.parser :as parser]
            [ubergraph.core :as uber]
            [prolog-analyzer.analyzer.calculate-env :as calc]))

(def data (parser/process-edn "edns/test.edn"))

(-> data
    :preds
    (get ["tmp" "goo" 1])
    (get 0))

(def env (-> (uber/digraph)
             (utils/set-title ["foo" "tmp" 1 0])))

(def or (-> data
            :preds
            (get ["tmp" "foo" 1])
            (get 0)
            :body
            first))

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
  (if (or
       (zero? arity)
       (= :not goal))
    env
    (let [pred-id [module goal arity]
          pre-spec (get-pre-spec data pred-id)
          post-specs (get-post-specs data pred-id)
          e (calc/get-env-for-subgoal env pred-id arglist pre-spec post-specs)]
      e)))

(defn bla [data env body]
  (reduce
   (partial subgoal-analyzer data)
   env
   body))

(defn yo [{bodies :arglist :as or-pred} data env]
  (for [x bodies]
    (bla data env x)))

(for [x (yo or data env)]
  (utils/get-dom-of-term x (assoc (r/->VarTerm "X") :singleton? false)))
