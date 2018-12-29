(ns prolog-analyzer.pre-processor
  (:require [prolog-analyzer.utils :as utils]))

;; Set correct module when the module is set on "self"
(defn- get-correct-goal-module [source-module goal-name data goal-module]
  (if (= "self" goal-module)
    (if (contains? (get-in data [:preds source-module]) goal-name)
      source-module
      :built-in)
    goal-module
    ))

(defn- set-correct-goal-module [source-module data {goal-name :goal arity :arity goal-module :module :as goal}]
  (update goal :module (partial get-correct-goal-module source-module goal-name data)))

(defn- set-correct-modules [data]
  (loop [clause-keys (utils/get-clause-identities data)
         result data]
    (if (empty? clause-keys)
      result
      (let [[source-module & _ :as clause-key] (first clause-keys)
            body-key (apply vector :preds (conj clause-key :body))]
        (recur (rest clause-keys) (update-in result body-key (partial map (partial set-correct-goal-module source-module data)))))
   )))

;; If there are no pre-specs for a predicate, add one
(defn- add-any-pre-specs [data]
  (loop [pred-ids (utils/get-pred-identities data)
         result data]
    (if-let [[_ pred-name arity :as pred-id] (first pred-ids)]
      (if (nil? (:pre-specs (utils/get-specs-of-pred pred-id data)))
        (recur (rest pred-ids) (assoc-in result [:pre-specs pred-name arity] (list (repeat arity {:spec :any}))))
        (recur (rest pred-ids) result))
      result)))

;;mark self-calling clauses
(defn- mark-self-calling-clause [[_ pred-name arity _] {body :body :as clause}]
  (if (some #(and (= pred-name (:goal %)) (= arity (:arity %))) body)
    (assoc clause :self-calling? true)
    (assoc clause :self-calling? false)))

(defn mark-self-calling-clauses [data]
  (loop [clause-ids (utils/get-clause-identities data)
         result data]
    (if-let [clause-id (first clause-ids)]
      (recur (rest clause-ids) (update-in result (apply vector :preds clause-id) (partial mark-self-calling-clause clause-id)))
      result)))

(defn pre-process [data]
  (-> data
      set-correct-modules
      add-any-pre-specs
      mark-self-calling-clauses
      ))
