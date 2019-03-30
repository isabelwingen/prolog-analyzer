(ns prolog-analyzer.pre-processor
  (:require [prolog-analyzer.utils :as utils]
            [prolog-analyzer.records :as r]
            [clojure.tools.logging :as log]
            ))


(defn- pred->module-map [data]
  (reduce-kv
   (fn [m k v]
     (merge m (apply hash-map (interleave (keys v) (repeat k)))))
   {}
   (:preds data)))

(defn- get-module-of-predicate [data pred]
  (get (pred->module-map data) pred "user"))

;; Set correct module when the module is set on "self"
(defn- get-correct-goal-module [source-module goal-name data goal-module]
  (if (= "self" goal-module)
    (get-module-of-predicate data goal-name)
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
    (if-let [[module pred-name arity :as pred-id] (first pred-ids)]
      (if (nil? (:pre-specs (utils/get-specs-of-pred pred-id data)))
        (recur (rest pred-ids) (assoc-in result [:pre-specs module pred-name arity] (list (repeat arity (r/->AnySpec)))))
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

(defn- transform-arglist [args]
  (apply vector (map r/map-to-term args)))

(declare transform-body)

(defn- transform-body-elements [{goal-name :goal arglist :arglist :as goal}]
  (case goal-name
    (:or, :if) (-> goal
                   (update :arglist (partial map transform-body))
                   (assoc :module :built-in))
    (update goal :arglist transform-arglist)))

(defn- transform-body [body]
  (map transform-body-elements body))


(defn transform-args-to-term-records [data]
  (reduce (fn [data clause-id]
            (-> data
                (update-in (concat [:preds] clause-id [:arglist]) transform-arglist)
                (update-in (concat [:preds] clause-id [:body]) transform-body))
            )
          data
          (utils/get-clause-identities data)))

(defn pre-process-single [data]
  (-> data
      add-any-pre-specs
      mark-self-calling-clauses
      transform-args-to-term-records
      set-correct-modules
      ))


(defn pre-process-multiple [& data]
  (->> data
       (map add-any-pre-specs)
       (map mark-self-calling-clauses)
       (map transform-args-to-term-records)
       (map #(dissoc % :error-msg))
       (apply merge-with into)
       set-correct-modules))
