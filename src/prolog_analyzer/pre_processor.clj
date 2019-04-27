(ns prolog-analyzer.pre-processor
  (:require [prolog-analyzer.utils :as utils :refer [case+]]
            [prolog-analyzer.records :as r]
            [clojure.tools.logging :as log]
            ))


(defn- pred->module-map [data]
  (reduce (fn [m [module pred _]] (assoc m pred module)) {} (keys (:preds data))))

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
      (let [[[source-module & _ :as pred-id] clause-number] (first clause-keys)]
        (recur (rest clause-keys) (update-in result [:preds pred-id clause-number :body] (partial map (partial set-correct-goal-module source-module data)))))
   )))

(defn- maybe-spec [spec]
  (cond
    (:arglist spec) (r/->OneOfSpec #{(r/->VarSpec) (update spec :arglist (partial map (fn [x] (r/->AnySpec))))})
    (:type spec) (r/->OneOfSpec #{(r/->VarSpec) (assoc spec :type (r/->AnySpec))})
    :else (if (r/any-spec? spec) spec (r/->OneOfSpec #{(r/->VarSpec) spec}))))

(defn- create-pre-spec [pred-id data]
  (->> data
       (utils/get-clauses-of-pred pred-id)
       (map :arglist)
       (map (partial map (comp maybe-spec r/initial-spec)))
       (map (partial apply vector))
       (apply vector)
       ))


(defn- create-post-spec [[_ _ arity :as pred-id] data]
  (->> data
       (utils/get-clauses-of-pred pred-id)
       (map :arglist)
       (map (partial map r/initial-spec))
       (map (partial apply vector))
       (map (partial vector (apply vector (repeatedly arity r/->AnySpec))))
       (apply vector)
       ))

;; If there are no pre-specs for a predicate, add one
(defn- add-any-pre-specs [data]
  (loop [pred-ids (utils/get-pred-identities data)
         result data]
    (if-let [[module pred-name arity :as pred-id] (first pred-ids)]
      (if (nil? (:pre-specs (utils/get-specs-of-pred pred-id data)))
        (recur (rest pred-ids) (assoc-in result [:pre-specs [module pred-name arity]] (create-pre-spec pred-id data)))
        (recur (rest pred-ids) result))
      result)))

(defn- add-any-post-specs [data]
  (loop [pred-ids (utils/get-pred-identities data)
         result data]
    (if-let [[module pred-name arity :as pred-id] (first pred-ids)]
      (if (nil? (:post-specs (utils/get-specs-of-pred pred-id data)))
        (recur (rest pred-ids) (assoc-in result [:post-specs [module pred-name arity]] (create-post-spec pred-id data)))
        (recur (rest pred-ids) result))
      result)))


;;mark self-calling clauses
(defn- mark-self-calling-clause [[_ pred-name arity] {body :body :as clause}]
  (if (some #(and (= pred-name (:goal %)) (= arity (:arity %))) body)
    (assoc clause :self-calling? true)
    (assoc clause :self-calling? false)))

(defn mark-self-calling-clauses [data]
  (loop [clause-ids (utils/get-clause-identities data)
         result data]
    (if-let [[pred-id clause-number] (first clause-ids)]
      (recur (rest clause-ids) (update-in result [:preds pred-id clause-number] (partial mark-self-calling-clause pred-id)))
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
  (reduce (fn [data [pred-id clause-number]]
            (-> data
                (update-in [:preds pred-id clause-number :arglist] transform-arglist)
                (update-in [:preds pred-id clause-number :body] transform-body))
            )
          data
          (utils/get-clause-identities data)))

(defn pre-process-single [data]
  (log/debug "Start Pre Process Single")
  (-> data
      mark-self-calling-clauses
      transform-args-to-term-records
      add-any-pre-specs
      add-any-post-specs
      set-correct-modules
      ))
