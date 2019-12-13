(ns prolog-analyzer.parser.pre-processor
  (:require [clojure.spec.alpha :as s]
            [clojure.tools.logging :as log]
            [prolog-analyzer.parser.add-userdefs :as add-userdefs]
            [prolog-analyzer.parser.create-missing-annotations :as anno]
            [prolog-analyzer.parser.transform-to-records :as transform]
            [orchestra.core :refer [defn-spec]]
            [prolog-analyzer.records :as r]
            [prolog-analyzer.specs :as specs]
            [prolog-analyzer.utils :as utils]
            [orchestra.spec.test :as stest]))


(defn- set-correct-goal-module [pred->module-map {goal-name :goal arity :arity goal-module :module :as goal}]
  (if (= "self" goal-module)
    (assoc goal :module (get pred->module-map goal-name "user"))
    goal))

(defn-spec get-complete-imported (s/coll-of string?)
  [module-map (s/map-of string? (s/or :all keyword? :list seq?))]
  (->> module-map
       (filter #(= :all (second %)))
       (map first)
       ))

(defn-spec switch-map (s/map-of keyword? keyword?)
  [in (s/map-of keyword? (s/coll-of keyword?))]
  (reduce-kv (fn [m k v] (reduce #(assoc %1 %2 k) m v)) {} in))

(defn-spec intersect set?
  [a seq? b seq?]
  (clojure.set/intersection (set a) (set b)))


(defn-spec calculate-pred-to-module-map (s/map-of ::specs/predicate-name ::specs/module)
  [module->pred (s/map-of ::specs/module ::specs/predicate-name)
   caller-module ::specs/module
   imports (s/map-of ::specs/module (s/map-of ::specs/module ::specs/predicate-name))]
  (let [used-modules (-> imports
                         (get caller-module)
                         (assoc "user" :all)
                         (assoc "lists" :all)
                         (assoc caller-module :all))
        completely-imported (get-complete-imported used-modules)
        other-imports (apply dissoc used-modules completely-imported)
        weak-mappings (-> module->pred
                          (select-keys completely-imported)
                          switch-map)
        strong-mappings (->> (select-keys module->pred (keys other-imports))
                             (merge-with intersect other-imports)
                             switch-map)
        ]
    (merge weak-mappings strong-mappings)))


(defn-spec collect-predicates (s/coll-of ::specs/pred-id)
  [data map?]
  (->> (select-keys data [:pre-specs :post-specs :preds])
       (vals)
       (mapcat keys)
       set))

(defn-spec create-module->pred (s/map-of ::specs/module ::specs/pred-id)
  [predicates (s/coll-of ::specs/pred-id)]
  (reduce (fn [res [module name arity]] (update res module #(conj % name))) {} predicates))


(defn-spec calculate-pred-to-module-maps (s/map-of ::specs/module (s/map-of ::specs/predicate-name ::specs/module))
  [data map?]
  (let [predicates (collect-predicates data)
        modules (->> predicates
                     (map first)
                     set)
        module->pred (create-module->pred predicates)]
    (apply merge (for [m modules]
                   {m (calculate-pred-to-module-map module->pred m (:imports data))}))))

(defn- set-correct-module-in-body [pred->module-maps data [[source-module & _ :as pred-id] clause-number]]
  (update-in data [:preds pred-id clause-number :body] (partial map (partial set-correct-goal-module (get pred->module-maps source-module)))))

(defn set-correct-modules [data]
  (let [pred->module-maps (calculate-pred-to-module-maps data)]
    (reduce
     (partial set-correct-module-in-body pred->module-maps)
     data
     (utils/get-clause-identities data))))


(defn transform-singleton-lists [data]
  (reduce (fn [d [pred-id clause-number]] (update-in d [:singletons pred-id clause-number] #(apply vector (map r/map-to-term %)))) data (utils/get-clause-identities data)))

(defn- mark-self-calling-clause
  "Marks clauses that are calling themselves"
  [[_ pred-name arity] {body :body :as clause}]
  (if (some #(and (= pred-name (:goal %)) (= arity (:arity %))) body)
    (assoc clause :self-calling? true)
    (assoc clause :self-calling? false)))

(defn mark-self-calling-clauses [data]
  (loop [clause-ids (utils/get-clause-identities data)
         result data]
    (if-let [[pred-id clause-number] (first clause-ids)]
      (recur (rest clause-ids) (update-in result [:preds pred-id clause-number] (partial mark-self-calling-clause pred-id)))
      result)))

(defn remove-not-needed-stuff [data]
  (-> data
      (dissoc :specs)
      (dissoc :module)
      (dissoc :imports)
      ))


(s/fdef pre-process-single
    :args (s/cat :data map?)
    :ret ::specs/data)

(defn pre-process-single [data]
  (def sd data)
  (log/debug "Start Pre Process Single")
  (let [p (-> data
              mark-self-calling-clauses
              transform-singleton-lists
              transform/transform-args-to-term-records
              anno/start
              set-correct-modules
              add-userdefs/do-it
              remove-not-needed-stuff
              )]
    (log/debug "Done Pre Process Single")
    p))
