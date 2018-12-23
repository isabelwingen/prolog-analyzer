(ns prolog-analyzer.analyzer.predicate-relations
  (:require [prolog-analyzer.parser :refer [process-prolog-file process-prolog-snippets]]
            [prolog-analyzer.analyzer.core :refer [data]]
            [clojure.pprint :refer [pprint]]))

(def graph (atom {}))

(defn get-list-of-pred-keys []
  (let [preds (:preds @data)]
    (for [module (keys preds)
          pred (keys (get-in preds [module]))
          arity (keys (get-in preds [module pred]))]
      [module pred arity])))

(defn get-list-of-clause-keys []
  (let [preds (:preds @data)]
    (for [pred-key (get-list-of-pred-keys)
          clause (keys (get-in preds pred-key))]
      (conj pred-key clause))))

(defn- mark-self-calling-clause [[module pred-name arity :as pred-id] {body :body :as clause}]
  (if (some #(and (= pred-name (:goal %)) (= arity (:arity %))) body)
    (assoc clause :self-calling? true)
    (assoc clause :self-calling? false)))

(defn mark-self-calling-predicates []
  (for [pred-id (get-list-of-pred-keys)
        clause-key (keys (get-in (:preds @data) pred-id))]
    (swap!
     data
     update-in
     (apply vector :preds (conj pred-id clause-key))
     (partial mark-self-calling-clause pred-id))))

(defn pred-key? [key]
  (= 3 (count key)))
(defn clause-key? [key]
  (= 4 (count key)))

(defn add-predicates-to-graph []
  (doseq [pred-id (get-list-of-pred-keys)
          clause-key (keys (get-in (:preds @data) pred-id))]
    (if (contains? @graph pred-id)
      (swap! graph update pred-id #(conj % (conj pred-id clause-key)))
      (swap! graph assoc pred-id [(conj pred-id clause-key)])))
  @graph)



(defn- correct-module [source-module goal-name goal-module]
  (if (= "self" goal-module)
    (if (contains? (get-in @data [:preds source-module]) goal-name)
      source-module
      :built-in)
    goal-module
    ))

(defn add-clauses-to-graph []
  (doseq [[source-module & _ :as clause-key] (get-list-of-clause-keys)
          {goal-name :goal arity :arity goal-module :module :as goal} (get-in (:preds @data) (conj clause-key :body))]
    (let [corrected-module (correct-module source-module goal-name goal-module )
          goal-pred [corrected-module goal-name arity]]
      (if (contains? @graph clause-key)
        (swap! graph update clause-key #(conj % goal-pred))
        (swap! graph assoc clause-key [goal-pred])))
    )
  @graph)

(defn create-graph []
  (reset! graph {})
  (add-predicates-to-graph)
  (add-clauses-to-graph)
  )

(pprint (create-graph))
@graph
