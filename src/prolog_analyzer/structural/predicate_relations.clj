(ns prolog-analyzer.structural.predicate-relations
  (:require [prolog-analyzer.utils :as utils]
            [ubergraph.core :as uber]
            [clojure.pprint :refer [pprint]]))
  
(def graph (atom (uber/multidigraph)))

(defn add-predicates-to-graph2 [data]
  (doseq [pred-id (utils/get-pred-identities data)]
    (->> (keys (get-in (:preds data) pred-id))
         (map (partial conj pred-id))
         (map-indexed #(vector pred-id %2 {:index %1}))
         (apply swap! graph uber/add-edges))))

(defn add-predicates-to-graph [data]
     (doseq [pred-id (utils/get-pred-identities data)
             clause-key (keys (get-in (:preds data) pred-id))]
       (let [src pred-id
             dest (conj pred-id clause-key)]
         (swap! graph uber/add-edges [src dest])
          ))
     @graph)

(defn add-clauses-to-graph [data]
  (doseq [[source-module & _ :as clause-key] (utils/get-clause-identities data)]
    (->> (get-in (:preds data) (conj clause-key :body))
         (map (juxt :module :goal :arity))
         (map-indexed #(vector clause-key %2 {:index %1}))
         (apply swap! graph uber/add-edges))))

(defn create-graph [data]
  (reset! graph (uber/multidigraph))
  (add-predicates-to-graph2 data)
  (add-clauses-to-graph data)
  @graph
  )

