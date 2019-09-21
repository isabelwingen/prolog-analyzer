(ns prolog-analyzer.analyzer.recursive-types
  (:require [prolog-analyzer.parser.parser :refer [process-edn]]
            [ubergraph.alg :as uberalg]
            [clojure.math.combinatorics :as combo]
            [ubergraph.core :as uber]))

(def d (process-edn "edns/test.edn"))
;(def prob (process-edn "edns/prob_cli.edn"))

(defn to-pred-id [{module :module goal :goal arity :arity}]
  [module goal arity])

(defn get-subgoals [{body :body}]
  (map to-pred-id body))


(defn call-graph [data]
  (->> (for [[k v] (:preds data)]
         {k (->> v vals (mapcat get-subgoals) set)})
       (apply merge))
  (->> (for [[k v] (:preds data)]
         (for [x (->> v vals (mapcat get-subgoals) (remove #{["user" "true" 0]}) set)]
           [k x]))
       (mapcat identity)
       (apply uber/add-edges (uber/digraph))))

(defn scc [data]
  (let [graph (call-graph data)
        sccs (uberalg/scc graph)
        combos (map (partial mapcat identity) (combo/combinations sccs (dec (count sccs))))
        graphs (for [x combos]
                 (apply uber/remove-nodes graph x))]
    sccs))

(reduce #() data (scc d))

(:preds d)
:-
