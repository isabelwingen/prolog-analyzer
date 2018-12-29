(ns prolog-analyzer.analyzer.predicate-relations
  (:require [prolog-analyzer.parser :refer [process-prolog-file process-prolog-snippets]]
            [prolog-analyzer.analyzer.core :refer [data]]
            [prolog-analyzer.utils :as utils]
            [ubergraph.core :as uber]
            [clojure.pprint :refer [pprint]]))

(def graph (atom {}))

(defn add-predicates-to-graph []
  (doseq [pred-id (utils/get-pred-identities @data)
          clause-key (keys (get-in (:preds @data) pred-id))]
    (swap! graph uber/add-directed-edges [pred-id (conj pred-id clause-key)]))
  @graph)


(defn add-clauses-to-graph []
  (doseq [[source-module & _ :as clause-id] (utils/get-clause-identities @data)]
    (let [goals (map (juxt :module :goal :arity) (get-in (:preds @data) (conj clause-id :body)))]
      (doseq [goal-number (range 0 (count goals))]
        (swap! graph uber/add-directed-edges [clause-id (nth goals goal-number) {:position goal-number}]))))
  @graph)

(defn create-graph []
  (reset! graph (uber/digraph))
  (add-predicates-to-graph)
  (add-clauses-to-graph)
  )

(-> "resources/module1.pl" process-prolog-file prolog-analyzer.analyzer.core/complete-analysis)
(uber/pprint (create-graph))


