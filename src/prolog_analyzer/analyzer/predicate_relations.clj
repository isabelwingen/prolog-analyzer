(ns prolog-analyzer.analyzer.predicate-relations
  (:require [prolog-analyzer.parser :refer [process-prolog-file process-prolog-snippets]]
            [prolog-analyzer.analyzer.core :refer [data]]
            [prolog-analyzer.utils :as utils]
            [clojure.pprint :refer [pprint]]))

(def graph (atom {}))

(defn add-predicates-to-graph []
  (doseq [pred-id (utils/get-pred-identities @data)
          clause-key (keys (get-in (:preds @data) pred-id))]
    (if (contains? @graph pred-id)
      (swap! graph update pred-id #(conj % (conj pred-id clause-key)))
      (swap! graph assoc pred-id [(conj pred-id clause-key)])))
  @graph)

(defn add-clauses-to-graph []
  (doseq [[source-module & _ :as clause-key] (utils/get-clause-identities @data)
          {goal-name :goal arity :arity goal-module :module :as goal} (get-in (:preds @data) (conj clause-key :body))]
    (let [goal-pred [goal-module goal-name arity]]
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
