(ns prolog-analyzer.utils
  "Contains usefull utility functions used across different namespaces."
  (:require [ubergraph.core :as uber]
            [clojure.tools.logging :as log]
            [ubergraph.protocols]
            [loom.graph]
            [loom.attr]
            ))

;; for data extracted from a prolog file

(defn get-specs-of-pred
  "Returns the pre, post and invariant specs of a given `pred-identity` loaded in `data`."
  [[module pred-name arity :as pred-identity] data]
  (-> data
      (select-keys [:pre-specs :post-specs :inv-specs])
      (update :pre-specs #(get-in % pred-identity))
      (update :post-specs #(get-in % pred-identity))
      (update :inv-specs #(get-in % pred-identity))
      ))

(defn get-pred-identities
  "Returns the predicate ids of all predicates loaded in `data`."
  [data]
  (for [module (keys (:preds data))
        pred-name (keys (get-in data [:preds module]))
        arity (keys (get-in data [:preds module pred-name]))]
    [module pred-name arity]))

(defn get-clause-identities
  "Returns the clause ids of all clauses loaded in `data`."
  [data]
  (let [preds (:preds data)]
    (for [pred-id (get-pred-identities data)
          clause-number (keys (get-in preds pred-id))]
      (conj pred-id clause-number))))

(defn get-clause-identities-of-pred
  "Returns the clause ids of the predicate with id `pred-id` loaded in `data`."
  [pred-id data]
  (for [clause-number (keys (get-in (:preds data) pred-id))]
    (conj pred-id clause-number)))

(defn get-clause
  "Gets the actual code of a clause in `data` with id `clause-id`."
  [clause-id data]
  (get-in (:preds data) clause-id))

(defn get-clauses-of-pred
  "Gets the code of all clauses belonging to the predicate with `pred-identity` loaded in `data`."
  [pred-identity data]
  (vals (get-in (:preds data) pred-identity)))

(defn get-terms [env]
  (remove #{:ENVIRONMENT} (uber/nodes env)))

(defn get-dom-of-term [env term]
  (if (uber/has-node? env term)
    (uber/attr env term :dom)
    (do
      (log/debug (str "Term " term " could not be found, returning empty dom"))
      [])))

(defn get-goals [data]
  (->> (get-clause-identities data)
       (map #(get-in (:preds data) %))
       (mapcat :body)))

(defmacro case+
  "Same as case, but evaluates dispatch values, needed for referring to
   class and def'ed constants as well as java.util.Enum instances.
  https://cemerick.com/2010/08/03/enhancing-clojures-case-to-evaluate-dispatch-values/"
  [value & clauses]
  (let [clauses (partition 2 2 nil clauses)
        default (when (-> clauses last count (== 1))
                  (last clauses))
        clauses (if default (drop-last clauses) clauses)
        eval-dispatch (fn [d]
                        (if (list? d)
                          (map eval d)
                          (eval d)))]
    `(case ~value
       ~@(concat (->> clauses
                      (map #(-> % first eval-dispatch (list (second %))))
                      (mapcat identity))
                 default))))
