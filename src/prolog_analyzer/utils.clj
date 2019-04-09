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
  [data]
  (let [modules (keys (:preds data))]
    (reduce-kv
     (fn [m1 k1 v1]
       (apply conj m1 (reduce-kv (fn [m2 k2 v2] (apply conj m2 (map #(vector k1 k2 %) (keys v2)))) [] v1)))
     []
     (:preds data))))


(defn get-clause-identities
  "Returns the clause ids of all clauses loaded in `data`."
  [data]
  (let [pred-ids (get-pred-identities data)
        preds (:preds data)]
    (->> pred-ids
         (reduce-kv (fn [m _ pred-id] (assoc m pred-id (keys (get-in preds pred-id)))) {})
         (reduce-kv (fn [m key value] (reduce #(conj %1 (conj (apply vector key) %2)) m value)) []))))

(defn get-clause-identities-of-pred
  "Returns the clause ids of the predicate with id `pred-id` loaded in `data`."
  [pred-id data]
  (->> pred-id
       (get-in (:preds data))
       keys
       (map #(conj pred-id %))))


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

(defn get-user-defined-specs [env]
  (uber/attr env :ENVIRONMENT :user-defined-specs))

(defn get-dom-of-term [env term]
  (if (uber/has-node? env term)
    (uber/attr env term :dom)
    nil))

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

(defn get-elements-of-list [{head :head tail :tail}]
  (if (nil? tail)
    (list)
    (conj (get-elements-of-list tail) head)))
