(ns prolog-analyzer.utils
  "Contains usefull utility functions used across different namespaces."
  (:require [ubergraph.core :as uber]
            [clojure.tools.logging :as log]
            [ubergraph.protocols]
            [loom.graph]
            [loom.attr]
            ))

;; for data extracted from a prolog file

(defn self-calling? [[pred-id clause-number] data]
  (get-in data [:preds pred-id clause-number :self-calling?]))

(defn get-specs-of-pred
  "Returns the pre, post and invariant specs of a given `pred-identity` loaded in `data`."
  [[module pred-name arity :as pred-identity] data]
  (-> data
      (select-keys [:pre-specs :post-specs :inv-specs])
      (update :pre-specs #(get % pred-identity))
      (update :post-specs #(get % pred-identity))
      (update :inv-specs #(get % pred-identity))
      ))

(defn get-pred-identities
  [data]
  (keys (:preds data)))

(defn get-clause-identities-of-pred
  "Returns the clause ids of the predicate with id `pred-id` loaded in `data`."
  [pred-id data]
  (keys (get-in data [:preds pred-id])))


(defn get-clause-identities
  "Returns the clause ids of all clauses loaded in `data`."
  [data]
  (for [p (get-pred-identities data)
        c (get-clause-identities-of-pred p data)]
    [p c]))

(defn get-clause
  "Gets the actual code of a clause in `data` with id `clause-id`."
  [pred-id clause-number data]
  (get-in data [:preds pred-id clause-number]))

(defn get-clauses-of-pred
  "Gets the code of all clauses belonging to the predicate with `pred-identity` loaded in `data`."
  [pred-identity data]
  (vals (get-in data [:preds pred-identity])))


(defn get-terms [env]
  (remove #{:ENVIRONMENT} (uber/nodes env)))

(defn get-user-defined-specs [env]
  (uber/attr env :ENVIRONMENT :user-defined-specs))

(defn get-dom-of-term [env term default]
  (if (uber/has-node? env term)
    (or (uber/attr env term :dom) default)
    default))

(defn get-clause-number [env]
  (uber/attr env :ENVIRONMENT :clause-number))

(defn get-pred-id [env]
  (uber/attr env :ENVIRONMENT :pred-id))

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


(defn recursive-check-condition [l msg]
  (if (map? l)
    (cond
      (contains? l :type) (do (assert (:type l) msg)
                              (recursive-check-condition (:type l) msg))
      (contains? l :arglist) (do (assert (:arglist l) msg)
                               ;  (assert (not (empty? (:arglist l))) (str "1 " msg))
                                 (recursive-check-condition (:arglist l) msg))
      :else (assert l msg))
    (when (not-empty l)
      (assert (first l) msg)
      (recursive-check-condition (first l) msg)
      (recursive-check-condition (rest l) msg))))
