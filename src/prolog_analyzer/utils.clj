(ns prolog-analyzer.utils
  "Contains usefull utility functions used across different namespaces."
  (:require [ubergraph.core :as uber]
            [clojure.tools.logging :as log]
            [prolog-analyzer.records :as r]
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
  (let [result (-> data
                   (select-keys [:pre-specs :post-specs])
                   (update :pre-specs #(get % pred-identity))
                   (update :post-specs #(get % pred-identity))
                   )]
    (assert (not (nil? (:pre-specs data))) (str "Could not find pre-specs for " pred-identity))
    (assert (not (nil? (:post-specs data))) (str "Could not find post-specs for " pred-identity))
    result))

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
  (uber/nodes env))


(defn get-dom-of-term
  ([env term default] (get-dom-of-term env term))
  ([env term]
   (let [result (if (uber/has-node? env term)
                  (uber/attr env term :dom)
                  nil)]
     (assert (not= nil result) (str "Could not find domain of term " (r/to-string term)))
     result)))

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

(defn- remodel-cases [cases var1 var2]
  (cons 'case+
        (cons var1
              (apply concat
                     (for [[a b] (group-by ffirst (partition 2 cases))]
                       (list a
                             (cons 'case+
                                   (cons var2
                                         (apply concat
                                                (for [[[_ y] z] (sort-by (partial not= :idclol) b)]
                                                  (if (= y :idclol) (list z) (list y z))))))))))))

(defmacro duocase
  ([expr & cases]
   (let [v1 (gensym)
         v2 (gensym)]
     `(let [[~v1 ~v2] ~expr]
        ~(remodel-cases cases v1 v2)
        ))))

(defn env->map
  "Mostly for test purpose"
  [env]
  (let [nodes (for [term (get-terms env)
                    :let [dom (get-dom-of-term env term nil)]]
                [(r/to-string term) (apply vector (map r/to-string dom))])
        edges (for [edge (uber/edges env)
                    :let [s (r/to-string (uber/src edge))
                          d (r/to-string (uber/dest edge))
                          rel (uber/attr env edge :relation)]]
                [[s d] rel])]
    (apply merge {} (concat nodes edges))))


(defn update-attr
  "Updates an attribue similar to 'update'"
  [graph node key f & args]
  (let [attrs (uber/attrs graph node)]
    (uber/set-attrs graph node (apply update attrs key f args))))


(defn- same-dom? [env1 env2 term]
  (= (get-dom-of-term env1 term nil) (get-dom-of-term env2 term nil)))

(defn same? [env1 env2]
  (and (= (get-terms env1) (get-terms env2))
       (every? (partial same-dom? env1 env2) (get-terms env1))))
