(ns prolog-analyzer.utils
  "Contains usefull utility functions used across different namespaces."
  (:require [orchestra.core :refer [defn-spec]]
            [orchestra.spec.test :as stest]
            [prolog-analyzer.records :as r]
            [prolog-analyzer.specs :as specs]
            [ubergraph.core :as uber]))

;; for data extracted from a prolog file

(defn self-calling? [[pred-id clause-number] data]
  (get-in data [:preds pred-id clause-number :self-calling?]))

(defn- default-spec [n]
  (repeat n (r/->AnySpec)))

(defn get-pre-specs [pred-identity data]
  (get (:pre-specs data) pred-identity))

(defn get-post-specs [pred-identity data]
  (get (:post-specs data) pred-identity))

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


(defn-spec get-terms ::specs/arglist
  [env ::specs/env]
  (remove #(= % :environment) (uber/nodes env)))

(defn get-active-post-specs [env]
  (uber/attr env :environment :post-specs))

(defn get-dom-of-term
  ([env term default] (get-dom-of-term env term))
  ([env term]
   (if (uber/has-node? env term)
     (or (uber/attr env term :dom) (r/->AnySpec))
     (r/->AnySpec))))

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
        ~(remodel-cases cases v1 v2)))))

(defn env->map
  "Mostly for test purpose"
  [env]
  (let [nodes (for [term (get-terms env)
                    :let [dom (get-dom-of-term env term nil)]]
                [(r/to-string term) (r/to-string dom)])
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
  (and
   (= (get-terms env1) (get-terms env2))
   (every? (partial same-dom? env1 env2) (get-terms env1))))


(defn set-title [env title]
  (-> env
      (uber/add-nodes :environment)
      (uber/add-attr :environment :title title)))

(defn get-title [env]
  (if (uber/has-node? env :environment)
    (uber/attr env :environment :title)
    (throw (Exception. "No title found"))))

(defn set-arguments [env arglist]
  (-> env
      (uber/add-nodes :environment)
      (uber/add-attr :environment :arglist arglist)))

(defn get-arguments [env]
  (if (uber/has-node? env :environment)
    (uber/attr env :environment :arglist)
    []))


(defmulti format-log (fn [a & args] (type a)))

(defmethod format-log ubergraph.core.Ubergraph [env & msgs]
  (apply format-log (get-title env) msgs))

(defmethod format-log :default [title & msgs]
  (apply str title " - " msgs))


(defn is-graph? [env]
  (= ubergraph.core.Ubergraph (type env)))


(defn error-dom? [env term]
  (= r/ERROR (r/spec-type (get-dom-of-term env term (r/->AnySpec)))))


(defn errors [env]
  (let [error-map (->> env
                       get-terms
                       (filter (partial error-dom? env))
                       (map #(hash-map % (get-dom-of-term env %)))
                       (apply merge))]
    (if (nil? error-map)
      {}
      {:errors {(get-title env) error-map}})))
