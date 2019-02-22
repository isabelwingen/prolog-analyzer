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
  (if (= :built-in module)
    (-> data
        (select-keys [:pre-specs :post-specs :inv-specs])
        (update :pre-specs #(get-in % pred-identity))
        (update :post-specs #(get-in % pred-identity))
        (update :inv-specs #(get-in % pred-identity))
        )))

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

(defn replace-specvar-name-with-value [spec specvar-name replace-value]
  (case (:spec spec)
    :specvar
    (if (= specvar-name (:name spec)) (assoc spec :name replace-value) spec)

    (:user-defined, :one-of, :and, :compound, :tuple)
    (update spec :arglist (fn [s] (seq (map #(replace-specvar-name-with-value % specvar-name replace-value) s))))

    :list
    (update spec :type #(replace-specvar-name-with-value % specvar-name replace-value))

    spec
    ))

(defn replace-specvars-with-spec [spec specvar-name replace-spec]
  (case (:spec spec)
    :specvar
    (if (= specvar-name (:name spec)) replace-spec spec)

    (:user-defined, :one-of, :and, :compound, :tuple)
    (update spec :arglist (fn [s] (seq (map #(replace-specvars-with-spec % specvar-name replace-spec) s))))

    :list
    (update spec :type #(replace-specvars-with-spec % specvar-name replace-spec))

    spec
    ))

(defn find-specvars [spec]
  (case (:spec spec)
    :specvar [spec]
    (:user-defined, :one-of, :and, :compound, :tuple) (distinct (reduce concat (map find-specvars (:arglist spec))))
    :list (find-specvars (:type spec))
    []))

(defn get-terms [env]
  (remove #{:ENVIRONMENT} (uber/nodes env)))

(defn get-all-specvars-in-doms [env]
  (->> (get-terms env)
       (mapcat #(uber/attr env % :dom))
       (mapcat find-specvars)
       distinct))

(defn valid-env?
  "Checks, if a env is valid and contains no errors."
  [env]
  (every? #(not= (:spec %) :error) (mapcat #(uber/attr env % :dom) (get-terms env))))

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
