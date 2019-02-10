(ns prolog-analyzer.utils
  "Contains usefull utility functions used across different namespaces."
  (:require [prolog-analyzer.analyzer.built-in-specs :as built-ins]
            [ubergraph.core :as uber]
            [ubergraph.protocols]
            [loom.graph]
            [loom.attr]
            ))

;; for data extracted from a prolog file

(defn get-specs-of-pred
  "Returns the pre, post and invariant specs of a given `pred-identity` loaded in `data`."
  [[module pred-name arity :as pred-identity] data]
  (if (= :built-in module)
    (built-ins/get-specs-of-built-in-pred pred-name arity)
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

(defn empty-list?
  "Checks if the input is the empty (prolog) list."
  [{term :term type :type}]
  (and (= type :atomic) (= term "[]")))

(defn to-head-tail-list
  "Transforms a bunch of `terms` to a proper prolog list."
  [& terms]
  (if (empty? terms)
    {:term "[]" :type :atomic}
    {:type :list :head (first terms) :tail (apply to-head-tail-list (rest terms))}))

(defn to-tuple-spec
  "Transforms a bunch of `specs` to a tuple spec."
  [& specs]
  (if (empty? specs)
    {:spec :error :reason "Cannot build a tuple with zero arguments"}
    {:spec :tuple :arglist specs}))

(defn to-or-spec
  "Transforms a bunch of `specs` to a one-of spec."
  [& specs]
  (case (count specs)
    0 {:spec :error :reason "Cannot build empty one-of"}
    1 (first specs)
    {:spec :one-of :arglist specs}))

(defn get-elements-of-list [{head :head tail :tail}]
  (if (empty-list? tail)
    (list head)
    (conj (get-elements-of-list tail) head)))

(defn- merge-attr [attr1 attr2]
  (let [dom1 (:dom attr1)
        dom2 (:dom attr2)]
    (assoc (merge attr1 attr2)
           :dom
           (apply vector (concat dom1 dom2)))))

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
       (filter #(= :specvar (:spec %)))
       distinct))
