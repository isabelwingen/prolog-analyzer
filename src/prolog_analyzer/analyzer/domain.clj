(ns prolog-analyzer.analyzer.domain
  (:require [prolog-analyzer.utils :as utils :refer [case+]]
            [prolog-analyzer.records :as r :refer [suitable-spec]]
            [ubergraph.core :as uber]
            [clojure.tools.logging :as log]
            [loom.graph]
            [loom.attr]
            [ubergraph.protocols]
            ))

(defn- get-definition-of-alias [env user-defined-alias]
  (get (uber/attr env :ENVIRONMENT :user-defined-specs) user-defined-alias))

(defn- resolve-definition-with-parameters
  "User-defined specs can have parameters and when in use in spec annotations,
  there are values assigned to these parameters. To get the correct definition,
  we have to replace the parameter with their value."
  [{n :name arglist :arglist :as user-def-spec} env]
  (if (nil? arglist)
    (get-definition-of-alias env user-def-spec)
    (let [alias (->> (uber/attr env :ENVIRONMENT :user-defined-specs)
                     keys
                     (filter #(= (:name %) n))
                     (filter #(= (count (:arglist %)) (count arglist)))
                     first)
          definition (get-definition-of-alias env alias)
          replace-map (apply hash-map (interleave (map :name (:arglist alias)) arglist))]
      (reduce-kv r/replace-specvars-with-spec definition replace-map))))


(declare remove-invalid-or-parts)

(defn add-doms-to-node [env node & doms]
  (if (uber/has-node? env node)
    (uber/set-attrs env node (-> (uber/attrs env node)
                                 (update :dom #(concat % doms))
                                 (update :dom distinct)))
    (uber/add-nodes-with-attrs env [node {:dom doms}])))

(defn mark-as-was-var [env term]
  (let [attrs (uber/attrs env term)]
    (uber/set-attrs env term (assoc attrs :was-var true))))

(declare fill-env-for-term-with-spec)

(defn WRONG-TYPE
  ([]
   (log/error "Wrong type associated")
   (r/make-spec:error "Wrong type associated"))
  ([term spec]
   (log/error "term" (r/to-string term) "cannot be of type " (r/to-string spec))
   (r/make-spec:error (str "Term " (r/to-string term) " cannot be of spec " (r/to-string spec)))))

(defn ALREADY-NONVAR []
  (r/make-spec:error (str "Term cannot be var, because its already nonvar")))

(defn fill-env-for-term-with-spec-list [env term {t :type :as spec}]
  (log/debug "Fill env for term" (r/to-string term) "and spec" (r/to-string spec))
  (if-let [suitable-spec (r/suitable-spec spec term)]
    (if (= r/LIST (r/term-type term))
      (-> (if (r/empty-list? (:tail term)) env (fill-env-for-term-with-spec env (:tail term) spec))
          (add-doms-to-node term suitable-spec)
          (fill-env-for-term-with-spec (:head term) t))
      (add-doms-to-node env term suitable-spec))
    (add-doms-to-node env term (WRONG-TYPE term spec))))

(defn fill-env-for-term-with-spec-tuple [env term {[head-type & rest-types :as arglist] :arglist :as spec}]
  (log/debug "Fill env for term" (r/to-string term) "and spec" (r/to-string spec))
  (if-let [suitable-spec (r/suitable-spec spec term)]
    (if (= r/LIST (r/term-type term))
      (-> (if (r/empty-list? (:tail term)) env (fill-env-for-term-with-spec env (:tail term) (update spec :arglist rest)))
          (fill-env-for-term-with-spec (:head term) head-type)
          (add-doms-to-node term suitable-spec))
      (add-doms-to-node env term suitable-spec))
    (add-doms-to-node env term (WRONG-TYPE term spec))))


(defn fill-env-for-term-with-spec-compound [env term {spec-fun :functor spec-args :arglist :as spec}]
  (log/debug "Fill env for term" (r/to-string term) "and spec" (r/to-string spec))
  (if-let [suitable-spec (r/suitable-spec spec term)]
    (if (= r/COMPOUND (r/term-type term))
      (let [{term-fun :functor term-args :arglist} term
            pairs (map vector term-args (:arglist spec))]
        (reduce #(apply fill-env-for-term-with-spec %1 %2)
                (add-doms-to-node env term suitable-spec)
                pairs))
      (add-doms-to-node env term suitable-spec))
    (add-doms-to-node env term (WRONG-TYPE term spec))))

(defn fill-env-for-term-with-spec-any [env term spec]
  (log/debug "Fill env for term" (r/to-string term) "and spec" (r/to-string spec))
  (if-let [suitable-spec (r/suitable-spec spec term)]
    (case+ (r/term-type term)
           (:var, :any, :ground, :nonvar, :atom, :atomic, :integer, :float, :number) (add-doms-to-node env term suitable-spec)
           (:list, :compound) (fill-env-for-term-with-spec env term suitable-spec)
           (add-doms-to-node env term (WRONG-TYPE term spec)))
    (add-doms-to-node env term (WRONG-TYPE term spec))))

(defn fill-env-for-term-with-spec-ground [env term spec]
  (log/debug "Fill env for term" (r/to-string term) "and spec" (r/to-string spec))
  (if-let [suitable-spec (r/suitable-spec spec term)]
    (case+ (r/term-type term)
           (:var, :any, :ground, :nonvar, :atom, :atomic, :integer, :float, :number) (add-doms-to-node env term suitable-spec)
           (:compound, :list) (fill-env-for-term-with-spec env term suitable-spec)
           (add-doms-to-node env term (WRONG-TYPE term spec)))
    (add-doms-to-node env term (WRONG-TYPE term spec))))

(defn fill-env-for-term-with-spec-nonvar [env term spec]
  (log/debug "Fill env for term" (r/to-string term) "and spec" (r/to-string spec))
  (if-let [suitable-spec (r/suitable-spec spec term)]
    (case+ (r/term-type term)
           (:var, :any, :ground, :nonvar, :atom, :atomic, :integer, :float, :number) (add-doms-to-node env term suitable-spec)
           (:list, :compound) (fill-env-for-term-with-spec env term suitable-spec)
           (add-doms-to-node env term (WRONG-TYPE term spec)))
    (add-doms-to-node env term (WRONG-TYPE term spec))))

(defn fill-env-for-term-with-spec-var [env term spec]
  (log/debug "Fill env for term" (r/to-string term) "and spec" (r/to-string spec))
  (if (empty? (utils/get-dom-of-term env term)) ;; if there is no initial domain
    (-> env
        (fill-env-for-term-with-spec term (r/make-spec:any))
        (mark-as-was-var term))
    (if (contains? #{:var :anon_var :any} (r/term-type term))
      (if (every?
           #{:var :any :specvar}
           (map r/spec-type (utils/get-dom-of-term env term)))
        (add-doms-to-node env term spec)
        (add-doms-to-node env term (ALREADY-NONVAR)))
      (add-doms-to-node env term (ALREADY-NONVAR)))))


(defn fill-env-for-term-with-spec-specvar [env term spec]
  (log/debug "Fill env for term" (r/to-string term) "and spec" (r/to-string spec))
  (let [suitable-spec (r/suitable-spec spec term)
        term-env (if (= r/AND (r/spec-type suitable-spec))
                   (apply add-doms-to-node env term (:arglist suitable-spec))
                   (add-doms-to-node env term suitable-spec))]
    (apply add-doms-to-node term-env spec (remove #{spec} (utils/get-dom-of-term term-env term)))
    ))

(defn fill-env-for-term-with-spec-one-of [env term spec]
  (log/debug "Fill env for term" (r/to-string term) "and spec" (r/to-string spec))
  (let [modified-or (remove-invalid-or-parts term spec)]
    (if (= r/OR (r/spec-type modified-or))
      (add-doms-to-node env term modified-or)
      (fill-env-for-term-with-spec env term modified-or))))

(defn fill-env-for-term-with-spec-and [env term spec]
  (log/debug "Fill env for term" (r/to-string term) "and spec" (r/to-string spec))
  (if-let [suitable-spec (r/suitable-spec spec term)]
    (reduce #(fill-env-for-term-with-spec %1 term %2) env (:arglist spec))
    (add-doms-to-node env term (WRONG-TYPE term spec))))

(defn fill-env-for-term-with-spec-user-defined [env term spec]
  (let [transformed-definition (resolve-definition-with-parameters spec env)]
    (-> env
        (add-doms-to-node term spec)
        (fill-env-for-term-with-spec term transformed-definition))))

(defn multiple-fills [env terms specs]
  (reduce #(apply fill-env-for-term-with-spec %1 %2) env (map vector terms specs)))

(defn fill-env-for-term-with-spec-simple [env term spec]
  (log/debug "Fill env for term" (r/to-string term) "and spec" (r/to-string spec))
  (if-let [suitable-spec (r/suitable-spec spec term)]
    (add-doms-to-node env term suitable-spec)
    (add-doms-to-node env term (WRONG-TYPE term spec))))

(defn fill-env-for-term-with-spec [env term spec]
  (case+ (r/spec-type spec)
    r/ANY (fill-env-for-term-with-spec-any env term spec)
    r/GROUND (fill-env-for-term-with-spec-ground env term spec)
    r/NONVAR (fill-env-for-term-with-spec-nonvar env term spec)
    r/VAR (fill-env-for-term-with-spec-var env term spec)
    r/LIST (fill-env-for-term-with-spec-list env term spec)
    r/TUPLE (fill-env-for-term-with-spec-tuple env term spec)
    r/COMPOUND (fill-env-for-term-with-spec-compound env term spec)
    r/SPECVAR (fill-env-for-term-with-spec-specvar env term spec)
    r/OR (fill-env-for-term-with-spec-one-of env term spec)
    r/AND (fill-env-for-term-with-spec-and env term spec)
    r/USERDEFINED (fill-env-for-term-with-spec-user-defined env term spec)
    (r/ATOMIC, r/ATOM, r/NUMBER, r/FLOAT, r/INTEGER, r/EXACT) (fill-env-for-term-with-spec-simple env term spec)))

(defn- simplify-and [term {speclist :arglist :as or-spec}]
  (let [simplified-and (->> speclist
                            (map #(r/suitable-spec % term))
                            (remove nil?)
                            (distinct)
                            (apply vector)
                            r/make-spec:and)]
    (case (count (:arglist simplified-and))
      0 (r/make-spec:error "No valid and component")
      1 (first (:arglist simplified-and))
      simplified-and)))


(defn- remove-invalid-or-parts [term {speclist :arglist :as or-spec}]
  (let [simplified-or (->> speclist 
                           (map #(r/suitable-spec % term))
                           (remove nil?)
                           (distinct)
                           (apply vector)
                           r/make-spec:one-of)]
    (case (count (:arglist simplified-or))
      0 (r/make-spec:error "No valid or component")
      1 (first (:arglist simplified-or))
      simplified-or)))
