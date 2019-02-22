(ns prolog-analyzer.analyzer.domain
  (:require [prolog-analyzer.utils :as utils :refer [case+]]
            [prolog-analyzer.records :as r]
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
      (reduce-kv utils/replace-specvars-with-spec definition replace-map))))


(declare remove-invalid-or-parts)

(defmulti fill-env-for-term-with-spec* (juxt (comp :type first) (comp :spec second)))

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
   (log/error "term" term "cannot be of type " spec)
   (r/make-spec:error (str "Term " term " cannot be of spec " spec))))

(defn ALREADY-NONVAR []
  (r/make-spec:error (str "Term cannot be var, because its already nonvar")))

(defn fill-env-for-term-with-spec-list [env term {t :type :as spec}]
  (log/debug "Fill env for term" term "and spec" spec)
  (case (:type term)
    (:var :anon_var) (add-doms-to-node env term spec)
    :list (let [{tail :tail head :head} term
                tail-env (if (r/empty-list? tail) env (fill-env-for-term-with-spec env tail spec))]
            (-> tail-env
                (add-doms-to-node term spec)
                (fill-env-for-term-with-spec head t)))
    :atomic (if (= "[]" (:term term)) (add-doms-to-node env term spec) (add-doms-to-node env term (WRONG-TYPE term spec)))
    (add-doms-to-node env term (WRONG-TYPE term spec)))
  )

(defn fill-env-for-term-with-spec-tuple [env term {[head-type & rest-types :as arglist] :arglist :as spec}]
  (log/debug "Fill env for term" term "and spec" spec)
  (case (:type term)
    (:var :anon_var) (add-doms-to-node env term spec)
    :list (let [{tail :tail head :head} term
                tail-env (case [(r/empty-list? tail) (empty? rest-types)]
                           [true true] env
                           ([true false], [false, true]) (add-doms-to-node env tail (WRONG-TYPE term spec))
                           [false false] (fill-env-for-term-with-spec env tail (update spec :arglist rest)))]
            (-> tail-env
                (fill-env-for-term-with-spec head head-type)
                (add-doms-to-node term spec)))
    :atomic (if (and (= "[]" (:term term)) (empty? arglist)) (add-doms-to-node env term spec) (add-doms-to-node env term (WRONG-TYPE term spec)))
    (add-doms-to-node env term (WRONG-TYPE term spec)))
  )

(defn fill-env-for-term-with-spec-compound [env term {spec-fun :functor spec-args :arglist :as spec}]
  (log/debug "Fill env for term" term "and spec" spec)
  (case (:type term)
    (:var :anon_var) (add-doms-to-node env term spec)
    :compound (let [{term-fun :functor term-args :arglist} term
                    pairs (map vector term-args spec-args)]
                (if (and (= term-fun spec-fun) (= (count term-args) (count spec-args)))
                  (reduce #(apply fill-env-for-term-with-spec %1 %2) (add-doms-to-node env term spec) pairs)
                  (add-doms-to-node env term (WRONG-TYPE term spec))))
    (add-doms-to-node env term (WRONG-TYPE term spec))))

(defn fill-env-for-term-with-spec-any [env term spec]
  (log/debug "Fill env for term" term "and spec" spec)
  (case (:type term)
    (:var :anon_var) (add-doms-to-node env term (r/make-spec:var))
    :any (add-doms-to-node env term (r/make-spec:any))
    :ground (add-doms-to-node env term (r/make-spec:ground))
    :nonvar (add-doms-to-node env term (r/make-spec:nonvar))
    :atom (add-doms-to-node env term (r/make-spec:atom))
    :atomic (add-doms-to-node env term (r/make-spec:atomic))
    :integer (add-doms-to-node env term (r/make-spec:integer))
    :float (add-doms-to-node env term (r/make-spec:float))
    :number (add-doms-to-node env term (r/make-spec:number))
    :list (fill-env-for-term-with-spec env term (r/make-spec:list (r/make-spec:any)))
    :compound (fill-env-for-term-with-spec env term (r/make-spec:compound (:functor term) (repeat (count (:arglist term)) (r/make-spec:any))))
    (add-doms-to-node env term (WRONG-TYPE term spec))))

(defn fill-env-for-term-with-spec-ground [env term spec]
  (log/debug "Fill env for term" term "and spec" spec)
  (case (:type term)
    (:var :anon_var) (add-doms-to-node env term (r/make-spec:ground))
    :any (add-doms-to-node env term (r/make-spec:ground))
    :ground (add-doms-to-node env term (r/make-spec:ground))
    :nonvar (add-doms-to-node env term (r/make-spec:ground))
    :atom (add-doms-to-node env term (r/make-spec:atom))
    :atomic (add-doms-to-node env term (r/make-spec:atomic))
    :integer (add-doms-to-node env term (r/make-spec:integer))
    :float (add-doms-to-node env term (r/make-spec:float))
    :number (add-doms-to-node env term (r/make-spec:number))
    :list (fill-env-for-term-with-spec env term (r/make-spec:list (r/make-spec:ground)))
    :compound (fill-env-for-term-with-spec env term (r/make-spec:compound (:functor term) (repeat (count (:arglist term)) (r/make-spec:ground))))
    (add-doms-to-node env term (WRONG-TYPE term spec))))

(defn fill-env-for-term-with-spec-nonvar [env term spec]
  (log/debug "Fill env for term" term "and spec" spec)
  (case (:type term)
    (:var :anon_var) (add-doms-to-node env term (r/make-spec:nonvar))
    :any (add-doms-to-node env term (r/make-spec:nonvar))
    :ground (add-doms-to-node env term (r/make-spec:ground))
    :nonvar (add-doms-to-node env term (r/make-spec:nonvar))
    :atom (add-doms-to-node env term (r/make-spec:atom))
    :atomic (add-doms-to-node env term (r/make-spec:atomic))
    :integer (add-doms-to-node env term (r/make-spec:integer))
    :float (add-doms-to-node env term (r/make-spec:float))
    :number (add-doms-to-node env term (r/make-spec:number))
    :list (fill-env-for-term-with-spec env term (r/make-spec:list (r/make-spec:any)))
    :compound (fill-env-for-term-with-spec env term (r/make-spec:compound (:functor term) (repeat (count (:arglist term)) (r/make-spec:any))))
    (add-doms-to-node env term (WRONG-TYPE term spec))))

(defn fill-env-for-term-with-spec-var [env term spec]
  (log/debug "Fill env for term" term "and spec" spec)
  (if (empty? (utils/get-dom-of-term env term))
    (-> env
        (fill-env-for-term-with-spec term (r/make-spec:any))
        (mark-as-was-var term))
    (if (contains? #{:var :anon_var :any} (:type term))
      (if (every?
           #{:var :any :specvar}
           (map :spec (utils/get-dom-of-term env term)))
        (add-doms-to-node env term spec)
        (add-doms-to-node env term (ALREADY-NONVAR)))
      (add-doms-to-node env term (ALREADY-NONVAR)))))


(defn fill-env-for-term-with-spec-specvar [env term spec]
  (log/debug "Fill env for term" term "and spec" spec)
  (let [term-env (case (:type term)
                   (:ground, :nonvar, :atom, :atomic, :integer, :float, :number) (add-doms-to-node env term spec (r/map-to-spec {:spec (:type term)}))
                   (:var, :anon_var) (add-doms-to-node env term spec (r/make-spec:var))
                   :any (add-doms-to-node env term spec (r/make-spec:any))
                   :list (add-doms-to-node env term spec (r/make-spec:list (r/make-spec:any)))
                   :compound (add-doms-to-node env term spec (r/make-spec:compound (:functor term) (repeat (count (:arglist term)) (r/make-spec:any))))
                   (add-doms-to-node env term (WRONG-TYPE term spec)))]
    (apply add-doms-to-node term-env spec (remove #{spec} (utils/get-dom-of-term term-env term)))
    ))

(defn fill-env-for-term-with-spec-one-of [env term spec]
  (log/debug "Fill env for term" term "and spec" spec)
  (let [modified-or (remove-invalid-or-parts term spec env)]
    (if (= :one-of (:spec modified-or))
      (add-doms-to-node env term modified-or)
      (fill-env-for-term-with-spec env term modified-or))))

(defn fill-env-for-term-with-spec-and [env term spec]
  (log/debug "Fill env for term" term "and spec" spec)
  (if (contains? #{:ground, :nonvar, :atomic, :atom, :number, :integer, :float, :any, :var, :anon_var :compound :list} (:type term))
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
    :any (fill-env-for-term-with-spec-any env term spec)
    :ground (fill-env-for-term-with-spec-ground env term spec)
    :nonvar (fill-env-for-term-with-spec-nonvar env term spec)
    :var (fill-env-for-term-with-spec-var env term spec)
    :list (fill-env-for-term-with-spec-list env term spec)
    :tuple (fill-env-for-term-with-spec-tuple env term spec)
    :compound (fill-env-for-term-with-spec-compound env term spec)
    :specvar (fill-env-for-term-with-spec-specvar env term spec)
    :one-of (fill-env-for-term-with-spec-one-of env term spec)
    :and (fill-env-for-term-with-spec-and env term spec)
    :user-defined (fill-env-for-term-with-spec-user-defined env term spec)
    (r/ATOMIC, r/ATOM, r/NUMBER, r/FLOAT, r/INTEGER, r/EXACT) (fill-env-for-term-with-spec-simple env term spec)))

(defn- simplify-and [term {speclist :arglist :as or-spec} env]
  (let [env-attrs (uber/attrs env :ENVIRONMENT)
        empty-env (-> (uber/digraph) (uber/add-nodes-with-attrs [:ENVIRONMENT env-attrs]))
        simplified-and (->> speclist
                           (map (partial fill-env-for-term-with-spec empty-env term))
                           (filter utils/valid-env?)
                           (mapcat #(utils/get-dom-of-term % term))
                           (distinct)
                           (apply vector)
                           r/make-spec:and)]
    (case (count (:arglist simplified-and))
      0 (r/make-spec:error "No valid or component")
      1 (first (:arglist simplified-and))
      simplified-and)))


(defn- remove-invalid-or-parts [term {speclist :arglist :as or-spec} env]
  (let [env-attrs (uber/attrs env :ENVIRONMENT)
        empty-env (-> (uber/digraph) (uber/add-nodes-with-attrs [:ENVIRONMENT env-attrs]))
        simplified-or (->> speclist
                           (map (partial fill-env-for-term-with-spec empty-env term))
                           (filter utils/valid-env?)
                           (map #(utils/get-dom-of-term % term))
                           (map distinct)
                           (map (partial apply vector))
                           (map r/make-spec:and)
                           (map #(simplify-and term % empty-env))
                           (distinct)
                           (apply vector)
                           r/make-spec:one-of)]
    (case (count (:arglist simplified-or))
      0 (r/make-spec:error "No valid and component")
      1 (first (:arglist simplified-or))
      simplified-or)))
