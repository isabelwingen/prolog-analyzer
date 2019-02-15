(ns prolog-analyzer.analyzer.domain
  (:require [prolog-analyzer.utils :as utils]
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

(declare fill-env-for-term-with-spec)

(defn WRONG-TYPE
  ([]
   (log/error "Wrong type associated")
   {:spec :error :reason "Wrong type associated"})
  ([term spec]
   (log/error "term" term "cannot be of type " spec)
   {:spec :error :reason (str "Term " term " cannot be of spec " spec)}))

(defn ALREADY-NONVAR []
  {:spec :error :reason (str "Term cannot be var, because its already nonvar")})

(defn fill-env-for-term-with-spec-integer [env term spec]
  (log/debug "Fill env for term" term "and spec" spec)
  (add-doms-to-node env term (case (:type term)
                               (:integer, :var :anon_var) spec
                               :number (if (int? (:value term)) spec (WRONG-TYPE term spec))
                               :atomic (if (int? (read-string (:term term))) spec (WRONG-TYPE term spec))
                               (WRONG-TYPE term spec))))

(defn fill-env-for-term-with-spec-float [env term spec]
  (log/debug "Fill env for term" term "and spec" spec)
  (add-doms-to-node env term (case (:type term)
                               (:float, :var :anon_var) spec
                               :number (if (float? (:value term)) spec (WRONG-TYPE term spec))
                               :atomic (if (float? (read-string (:term term))) spec (WRONG-TYPE term spec))
                               (WRONG-TYPE term spec))))

(defn fill-env-for-term-with-spec-number [env term spec]
  (log/debug "Fill env for term" term "and spec" spec)
  (add-doms-to-node env term (case (:type term)
                               (:number, :var :anon_var) spec
                               :integer {:spec :integer}
                               :float {:spec :float}
                               :atomic (if (number? (read-string (:term term))) spec (WRONG-TYPE term spec))
                               (WRONG-TYPE term spec))))

(defn fill-env-for-term-with-spec-atom [env term spec]
  (log/debug "Fill env for term" term "and spec" spec)
  (add-doms-to-node env term (case (:type term)
                               (:atom, :var :anon_var) spec
                               :atomic (if (and (not= "[]" (:term term)) ((complement number?) (read-string (:term term)))) spec (WRONG-TYPE term spec))
                               (WRONG-TYPE term spec))))

(defn fill-env-for-term-with-spec-atomic [env term spec]
  (log/debug "Fill env for term" term "and spec" spec)
  (add-doms-to-node env term (case (:type term)
                               (:atomic, :var :anon_var) spec
                               :atom {:spec :atom}
                               :number {:spec :number}
                               :integer {:spec :integer}
                               :float {:spec :float}
                               (WRONG-TYPE term spec))))

(defn fill-env-for-term-with-spec-exact [env term {value :value :as spec}]
  (log/debug "Fill env for term" term "and spec" spec)
  (add-doms-to-node env term (case (:type term)
                               (:var :anon_var) spec
                               (:atomic, :atom) (if (= value (:term term)) spec (WRONG-TYPE term spec))
                               (WRONG-TYPE term spec))))

(defn fill-env-for-term-with-spec-list [env term {t :type :as spec}]
  (log/debug "Fill env for term" term "and spec" spec)
  (case (:type term)
    (:var :anon_var) (add-doms-to-node env term spec)
    :list (let [{tail :tail head :head} term
                tail-env (if (utils/empty-list? tail) env (fill-env-for-term-with-spec env tail spec))]
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
                tail-env (case [(utils/empty-list? tail) (empty? rest-types)]
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
    (:var :anon_var) (add-doms-to-node env term {:spec :var})
    :any (add-doms-to-node env term {:spec :any})
    :ground (add-doms-to-node env term {:spec :ground})
    :nonvar (add-doms-to-node env term {:spec :nonvar})
    :atom (add-doms-to-node env term {:spec :atom})
    :atomic (add-doms-to-node env term {:spec :atomic})
    :integer (add-doms-to-node env term {:spec :integer})
    :float (add-doms-to-node env term {:spec :float})
    :number (add-doms-to-node env term {:spec :number})
    :list (fill-env-for-term-with-spec env term {:spec :list :type {:spec :any}})
    :compound (fill-env-for-term-with-spec env term {:spec :compound :functor (:functor term) :arglist (repeat (count (:arglist term)) {:spec :any})})
    (add-doms-to-node env term (WRONG-TYPE term spec))))

(defn fill-env-for-term-with-spec-ground [env term spec]
  (log/debug "Fill env for term" term "and spec" spec)
  (case (:type term)
    (:var :anon_var) (add-doms-to-node env term {:spec :ground})
    :any (add-doms-to-node env term {:spec :ground})
    :ground (add-doms-to-node env term {:spec :ground})
    :nonvar (add-doms-to-node env term {:spec :ground})
    :atom (add-doms-to-node env term {:spec :atom})
    :atomic (add-doms-to-node env term {:spec :atomic})
    :integer (add-doms-to-node env term {:spec :integer})
    :float (add-doms-to-node env term {:spec :float})
    :number (add-doms-to-node env term {:spec :number})
    :list (fill-env-for-term-with-spec env term {:spec :list :type {:spec :ground}})
    :compound (fill-env-for-term-with-spec env term {:spec :compound :functor (:functor term) :arglist (repeat (count (:arglist term)) {:spec :ground})})
    (add-doms-to-node env term (WRONG-TYPE term spec))))

(defn fill-env-for-term-with-spec-nonvar [env term spec]
  (log/debug "Fill env for term" term "and spec" spec)
  (case (:type term)
    (:var :anon_var) (add-doms-to-node env term {:spec :nonvar})
    :any (add-doms-to-node env term {:spec :nonvar})
    :ground (add-doms-to-node env term {:spec :ground})
    :nonvar (add-doms-to-node env term {:spec :nonvar})
    :atom (add-doms-to-node env term {:spec :atom})
    :atomic (add-doms-to-node env term {:spec :atomic})
    :integer (add-doms-to-node env term {:spec :integer})
    :float (add-doms-to-node env term {:spec :float})
    :number (add-doms-to-node env term {:spec :number})
    :list (fill-env-for-term-with-spec env term {:spec :list :type {:spec :any}})
    :compound (fill-env-for-term-with-spec env term {:spec :compound :functor (:functor term) :arglist (repeat (count (:arglist term)) {:spec :any})})
    (add-doms-to-node env term (WRONG-TYPE term spec))))

(defn fill-env-for-term-with-spec-var [env term spec]
  (log/debug "Fill env for term" term "and spec" spec)
  (case (:type term)
    (:var, :anon_var, :any) (if (uber/has-node? env term)
                              (if (every? #{:var :any :specvar} (map :spec (uber/attr env term :dom))) ;;TODO: should :user-defined be added to this list?
                                (add-doms-to-node env term spec)
                                (add-doms-to-node env term (ALREADY-NONVAR)))
                              (add-doms-to-node env term spec ))
    :ground (add-doms-to-node env term (ALREADY-NONVAR))
    :nonvar (add-doms-to-node env term (ALREADY-NONVAR))
    :atom (add-doms-to-node env term (ALREADY-NONVAR))
    :atomic (add-doms-to-node env term (ALREADY-NONVAR))
    :integer (add-doms-to-node env term (ALREADY-NONVAR))
    :float (add-doms-to-node env term (ALREADY-NONVAR))
    :number (add-doms-to-node env term (ALREADY-NONVAR))
    :list (add-doms-to-node env term (ALREADY-NONVAR))
    :compound (add-doms-to-node env term (ALREADY-NONVAR))
    (add-doms-to-node env term (WRONG-TYPE term spec))))

(defn fill-env-for-term-with-spec-specvar [env term spec]
  (log/debug "Fill env for term" term "and spec" spec)
  (let [term-env (case (:type term)
                   (:ground, :nonvar, :atom, :atomic, :integer, :float, :number) (add-doms-to-node env term spec {:spec (:type term)})
                   (:var, :anon_var) (add-doms-to-node env term spec {:spec :var})
                   :any (add-doms-to-node env term spec {:spec :any})
                   :list (add-doms-to-node env term spec {:spec :list :type {:spec :any}})
                   :compound (add-doms-to-node env term spec {:spec :compound :functor (:functor term) :arglist (repeat (count (:arglist term)) {:spec :any})})
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

(defn fill-env-for-term-with-spec [env term spec]
  (let [func (case (:spec spec)
             :any fill-env-for-term-with-spec-any
             :ground fill-env-for-term-with-spec-ground
             :nonvar fill-env-for-term-with-spec-nonvar
             :var fill-env-for-term-with-spec-var
             :atomic fill-env-for-term-with-spec-atomic
             :atom fill-env-for-term-with-spec-atom
             :exact fill-env-for-term-with-spec-exact
             :number fill-env-for-term-with-spec-number
             :float fill-env-for-term-with-spec-float
             :integer fill-env-for-term-with-spec-integer
             :list fill-env-for-term-with-spec-list
             :tuple fill-env-for-term-with-spec-tuple
             :compound fill-env-for-term-with-spec-compound
             :specvar fill-env-for-term-with-spec-specvar
             :one-of fill-env-for-term-with-spec-one-of
             :and fill-env-for-term-with-spec-and
             :user-defined fill-env-for-term-with-spec-user-defined
             (do
               (log/error "I don't know what to do with" spec)
               identity))]
    (func env term spec))
  )
(defn- simplify-and [term {speclist :arglist :as or-spec} env]
  (let [env-attrs (uber/attrs env :ENVIRONMENT)
        empty-env (-> (uber/digraph) (uber/add-nodes-with-attrs [:ENVIRONMENT env-attrs]))
        simplified-and (->> speclist
                           (map (partial fill-env-for-term-with-spec empty-env term))
                           (filter utils/valid-env?)
                           (mapcat #(utils/get-dom-of-term % term))
                           (distinct)
                           (apply vector)
                           (hash-map :spec :and :arglist))]
    (case (count (:arglist simplified-and))
      0 {:spec :error :reason "No valid or component"}
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
                           (map (partial hash-map :spec :and :arglist))
                           (map #(simplify-and term % empty-env))
                           (distinct)
                           (apply vector)
                           (hash-map :spec :one-of :arglist))]
    (case (count (:arglist simplified-or))
      0 {:spec :error :reason "No valid or component"}
      1 (first (:arglist simplified-or))
      simplified-or)))
