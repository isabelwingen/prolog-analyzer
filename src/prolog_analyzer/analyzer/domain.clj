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

(defn WRONG-TYPE [term spec]
  (log/error "term" term "cannot be of type " spec)
  {:spec :error :reason (str "Cannot be of spec " spec)})

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




(defn fill-env-for-term-with-spec2 [env term spec]
  (case (:spec spec)
    :any env
    :ground env
    :nonvar env
    :var env
    :exact (fill-env-for-term-with-spec-exact env term spec)
    :atomic (fill-env-for-term-with-spec-atomic env term spec)
    :atom (fill-env-for-term-with-spec-atom env term spec)
    :number (fill-env-for-term-with-spec-number env term spec)
    :float (fill-env-for-term-with-spec-float env term spec)
    :integer (fill-env-for-term-with-spec-integer env term spec)
    :list env
    :tuple env
    :compound env
    :specvar env
    :one-of env
    :and env
    :user-defined env
    (do
      (log/error "I don't know what to do with" spec)
      env))
  )


(defn special-case-var [env term var-spec]
  (if (uber/has-node? env term)
    (if (every? #{:var :any :specvar} (map :spec (uber/attr env term :dom))) ;;TODO: should :user-defined be added to this list?
      (add-doms-to-node env term var-spec)
      (add-doms-to-node env term {:spec :error :reason "Term is already nonvar"}))
    (uber/add-nodes-with-attrs env [term {:dom (list var-spec)}])))

(defn fill-env-for-term-with-spec [env term spec]
  (if (= :var (:spec spec))
    (special-case-var env term spec)
    (if (or (= :var (:type term)) (= :anon_var (:type term)))
      (add-doms-to-node env term spec)
      (fill-env-for-term-with-spec* [term spec env]))))

(defn fill-env-for-terms-with-specs [env terms specs]
  (reduce #(apply fill-env-for-term-with-spec %1 %2) env (map vector terms specs)))

(defmethod fill-env-for-term-with-spec* [:atomic :list] [[term spec env]]
  (if (utils/empty-list? term)
    (add-doms-to-node env term spec)
    (add-doms-to-node env term {:spec :error :reason "atomic cannot be a list"})
    ))

(defmethod fill-env-for-term-with-spec* [:atomic :tuple] [[term {arglist :arglist :as spec} env]]
  (if (utils/empty-list? term)
    (if (empty? arglist)
      (add-doms-to-node env term spec)
      (add-doms-to-node env term {:spec :error :reason "empty list cannot be a tuple with non-empty arglist"}))
    (add-doms-to-node env term {:spec :error :reason "atomic cannot be a tuple"})))

(defmethod fill-env-for-term-with-spec* [:atomic :ground] [[term _ env]]
  (if (utils/empty-list? term)
    (add-doms-to-node env term {:spec :list :type {:spec :ground}})
    (add-doms-to-node env term {:spec :atomic})))

(defmethod fill-env-for-term-with-spec* [:atomic :nonvar] [[term _ env]]
  (if (utils/empty-list? term)
    (add-doms-to-node env term {:spec :list :type {:spec :any}})
    (add-doms-to-node env term {:spec :atomic})))

(defmethod fill-env-for-term-with-spec* [:atomic :any] [[term _ env]]
  (if (utils/empty-list? term)
    (add-doms-to-node env term {:spec :list :type {:spec :any}})
    (add-doms-to-node env term {:spec :atomic})))

(defmethod fill-env-for-term-with-spec* [:atomic :one-of] [[term spec env]]
  (add-doms-to-node env term (remove-invalid-or-parts term spec env)))

(defmethod fill-env-for-term-with-spec* [:atomic :and] [[term {arglist :arglist} env]]
  (reduce #(fill-env-for-term-with-spec %1 term %2) env arglist))

(defmethod fill-env-for-term-with-spec* [:atomic :user-defined] [[term {n :name arglist :arglist :as spec} env]]
  (-> env
      (add-doms-to-node term spec)
      (fill-env-for-term-with-spec term (resolve-definition-with-parameters spec env))))

(defmethod fill-env-for-term-with-spec* [:list :list] [[{head :head tail :tail :as term} {t :type :as spec} env]]
  (let [tail-env (if (utils/empty-list? tail) env (fill-env-for-term-with-spec env tail spec))]
    (-> tail-env
        (add-doms-to-node term spec)
        (fill-env-for-term-with-spec head t))))

(defmethod fill-env-for-term-with-spec* [:list :tuple] [[{head :head tail :tail :as term} {[head-type & rest-types :as r] :arglist :as spec} env]]
  (let [tail-env (case [(utils/empty-list? tail) (empty? rest-types)]
                   [true true] env
                   [true false] (add-doms-to-node env tail {:spec :error :reason (str "empty list cannot be a tuple with non-empty arglist")})
                   [false true] (add-doms-to-node env term {:spec :error :reason "non-empty list cannot be a tuple with no elements"})
                   [false false] (fill-env-for-term-with-spec env tail (update spec :arglist rest)))]
    (-> tail-env
        (fill-env-for-term-with-spec head head-type)
        (add-doms-to-node term spec)))
  )

(defmethod fill-env-for-term-with-spec* [:list :ground] [[{head :head tail :tail :as term} ground-spec env]]
  (let [tail-env (if (utils/empty-list? tail) env (fill-env-for-term-with-spec env tail {:spec :list :type ground-spec}))]
    (-> tail-env
        (fill-env-for-term-with-spec head ground-spec)
        (add-doms-to-node term {:spec :list :type ground-spec})))
  )

(defmethod fill-env-for-term-with-spec* [:list :nonvar] [[{head :head tail :tail :as term} nonvar-spec env]]
  (let [tail-env (if (utils/empty-list? tail) env (fill-env-for-term-with-spec env tail {:spec :list :type nonvar-spec}))]
    (-> tail-env
        (add-doms-to-node term {:spec :list :type {:type :any}})
        (fill-env-for-term-with-spec head {:spec :any}))))

(defmethod fill-env-for-term-with-spec* [:list :any] [[{head :head tail :tail :as term} any-spec env]]
  (let [tail-env (if (utils/empty-list? tail) env (fill-env-for-term-with-spec env tail {:spec :list :type any-spec}))]
    (-> tail-env
        (add-doms-to-node term {:spec :list :type {:spec :any}})
        (fill-env-for-term-with-spec head any-spec))))

(defmethod fill-env-for-term-with-spec* [:list :one-of] [[{tail :tail :as term} spec env]]
  (let [simplified-or (remove-invalid-or-parts term spec env)]
    (if (= :one-of (:spec simplified-or))
      (add-doms-to-node env term simplified-or)
      (fill-env-for-term-with-spec env term simplified-or))))

(defmethod fill-env-for-term-with-spec* [:list :and] [[term {speclist :arglist} env]]
  (reduce #(fill-env-for-term-with-spec %1 term %2) env speclist))

(defmethod fill-env-for-term-with-spec* [:list :user-defined] [[term spec env]]
  (-> env
      (add-doms-to-node term spec)
      (fill-env-for-term-with-spec term (resolve-definition-with-parameters spec env))))

(defmethod fill-env-for-term-with-spec* [:compound :compound] [[{term-func :functor term-elems :arglist :as term} {spec-func :functor spec-elems :arglist :as spec} env]]
  (cond
    (not= term-func spec-func) (add-doms-to-node env term {:spec :error :reason "functor not identical"})
    (not= (count term-elems) (count spec-elems)) (add-doms-to-node env term {:spec :error :reason "length of argument list not identical"})
    :else (let [pairs (map vector term-elems spec-elems)]
            (reduce #(apply fill-env-for-term-with-spec %1 %2) (add-doms-to-node env term spec) pairs))))

(defmethod fill-env-for-term-with-spec* [:compound :ground] [[{functor :functor arglist :arglist :as term} spec env]]
  (let [term-env (add-doms-to-node env term {:spec :compound :functor functor :arglist (repeat (count arglist) {:spec :ground})})]
    (reduce #(apply fill-env-for-term-with-spec %1 %2) term-env (map vector arglist (repeat spec)))))

(defmethod fill-env-for-term-with-spec* [:compound :nonvar] [[{functor :functor arglist :arglist :as term} _ env]]
  (let [term-env (add-doms-to-node env term {:spec :compound :functor functor :arglist (repeat (count arglist) {:spec :any})})]
    (reduce #(apply fill-env-for-term-with-spec %1 %2) term-env (map vector arglist (repeat {:spec :any})))))

(defmethod fill-env-for-term-with-spec* [:compound :any] [[{functor :functor arglist :arglist :as term} spec env]]
  (let [term-env (add-doms-to-node env term {:spec :compound :functor functor :arglist (repeat (count arglist) {:spec :any})})]
    (reduce #(apply fill-env-for-term-with-spec %1 %2) term-env (map vector arglist (repeat {:spec :any})))))

(defmethod fill-env-for-term-with-spec* [:compound :one-of] [[term spec env]]
  (let [simplified-or (remove-invalid-or-parts term spec env)]
    (if (= :one-of (:spec simplified-or))
      (add-doms-to-node env term simplified-or)
      (fill-env-for-term-with-spec env term simplified-or)))
  )

(defmethod fill-env-for-term-with-spec* [:compound :and] [[term {speclist :arglist} env]]
  (reduce #(fill-env-for-term-with-spec %1 term %2) env speclist))
 
(defmethod fill-env-for-term-with-spec* [:compound :user-defined] [[term spec env]]
  (-> env
      (add-doms-to-node term spec)
      (fill-env-for-term-with-spec term (resolve-definition-with-parameters spec env))))

(defmethod fill-env-for-term-with-spec* :default [[term spec env]]
  (if (= :user-defined (:spec spec))
    (-> env
        (add-doms-to-node term spec)
        (fill-env-for-term-with-spec term (resolve-definition-with-parameters spec env)))
    (case (:type term)
      :list (add-doms-to-node env term {:spec :error :reason (str "list cannot be of type " spec)})
      :compound (add-doms-to-node env term {:spec :error :reason (str "compound cannot be of type " spec)})
      (add-doms-to-node env term {:spec :and :arglist [{:spec (:type term)} spec]}))))

(defn- remove-invalid-or-parts [term {speclist :arglist :as or-spec} env]
  (let [env-attrs (uber/attrs env :ENVIRONMENT)
        simplified-or (->> speclist
                           (map #(fill-env-for-term-with-spec (-> (uber/digraph) (uber/add-nodes-with-attrs [:ENVIRONMENT env-attrs])) term %))
                           (map-indexed #(if (utils/valid-env? %2) %1 nil))
                           (filter #(not= nil %))
                           (map #(nth speclist %))
                           (apply vector)
                           (hash-map :spec :one-of :arglist)
                           )]
    (case (count (:arglist simplified-or))
      0 {:spec :error :reason "No valid or component"}
      1 (first (:arglist simplified-or))
      simplified-or)))
