(ns prolog-analyzer.analyzer.domain
  (:require [prolog-analyzer.utils :as utils :refer [case+]]
            [prolog-analyzer.records :as r :refer [intersect]]
            [ubergraph.core :as uber]
            [clojure.tools.logging :as log]
            [loom.graph]
            [loom.attr]
            [ubergraph.protocols]
            ))

(defn- var-domain [env term]
  (let [dom-types (map r/spec-type (utils/get-dom-of-term env term))]
    (every? #{r/VAR r/ANY} dom-types)))

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


(declare fill-env-for-term-with-spec)

(defn add-doms-to-node [env node & doms]
  (let [mod-doms (->> doms
                      (map #(if (= r/AND (r/spec-type %)) (:arglist %) %))
                      flatten)]
    (if (uber/has-node? env node)
      (uber/set-attrs env node (-> (uber/attrs env node)
                                   (update :dom #(concat % mod-doms))
                                   (update :dom distinct)))
      (uber/add-nodes-with-attrs env [node {:dom mod-doms}]))))

(defn mark-as-was-var [env term]
  (let [attrs (uber/attrs env term)]
    (uber/set-attrs env term (assoc attrs :was-var true))))

(defn WRONG-TYPE
  ([]
   (log/error "Wrong type associated")
   (r/->ErrorSpec "Wrong type associated"))
  ([term spec]
   (log/error "term" (r/to-string term) "cannot be of type " (r/to-string spec))
   (r/->ErrorSpec (str "Term " (r/to-string term) " cannot be of spec " (r/to-string spec)))))

(defn ALREADY-NONVAR []
  (r/->ErrorSpec (str "Term cannot be var, because its already nonvar")))


(defmulti fill-env (fn [env term spec initial?] (r/spec-type spec)))

(defmethod fill-env r/ANY [env term spec initial?]
  (log/info (str :any))
  (if (empty? (utils/get-dom-of-term env term))
    (add-doms-to-node env term (r/initial-spec term))
    env))

(defmethod fill-env r/SPECVAR [env term spec initial?]
  (log/info (str :specvar :term:var))
  (let [step1 (fill-env-for-term-with-spec initial? term (r/initial-spec term))
        step2 (apply add-doms-to-node step1 spec (utils/get-dom-of-term step1 term))
        step3 (uber/add-edges step2 [term spec {:relation :specvar}])]
    step3))

(defmethod fill-env r/USERDEFINED [env term spec initial?]
  (log/info (str :user-defined :term:nonvar))
  (let [transformed-definition (resolve-definition-with-parameters spec env)]
    (-> env
        (add-doms-to-node term spec)
        (fill-env-for-term-with-spec initial? term transformed-definition))))


(defmulti check-if-valid (fn [term spec] [(r/term-type term) (r/spec-type spec)]))

(defmethod check-if-valid [r/ATOM r/EXACT] [term spec]
  (= (:term term) (:value spec)))

(defmethod check-if-valid [r/NUMBER r/INTEGER] [term spec]
  (int? (:value term)))

(defmethod check-if-valid [r/NUMBER r/FLOAT] [term spec]
  (float? (:value term)))

(defmethod check-if-valid :default [term spec]
  true)

(defmethod fill-env :default [env term spec initial?]
  (log/info :default)
  (let [suitable-spec (if (and initial? (= r/VAR (r/spec-type spec))) (r/initial-spec term) (r/intersect spec (r/initial-spec term)))
        next-steps (r/next-steps spec term)]
    (if (r/error-spec? suitable-spec)
      (add-doms-to-node env term (WRONG-TYPE term spec))
      (if (check-if-valid term spec)
        (reduce #(apply fill-env-for-term-with-spec %1 initial? %2) (add-doms-to-node env term suitable-spec) (partition 2 next-steps))
        (add-doms-to-node env term (WRONG-TYPE term spec))))))

(defn fill-env-for-var [env term spec initial?]
  (if initial?
    (add-doms-to-node env term spec)
    (if (contains? #{r/VAR, r/ANY} (r/spec-type spec))
      (add-doms-to-node env term spec)
      (if (every? #{r/VAR, r/ANY} (utils/get-dom-of-term env term))
        (add-doms-to-node env term (ALREADY-NONVAR))
        (fill-env env term spec initial?)))
    ))



(defn fill-env-for-term-with-spec
  ([env initial? term spec]
   (log/debug "Fill env for term" (r/to-string term) "and spec" (r/to-string spec))
   (if (contains? #{r/VAR, r/ANY} (r/term-type term))
     (fill-env-for-var env term spec initial?)
     (fill-env env term spec initial?)))
  ([env term spec]
   (fill-env-for-term-with-spec env false term spec)))

(defn multiple-fills
  ([env initial? terms specs]
   (reduce #(apply fill-env-for-term-with-spec %1 initial? %2) env (map vector terms specs)))
  ([env terms specs]
   (multiple-fills env false terms specs)))
