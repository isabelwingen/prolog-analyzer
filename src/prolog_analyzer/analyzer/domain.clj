(ns prolog-analyzer.analyzer.domain
  (:require [prolog-analyzer.utils :as utils :refer [case+]]
            [prolog-analyzer.records :as r :refer [intersect]]
            [ubergraph.core :as uber]
            [clojure.tools.logging :as log]
            [loom.graph]
            [loom.attr]
            [ubergraph.protocols]
            ))
(defn- get-defs-from-env [env]
  (uber/attr env :ENVIRONMENT :user-defined-specs))

(defn- var-domain [env term]
  (let [dom-type (r/spec-type (utils/get-dom-of-term env term))]
    (contains? #{r/VAR r/ANY} dom-type)))

(declare fill-env-for-term-with-spec)

(defn add-doms-to-node [env node & doms]
  (let [mod-doms (->> doms
                      (map #(if (= r/AND (r/spec-type %)) (:arglist %) %))
                      flatten
                      distinct)]
    (if (uber/has-node? env node)
      (uber/set-attrs env node (-> (uber/attrs env node)
                                   (update :history #(concat % mod-doms))
                                   (update :history distinct)
                                   (update :dom #(conj mod-doms %))
                                   (update :dom distinct)
                                   (update :dom #(reduce (fn [spec1 spec2] (r/intersect spec1 spec2 (utils/get-user-defined-specs env))) %))
                                   ))
      (uber/add-nodes-with-attrs env [node {:dom (reduce #(r/intersect %1 %2 (utils/get-user-defined-specs env)) mod-doms) :history mod-doms}]))))

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

(defn CANNOT-GROUND []
  (r/->ErrorSpec (str "Var term cannot be grounded here")))


(defmulti check-if-valid (fn [term spec] [(r/term-type term) (r/spec-type spec)]))

(defmethod check-if-valid [r/ATOM r/EXACT] [term spec]
  (= (:term term) (:value spec)))

(defmethod check-if-valid [r/NUMBER r/INTEGER] [term spec]
  (int? (:value term)))

(defmethod check-if-valid [r/NUMBER r/FLOAT] [term spec]
  (float? (:value term)))

(defmethod check-if-valid :default [term spec]
  true)


(defmulti fill-env (fn [env term spec initial?] (r/spec-type spec)))

(defmethod fill-env r/ANY [env term spec initial?]
  (if (nil? (utils/get-dom-of-term env term))
    (add-doms-to-node env term (r/copy-mark spec (r/initial-spec term)))
    env))

(defmethod fill-env r/SPECVAR [env term spec initial?]
  (let [step1 (fill-env-for-term-with-spec env initial? term (r/copy-mark spec (r/initial-spec term)))
        step2 (add-doms-to-node step1 spec (utils/get-dom-of-term step1 term))
        step3 (uber/add-edges step2 [term spec {:relation :specvar}])]
    step3))

(defmethod fill-env r/USERDEFINED [env term spec initial?]
  (let [transformed-definition (r/copy-mark spec (r/resolve-definition-with-parameters spec (get-defs-from-env env)))]
    (-> env
        (add-doms-to-node term spec)
        (fill-env-for-term-with-spec initial? term transformed-definition))))

(defmethod fill-env r/VAR [env term spec initial?]
  (if initial?
    (fill-env-for-term-with-spec env initial? term (r/initial-spec term))
    (add-doms-to-node env term (ALREADY-NONVAR))))

(defmethod fill-env :default [env term spec initial?]
  (let [suitable-spec (r/copy-mark spec (r/intersect spec (r/initial-spec term) (utils/get-user-defined-specs env)))
        next-steps (r/next-steps spec term (utils/get-user-defined-specs env))]
    (if (r/error-spec? suitable-spec)
      (add-doms-to-node env term (r/copy-mark spec (WRONG-TYPE term spec)))
      (if (check-if-valid term spec)
        (reduce #(apply fill-env-for-term-with-spec %1 initial? %2) (add-doms-to-node env term suitable-spec) (map (fn [[t s]] [t (r/copy-mark spec s)]) (partition 2 next-steps)))
        (add-doms-to-node env term (r/copy-mark spec (WRONG-TYPE term spec)))))))


(defn- var-or-any? [spec]
  (if (nil? spec)
    true
    (contains? #{r/VAR r/ANY} (r/spec-type spec))))

(defn- fill-env-chooser [env term spec initial?]
  (vector (if initial? :initial :non-initial)
          (case+ (r/spec-type spec)
                 r/VAR :var
                 r/ANY :any
                 r/SPECVAR :specvar
                 :other)))

(defn- remove-vars-from-dom [env term]
  (if (uber/has-node? env term)
    (let [new-attrs (-> (uber/attrs env term)

                        (update :dom #(if (= r/VAR (r/spec-type %)) (r/->AnySpec) %)))]
      (uber/set-attrs env term new-attrs))
    env))


(defmulti fill-env-for-var fill-env-chooser)

(defmethod fill-env-for-var [:initial :var] [env term spec initial?]
  (if (var-or-any? (utils/get-dom-of-term env term))
    (add-doms-to-node env term spec)
    env))

(defmethod fill-env-for-var [:initial :any] [env term spec initial?]
  (add-doms-to-node env term spec))

(defmethod fill-env-for-var [:initial :other] [env term spec initial?]
  (-> env
      (remove-vars-from-dom term)
      (fill-env term spec initial?)))

(defmethod fill-env-for-var [:initial :specvar] [env term spec initial?]
  (fill-env env term spec initial?))


(defmethod fill-env-for-var [:non-initial :var] [env term spec initial?]
  (if (var-or-any? (utils/get-dom-of-term env term))
    (add-doms-to-node env term spec)
    (add-doms-to-node env term (ALREADY-NONVAR))))

(defmethod fill-env-for-var [:non-initial :any] [env term spec initial?]
  (add-doms-to-node env term spec))

(defmethod fill-env-for-var [:non-initial :other] [env term spec initial?]
  (if (var-or-any? (utils/get-dom-of-term env term))
    (add-doms-to-node env term (CANNOT-GROUND))
    (fill-env env term spec initial?)))

(defmethod fill-env-for-var [:non-initial :specvar] [env term spec initial?]
  (fill-env env term spec initial?))




(defn fill-env-for-term-with-spec
  ([env initial? term spec]
   (log/debug "Fill env for term" (r/to-string term) "and spec" (r/to-string spec))
   (if (contains? #{r/VAR, r/ANY} (r/term-type term))
     (fill-env-for-var env term spec initial?)
     (fill-env env term spec initial?)))
  ([env term spec]
   (fill-env-for-term-with-spec env false term spec)))

(defn multiple-fills
  ([env terms specs initial?]
   (reduce (fn [e [term spec]] (fill-env-for-term-with-spec e initial? term spec)) env (map vector terms specs)))
  ([env terms specs]
   (multiple-fills env terms specs false)))

(defn spec-valid? [env term spec]
  (let [dom (utils/get-dom-of-term env term)]
    (if (nil? dom)
      (not (r/error-spec? (r/intersect spec (r/initial-spec term) (get-defs-from-env env))))
      (if (contains? #{r/OR, r/ERROR} (r/spec-type dom))
        false
        (not (r/error-spec? (r/intersect spec dom (get-defs-from-env env))))))))
