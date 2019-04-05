(ns prolog-analyzer.analyzer.domain
  (:require [prolog-analyzer.utils :as utils :refer [case+]]
            [prolog-analyzer.records :as r :refer [intersect]]
            [prolog-analyzer.analyzer.pretty-printer :as pp]
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

(defn add-type-to-dom
  ([env term type {overwrite? :overwrite :as options}]
   (case+ (r/spec-type type)
          r/AND (reduce #(add-type-to-dom %1 term %2 options) env (:arglist type))
          r/SPECVAR (let [dom (or (utils/get-dom-of-term env term) (r/initial-spec term))]
                      (-> env
                          (add-type-to-dom type dom options)
                          (uber/add-edges [term type {:relation :specvar}])))
          (if (and (uber/has-node? env term) (utils/get-dom-of-term env term))
            (uber/set-attrs env term (-> (uber/attrs env term)
                                         (update :history #(conj % type))
                                         (update :history distinct)
                                         (update :dom #(if (= type %) % (r/intersect type % (utils/get-user-defined-specs env) overwrite?)))
                                         ))
            (uber/add-nodes-with-attrs env [term {:dom type :history [type]}]))))
  ([env term type]
   (if (r/error-spec? type)
     (add-type-to-dom env term type {:overwrite true})
     (add-type-to-dom env term type {:overwrite false}))))

(defn mark-as-was-var [env term]
  (let [attrs (uber/attrs env term)]
    (uber/set-attrs env term (assoc attrs :was-var true))))

(defn WRONG-TYPE
  ([]
   (r/->ErrorSpec "Wrong type associated"))
  ([term spec]
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

(defn- var-or-any? [spec]
  (if (nil? spec)
    false
    (contains? #{r/VAR r/ANY} (r/spec-type spec))))

(defn- var-or-any-or-nil? [spec]
  (if (nil? spec)
    true
    (contains? #{r/VAR r/ANY} (r/spec-type spec))))

(defn- remove-vars-from-dom [env term]
  (if (uber/has-node? env term)
    (let [new-attrs (-> (uber/attrs env term)

                        (update :dom #(if (= r/VAR (r/spec-type %)) (r/->AnySpec) %)))]
      (uber/set-attrs env term new-attrs))
    env))



(defmulti fill-dom (fn [env term spec options] [(if (= r/VAR (r/term-type term)) :var :nonvar) (case+ (r/spec-type spec)
                                                                                                     r/ANY :any
                                                                                                     r/USERDEFINED :userdefined
                                                                                                     r/SPECVAR :specvar
                                                                                                     r/VAR :var
                                                                                                     r/LIST :compound-or-list
                                                                                                     r/COMPOUND :compound-or-list
                                                                                                     r/TUPLE :compound-or-list
                                                                                                     r/ERROR :error
                                                                                                     :default)]))

(def ART_PREFIX "A__")

(defn- art-term? [{n :name}]
  (.startsWith (str n) ART_PREFIX))

(defn- has-artifical-term? [env term]
  (if (some->> term
               (uber/out-edges env)
               (filter #(= :artificial (uber/attr env % :relation)))
               first
               uber/dest)
    true
    false))

(defn- get-artificial-term [env term]
  (let [artificial-term (some->> term
                                 (uber/out-edges env)
                                 (filter #(= :artificial (uber/attr env % :relation)))
                                 first
                                 uber/dest)]
    artificial-term))

(defn- create-artifical-term [env term {type :type arglist :arglist functor :functor :as spec} {initial? :initial overwrite? :overwrite :as options}]
  (if (or (has-artifical-term? env term) (art-term? term))
    env
    (let [artificial-term (case+ (r/spec-type spec)
                                 r/LIST (r/->ListTerm (r/->VarTerm (str (gensym ART_PREFIX))) (r/->VarTerm (str (gensym ART_PREFIX))))
                                 r/COMPOUND (r/->CompoundTerm functor (repeatedly (count arglist) (fn [] (r/->VarTerm (str (gensym ART_PREFIX))))))
                                 r/TUPLE (apply r/to-head-tail-list (repeatedly (count arglist) (fn [] (r/->VarTerm (str (gensym ART_PREFIX))))))
                                 )]
      (-> env
          (fill-dom artificial-term (or (utils/get-dom-of-term env term) (r/->AnySpec)) {:initial true :overwrite true})
          (uber/add-edges [term artificial-term {:relation :artificial}])))))

(defn- add-to-artifical-term [env term spec options]
  (if (and (not (art-term? term)) (has-artifical-term? env term))
    (fill-dom env (get-artificial-term env term) spec options)
    env))

(defn- fill-dom-of-next-steps [env term spec options]
  (reduce (fn [e [t s]] (fill-dom e t s options)) env (partition 2 (r/next-steps spec term (utils/get-user-defined-specs env)))))


(defmethod fill-dom [:var :any] [env term spec options]
  (add-type-to-dom env term spec options))

(defmethod fill-dom [:var :userdefined] [env term spec {overwrite? :overwrite initial? :initial :as options}]
  (let [transformed-definition (r/resolve-definition-with-parameters spec (get-defs-from-env env))]
    (fill-dom env term transformed-definition options)))

(defmethod fill-dom [:var :specvar] [env term spec options]
  (let [step1 (fill-dom env term (r/initial-spec term) options)
        step2 (add-type-to-dom step1 spec (utils/get-dom-of-term step1 term) options)
        step3 (uber/add-edges step2 [term spec {:relation :specvar}])]
    step3))

(defmethod fill-dom [:var :var] [env term spec {initial? :initial :as options}]
  (if (var-or-any-or-nil? (utils/get-dom-of-term env term))
    (add-type-to-dom env term spec options)
    (if initial?
      env
      (add-type-to-dom env term (ALREADY-NONVAR)))))

(defn- compound-spec-for-var-term [env term spec options]
  (-> env
      (add-type-to-dom term spec options)
      (fill-dom-of-next-steps term spec options)
      (add-to-artifical-term term spec options)))

(defmethod fill-dom [:var :compound-or-list] [env term spec {overwrite? :overwrite initial? :initial :as options}]
  (if initial?
    (-> env
        (remove-vars-from-dom term)
        (create-artifical-term term spec options)
        (compound-spec-for-var-term term spec options)
        )
    (if overwrite?
      (-> env
          (create-artifical-term term spec options)
          (compound-spec-for-var-term term spec options))
      (if (var-or-any-or-nil? (utils/get-dom-of-term env term))
        (add-type-to-dom env term (CANNOT-GROUND))
        (-> env
            (create-artifical-term term spec options)
            (compound-spec-for-var-term term spec options))
        ))))

(defmethod fill-dom [:var :default] [env term spec {overwrite? :overwrite initial? :initial :as options}]
  (let [intersection (r/intersect (r/->VarSpec) spec (get-defs-from-env env) overwrite?)]
    (if initial?
      (-> env
          (remove-vars-from-dom term)
          (compound-spec-for-var-term term spec options)
          )
      (if overwrite?
        (-> env
            (compound-spec-for-var-term term spec options))
        (if (var-or-any-or-nil? (utils/get-dom-of-term env term))
          (if (r/error-spec? intersection)
            (add-type-to-dom env term (CANNOT-GROUND))
            (-> env
                (add-type-to-dom term intersection options)
                (add-to-artifical-term term spec options)))
          (-> env
              (compound-spec-for-var-term term spec options)
              ))))))


(defmethod fill-dom [:var :error] [env term spec options]
  (add-type-to-dom env term spec))

(defmethod fill-dom [:nonvar :any] [env term spec options]
  (if (nil? (utils/get-dom-of-term env term))
    (add-type-to-dom env term (r/initial-spec term) options)
    env))

(defmethod fill-dom [:nonvar :userdefined] [env term spec options]
  (let [transformed-definition (r/resolve-definition-with-parameters spec (get-defs-from-env env))]
    (-> env
        (fill-dom term transformed-definition options))))

(defmethod fill-dom [:nonvar :specvar] [env term spec options]
  (let [step1 (fill-dom env term (r/initial-spec term) options)
        step2 (add-type-to-dom step1 spec (utils/get-dom-of-term step1 term) options)
        step3 (uber/add-edges step2 [term spec {:relation :specvar}])]
    step3))

(defmethod fill-dom [:nonvar :var] [env term spec {initial? :initial overwrite? :overwrite :as options}]
  (if initial?
    (fill-dom env term (r/initial-spec term) options)
    (add-type-to-dom env term (ALREADY-NONVAR))))

(defmethod fill-dom [:nonvar :compound-or-list] [env term spec {overwrite? :overwrite :as options}]
  (let [suitable-spec (r/intersect spec (r/initial-spec term) (utils/get-user-defined-specs env) overwrite?)]
    (if (r/error-spec? suitable-spec)
      (add-type-to-dom env term (WRONG-TYPE term spec))
      (-> env
          (add-type-to-dom term suitable-spec options)
          (fill-dom-of-next-steps term spec options)))))

(defmethod fill-dom [:nonvar :default] [env term spec {overwrite? :overwrite :as options}]
  (let [suitable-spec (r/intersect spec (r/initial-spec term) (utils/get-user-defined-specs env) overwrite?)]
    (if (or (r/error-spec? suitable-spec) (not (check-if-valid term spec)))
      (add-type-to-dom env term (WRONG-TYPE term spec))
      (-> env
          (add-type-to-dom term suitable-spec options)
          (fill-dom-of-next-steps term spec options)))))


(defmethod fill-dom [:nonvar :error] [env term spec options]
  (add-type-to-dom env term spec true))


(defn fill-env-for-term-with-spec
  ([env term spec options]
   (log/debug "Fill env for term" (r/to-string term) "and spec" (r/to-string spec))
   (fill-dom env term spec options))
  ([env term spec]
   (fill-env-for-term-with-spec env term spec {:initial false})))

(defn multiple-fills
  ([env terms specs options]
   (reduce (fn [e [term spec]] (fill-env-for-term-with-spec e term spec options)) env (map vector terms specs)))
  ([env terms specs]
   (multiple-fills env terms specs {:initial false})))


(defmulti spec-valid? (fn [env term spec] (case+ (r/term-type term)
                                                (r/LIST r/EMPTYLIST) :list
                                                r/COMPOUND :compound
                                                :other)))

(defmethod spec-valid? :compound [env {functor-term :functor :as term} {functor-spec :functor :as spec}]
  (if-let [dom (utils/get-dom-of-term env term)]
    (if (contains? #{r/OR r/ERROR} (r/spec-type dom))
      false
      (and
       (= functor-term functor-spec)
       (not (r/error-spec? (r/intersect spec dom (get-defs-from-env env))))
       (every? (partial apply spec-valid? env) (partition 2 (r/next-steps spec term (get-defs-from-env env))))))
    (and
     (= functor-term functor-spec)
     (not (r/error-spec? (r/intersect spec (r/initial-spec term) (get-defs-from-env env))))))) ;;TODO: if dom is nil, is the result false?



(defmethod spec-valid? :list [env term spec]
  (if-let [dom (utils/get-dom-of-term env term)]
    (if (contains? #{r/OR r/ERROR} (r/spec-type dom))
      false
      (and
       (not (r/error-spec? (r/intersect spec dom (get-defs-from-env env))))
       (every? (partial apply spec-valid? env) (partition 2 (r/next-steps spec term (get-defs-from-env env))))))
    (not (r/error-spec? (r/intersect spec (r/initial-spec term) (get-defs-from-env env)))))) ;;TODO: if dom is nil, is the result false?

(defmethod spec-valid? :other [env term spec]
  (if-let [dom (utils/get-dom-of-term env term)]
    (if (contains? #{r/OR r/ERROR} (r/spec-type dom))
      false
      (not (r/error-spec? (r/intersect spec dom (get-defs-from-env env)))))
    (not (r/error-spec? (r/intersect spec (r/initial-spec term) (get-defs-from-env env))))))
