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

(declare fill-env-for-term-with-spec)
(declare multiple-fills)

(defn- get-defs-from-env [env]
  (uber/attr env :ENVIRONMENT :user-defined-specs))

(defn- var-domain [env term]
  (let [dom-type (r/spec-type (utils/get-dom-of-term env term (r/->AnySpec)))]
    (contains? #{r/VAR r/ANY} dom-type)))

(declare fill-env-for-term-with-spec)
(declare fill-dom)

(defn- replace-var-with-any [spec]
  (case+ (r/spec-type spec)
         r/VAR (r/->AnySpec)
         (r/OR,r/AND) (-> spec
                          (update :arglist (partial map replace-var-with-any))
                          (update :arglist set))
         (r/TUPLE, r/COMPOUND) (update spec :arglist (partial map replace-var-with-any))
         r/USERDEFINED (if (:arglist spec) (update spec :arglist (partial map replace-var-with-any)) spec)
         r/LIST (update spec :type replace-var-with-any)
         spec))


(defn- initial-dom [obj]
  (if (satisfies? prolog-analyzer.records/spec obj)
    (r/->AnySpec)
    (r/initial-spec obj)))

(defn add-type-to-dom
  ([env term type {overwrite? :overwrite :as options}]
   (case+ (r/spec-type type)
          r/AND (reduce #(add-type-to-dom %1 term %2 options) env (:arglist type))
          (let [new-type (r/replace-specvars-with-any type)
                dom (utils/get-dom-of-term env term (initial-dom term))
                old-type (if overwrite? (replace-var-with-any dom) dom)
                defs (utils/get-user-defined-specs env)]
            (-> env
                (uber/add-nodes term)
                (uber/add-attr term :dom (r/intersect new-type old-type defs))))))
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
  (if (nil? (:term term))
    true
    (= (:term term) (:value spec))))


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

(defn- var-or-nil? [spec]
  (if (nil? spec)
    true
    (= r/VAR (r/spec-type spec))))

(defn- remove-vars-from-dom [env term]
  (if (and (uber/has-node? env term) (r/var-spec? (utils/get-dom-of-term env term (r/->AnySpec))))
    (-> env
        (uber/remove-attr term :dom)
        (uber/add-attr term :dom (r/->AnySpec)))
    env))

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
          (fill-dom artificial-term (utils/get-dom-of-term env term (r/->AnySpec)) {:initial true :overwrite true})
          (uber/add-edges [term artificial-term {:relation :artificial}])))))

(defn- add-to-artifical-term [env term spec options]
  (if (and (not (art-term? term)) (has-artifical-term? env term))
    (fill-dom env (get-artificial-term env term) spec options)
    env))

(defn- fill-dom-of-next-steps [env term spec {overwrite? :overwrite initial? :initial :as options}]
  (let [next (partition 2 (r/next-steps spec term (utils/get-user-defined-specs env) overwrite?))]
    (reduce
     (fn [e [t s]]
       (assert s (str "Fill-dom-of-next-steps: " (utils/get-pred-id env) " " (r/to-string term) " " (r/to-string spec)))
       (fill-dom e t s options))
     env
     next)))

(defmulti fill-dom (fn [env term spec options] [(if (= r/VAR (r/term-type term)) :var :nonvar) (case+ (r/spec-type spec)
                                                                                                     r/ANY :any
                                                                                                     r/USERDEFINED :userdefined
                                                                                                     r/UNION :union
                                                                                                     r/COMPATIBLE :compatible
                                                                                                     r/VAR :var
                                                                                                     r/LIST :list
                                                                                                     r/COMPOUND :compound
                                                                                                     r/TUPLE :tuple
                                                                                                     r/ERROR :error
                                                                                                     :default)]))

(defmethod fill-dom [:var :any] [env term spec options]
  (add-type-to-dom env term spec options))

(defmethod fill-dom [:var :userdefined] [env term spec {overwrite? :overwrite initial? :initial :as options}]
  (let [transformed-definition (r/resolve-definition-with-parameters spec (get-defs-from-env env))]
    (if (r/has-specvars spec)
      (-> env
          (uber/add-edges [term spec {:relation :complex-userdef}])
          (fill-dom term (r/replace-specvars-with-any transformed-definition) options))
      (fill-dom env term transformed-definition options))
    ))

(defmethod fill-dom [:var :union] [env term spec options]
  (-> env
         (fill-dom term (r/initial-spec term) options)
         (uber/add-attr term :was-var true)
         (uber/add-edges [term (r/->SpecvarSpec (.name spec)) {:relation :union}])
         (add-to-artifical-term term spec options)))

(defmethod fill-dom [:var :compatible] [env term spec options]
  (-> env
      (fill-dom term (r/initial-spec term) options)
      (uber/add-attr term :was-var true)
      (uber/add-edges [term (r/->SpecvarSpec (.name spec)) {:relation :compatible}])
      (add-to-artifical-term term spec options)))


(defmethod fill-dom [:var :var] [env term spec {initial? :initial :as options}]
  (cond
    (nil? (utils/get-dom-of-term env term nil)) (add-type-to-dom env term spec options)
    (r/var-spec? (utils/get-dom-of-term env term (r/->AnySpec))) env
    (r/any-spec? (utils/get-dom-of-term env term (r/->AnySpec))) (-> env
                                                                     (uber/add-nodes term)
                                                                     (uber/add-attr term :assumed-type spec)
                                                                     (add-type-to-dom term spec options))
    :else (if initial?
            (-> env
                (uber/add-attr term :was-var true))
            (add-type-to-dom env term (ALREADY-NONVAR)))))

(defn- process-filling-for-var-term [env term spec options]
  (let [res
        (-> env
            (add-type-to-dom term spec options)
            (fill-dom-of-next-steps term spec options)
            (add-to-artifical-term term spec options)

            )]
    res))

(defn fill-dom-compound-or-list [in-env term spec {overwrite? :overwrite initial? :initial :as options}]
  (let [env (-> in-env (uber/add-nodes term) (uber/add-attr term :was-var true))]
    (if initial?
      (-> env
          (remove-vars-from-dom term)
          (process-filling-for-var-term term spec options))
      (if overwrite?
        (process-filling-for-var-term env term spec options)
        (if (r/var-spec? (utils/get-dom-of-term env term (r/->AnySpec)))
          (add-type-to-dom env term (CANNOT-GROUND))
          (if (r/any-spec? (utils/get-dom-of-term env term (r/->AnySpec)))
            (-> env
                (uber/add-nodes term)
                (uber/add-attr term :assumed-type spec)
                (process-filling-for-var-term term spec options))
            (process-filling-for-var-term env term spec options)))))))

(defmethod fill-dom [:var :compound] [in-env term spec {overwrite? :overwrite initial? :initial :as options}]
  (let [env (-> in-env (uber/add-nodes term) (uber/add-attr term :was-var true))]
    (if (nil? (.functor spec))
      (-> env
          (fill-dom-compound-or-list term spec options)
          (uber/add-attr term :is-compound true))
      (-> env
          (create-artifical-term term spec options)
          (uber/add-attr term :functor (.functor spec))
          (uber/add-attr term :is-compound true)
          (fill-dom-compound-or-list term spec options)))))

(defmethod fill-dom [:var :tuple] [in-env term spec {overwrite? :overwrite initial? :initial :as options}]
  (let [env (-> in-env (uber/add-nodes term) (uber/add-attr term :was-var true))]
    (-> env
        (create-artifical-term term spec options)
        (uber/add-attr term :is-tuple true)
        (fill-dom-compound-or-list term spec options))))

(defn- get-artificial-type [env term]
  (some->> term
           (uber/out-edges env)
           (filter #(= :has-type (uber/attr env % :relation)))
           first
           uber/dest))

(defmethod fill-dom [:var :list] [in-env term spec {overwrite? :overwrite initial? :initial :as options}]
  (let [env (-> in-env (uber/add-nodes term) (uber/add-attr term :was-var true))
        generated-var (r/->VarTerm (gensym "T__"))]
    (if-let [artificial-type (get-artificial-type env term)]
      (-> env
          (fill-dom-compound-or-list term spec options)
          (fill-dom artificial-type (.type spec) options))
      (-> env
          (uber/add-attr term :is-list true)
          (uber/add-edges [term generated-var {:relation :has-type}])
          (fill-dom-compound-or-list term spec options)
          (fill-dom generated-var (.type spec) {:initial true})))))


(defmethod fill-dom [:var :default] [in-env term spec {overwrite? :overwrite initial? :initial :as options}]
  (let [env (-> in-env (uber/add-nodes term) (uber/add-attr term :was-var true))]
    (if initial? ;; mode initial -> grounding ok
      (-> env
          (remove-vars-from-dom term)
          (process-filling-for-var-term term spec options))
      (if overwrite? ;; mode overwrite -> grounding ok
        (process-filling-for-var-term env term spec options)
        (if (r/var-spec? (utils/get-dom-of-term env term (r/->AnySpec)))
          (process-filling-for-var-term env term spec options)
          (if (r/any-spec? (utils/get-dom-of-term env term (r/->AnySpec)))
            (-> env
                (uber/add-nodes term)
                (uber/add-attr term :assumed-type spec)
                (process-filling-for-var-term term spec options))
            (process-filling-for-var-term env term spec options)))))))


(defmethod fill-dom [:var :error] [env term spec options]
  (add-type-to-dom env term spec))

(defmulti add-children (fn [env compound] (cond
                                           (:head compound) :list
                                           (:arglist compound) :compound
                                           :else :single)))

(defmethod add-children :list [env {head :head tail :tail}]
  (let [head-env (fn [e] (if (utils/get-dom-of-term e head nil) e (fill-env-for-term-with-spec e head (r/->AnySpec) {:initial true})))
        tail-env (fn [e] (if (utils/get-dom-of-term e tail nil) e (fill-env-for-term-with-spec e tail (r/->AnySpec) {:initial true})))]
    (-> env
        head-env
        tail-env)))

(defmethod add-children :compound [env {arglist :arglist}]
  (reduce #(if (utils/get-dom-of-term %1 %2 nil) %1 (fill-env-for-term-with-spec %1 %2 (r/->AnySpec) {:initial true})) env arglist))

(defmethod add-children :default [env _] env)

(defmethod fill-dom [:nonvar :any] [env term spec options]
  (add-children
   (if (utils/get-dom-of-term env term nil)
     env
     (add-type-to-dom env term (r/->AnySpec) options))
   term))

(defmethod fill-dom [:nonvar :userdefined] [env term spec options]
  (let [transformed-definition (r/resolve-definition-with-parameters spec (get-defs-from-env env))]
    (if (r/has-specvars spec)
      (-> env
          (uber/add-edges [term spec {:relation :complex-userdef}])
          (fill-dom term (r/replace-specvars-with-any transformed-definition) options))
      (fill-dom env term transformed-definition options))))

(defmethod fill-dom [:nonvar :union] [env term spec options]
  (-> env
      (fill-dom term (r/initial-spec term) options)
      (uber/add-edges [term (r/->SpecvarSpec (.name spec)) {:relation :union}])))

(defmethod fill-dom [:nonvar :compatible] [env term spec options]
  (-> env
      (fill-dom term (r/initial-spec term) options)
      (uber/add-edges [term (r/->SpecvarSpec (.name spec)) {:relation :compatible}])))



(defmethod fill-dom [:nonvar :var] [env term spec {initial? :initial overwrite? :overwrite :as options}]
  (if initial?
    (-> env
        (uber/add-nodes term)
        (uber/add-attr term :was-var true)
        (fill-dom term (r/initial-spec term) options)
        (add-children term))
    (add-type-to-dom env term (ALREADY-NONVAR))))

(defn fill-dom-nonvar-compound-or-list [env term spec options]
  (-> env
      (add-type-to-dom term spec options)
      (fill-dom-of-next-steps term spec options)))

(defmethod fill-dom [:nonvar :compound] [env term spec options] (fill-dom-nonvar-compound-or-list env term spec options))
(defmethod fill-dom [:nonvar :list] [env term spec options] (fill-dom-nonvar-compound-or-list env term spec options))
(defmethod fill-dom [:nonvar :tuple] [env term spec options] (fill-dom-nonvar-compound-or-list env term spec options))

(defmethod fill-dom [:nonvar :default] [env term spec {overwrite? :overwrite :as options}]
  (if (check-if-valid term spec)
    (-> env
        (add-type-to-dom term spec options)
        (fill-dom-of-next-steps term spec options))
    (add-type-to-dom env term (WRONG-TYPE term spec))
    ))



(defmethod fill-dom [:nonvar :error] [env term spec options]
  (add-type-to-dom env term spec true))


(defn fill-env-for-term-with-spec
  ([env term spec options]
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
  (if-let [dom (utils/get-dom-of-term env term nil)]
    (if (contains? #{r/OR r/ERROR} (r/spec-type dom))
      false
      (and
       (= functor-term functor-spec)
       (not (r/error-spec? (r/intersect dom spec (get-defs-from-env env))))
       (every? (partial apply spec-valid? env) (partition 2 (r/next-steps spec term (get-defs-from-env env))))))
    (and
     (= functor-term functor-spec)
     (not (r/error-spec? (r/intersect spec (r/initial-spec term) (get-defs-from-env env))))))) ;;TODO: if dom is nil, is the result false?



(defmethod spec-valid? :list [env term spec]
  (if-let [dom (utils/get-dom-of-term env term nil)]
    (if (contains? #{r/OR r/ERROR} (r/spec-type dom))
      false
      (and
       (not (r/error-spec? (r/intersect dom spec (get-defs-from-env env))))
       (every? (partial apply spec-valid? env) (partition 2 (r/next-steps spec term (get-defs-from-env env))))))
    (not (r/error-spec? (r/intersect spec (r/initial-spec term) (get-defs-from-env env)))))) ;;TODO: if dom is nil, is the result false?

(defmethod spec-valid? :other [env term spec]
  (if-let [dom (utils/get-dom-of-term env term nil)]
    (if (contains? #{r/OR r/ERROR} (r/spec-type dom))
      false
      (not (r/error-spec? (r/intersect dom spec (get-defs-from-env env)))))
    (not (r/error-spec? (r/intersect spec (r/initial-spec term) (get-defs-from-env env))))))
