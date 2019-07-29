(ns prolog-analyzer.analyzer.env-for-header
  (:require [prolog-analyzer.analyzer.domain :as dom]
            [prolog-analyzer.utils :as utils :refer [case+]]
            [prolog-analyzer.records :as r]
            [prolog-analyzer.record-utils :as ru]
            [clojure.pprint :refer [pprint]]
            [ubergraph.core :as uber]))

(def INITIAL true)
(def EMPTY-DEFS {})



(defn calculate-new-dom [env term]
  (let [spec (apply ru/intersect* INITIAL {} (utils/get-dom-of-term env term))
        new-env (-> env
                    (uber/remove-attr term dom/DOM)
                    (dom/add-to-dom term spec)
                    (uber/add-attr term :changed false)
                    )
        new-dom (seq (utils/get-dom-of-term new-env term))]
    (assert
     (= 1 (count new-dom))
     (str "new-dom is wrong " (count new-dom) (apply vector (map r/to-string new-dom)) " " (r/to-string term)))
    new-env))

(defn simplify-env [env]
  (reduce
   #(let [spec (apply ru/intersect* INITIAL {} (utils/get-dom-of-term %1 %2))]
      (uber/add-attr %1 %2 dom/DOM [spec]))
   env
   (utils/get-terms env)))

(defn shrink-domains [env]
  (loop [res env]
    (let [p (dom/changed-terms res)]
      (if (empty? p)
        (simplify-env res)
        (recur (reduce calculate-new-dom (dom/reset-changed-marker res) p))))))

(defmulti process-edge (fn [env edge] (uber/attr env edge :relation)))

(defn- compatible-with-head [head-dom term-dom]
  (case+ (r/safe-spec-type term-dom "compatible-with-head")
         r/TUPLE (let [new-dom (update term-dom :arglist #(assoc % 0 head-dom))]
                   (if (ru/non-empty-intersection new-dom term-dom EMPTY-DEFS INITIAL)
                     new-dom
                     nil))
         r/OR (let [new-dom (-> term-dom
                                (update :arglist (partial filter (partial compatible-with-head head-dom)))
                                (update :arglist set)
                                (ru/simplify EMPTY-DEFS INITIAL))]
                (if (ru/non-empty-intersection new-dom term-dom EMPTY-DEFS INITIAL)
                  new-dom
                  nil))
         r/LIST (if (ru/non-empty-intersection head-dom (:type term-dom) EMPTY-DEFS INITIAL)
                  term-dom
                  nil)
         (if (ru/non-empty-intersection (r/->ListSpec head-dom) term-dom EMPTY-DEFS INITIAL)
           term-dom
           nil)))

(defmethod process-edge :is-head [env edge]
  (let [head (uber/src edge)
        term (uber/dest edge)
        head-dom (utils/get-shrinked-domain env head)
        term-dom (utils/get-shrinked-domain env term)
        filtered-dom (compatible-with-head head-dom term-dom)]
    (if (nil? filtered-dom)
      (uber/add-attr env term dom/DOM [(r/->ErrorSpec "hallo")])
      (dom/add-to-dom env term filtered-dom))))

(defn get-matching-head [pair-id env]
  (let [head (some->> env
                      uber/edges
                      (filter #(= :is-head (uber/attr env % :relation)))
                      (filter #(= pair-id (uber/attr env % :pair)))
                      first
                      uber/src)]
    (assert (not (nil? head)))
    head))

(defmethod process-edge :is-tail [env edge]
  (let [tail (uber/src edge)
        term (uber/dest edge)
        tail-dom (utils/get-shrinked-domain env tail)
        term-dom (utils/get-shrinked-domain env term)
        pair-id (uber/attr env edge :pair)
        head (get-matching-head pair-id env)
        head-dom (utils/get-shrinked-domain env head)
        new-dom (case+ (r/safe-spec-type tail-dom "process-tail")
                       r/TUPLE (update tail-dom :arglist #(->> %
                                                               (cons head-dom)
                                                               (apply vector)))
                       r/LIST (update tail-dom :type #(r/->OneOfSpec (hash-set % head-dom)))
                       term-dom)]
    (dom/add-to-dom env term new-dom)))


(defmethod process-edge :arg-at-pos [env edge]
  (let [child (uber/src edge)
        child-dom (utils/get-shrinked-domain env child)
        parent (uber/dest edge)
        functor (:functor parent)
        args (count (:arglist parent))
        pos (uber/attr env edge :pos)
        new-dom (->> (r/->AnySpec)
                     (repeat args)
                     (apply vector)
                     (#(assoc % pos child-dom))
                     (apply vector)
                     (r/->CompoundSpec functor))]
    (dom/add-to-dom env parent new-dom)))

(defmethod process-edge :default [env edge]
  env)

(defn process-edges [env]
  (reduce (comp shrink-domains process-edge) env (uber/edges env)))

(defn post-process [env]
  (loop [res (shrink-domains env)]
    (let [next (process-edges res)]
      (if (utils/same? next res)
        res
        (recur next)))))

(defn get-env
  "Calculates an environment from the header terms and the prespec"
  [data {arglist :arglist :as clause} pre-spec]
  (-> (uber/digraph)
      (dom/add-to-dom (apply ru/to-head-tail-list arglist) pre-spec)
      dom/add-structural-edges
      post-process
      ))

(clojure.pprint/pprint (utils/env->map (get-env {}
                                                {:arglist [(r/->VarTerm "H") (r/->ListTerm (r/->VarTerm "H") (r/->VarTerm "T"))]}
                                                (r/->TupleSpec [(r/->IntegerSpec)
                                                                (r/->OneOfSpec #{(r/->ListSpec (r/->IntegerSpec))
                                                                                 (r/->ListSpec (r/->FloatSpec))})]))))
