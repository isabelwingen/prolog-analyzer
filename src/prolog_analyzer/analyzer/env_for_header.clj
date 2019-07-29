(ns prolog-analyzer.analyzer.env-for-header
  (:require [prolog-analyzer.analyzer.domain :as dom]
            [prolog-analyzer.utils :as utils :refer [case+]]
            [prolog-analyzer.records :as r]
            [prolog-analyzer.record-utils :as ru]
            [clojure.pprint :refer [pprint]]
            [ubergraph.core :as uber]))

(def INITIAL true)
(def EMPTY-DEFS {})


(defmulti process-edge (fn [env edge] (uber/attr env edge :relation)))

(defn add-to-dom [env term spec defs]
  (dom/add-to-dom env (dom/intersect-with-initial defs) term spec))

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
        head-dom (utils/get-dom-of-term env head)
        term-dom (utils/get-dom-of-term env term)
        filtered-dom (compatible-with-head head-dom term-dom)]
    (add-to-dom env term (or filtered-dom (r/DISJOINT)) EMPTY-DEFS)))

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
        tail-dom (utils/get-dom-of-term env tail)
        term-dom (utils/get-dom-of-term env term)
        pair-id (uber/attr env edge :pair)
        head (get-matching-head pair-id env)
        head-dom (utils/get-dom-of-term env head)
        new-dom (case+ (r/safe-spec-type tail-dom "process-tail")
                       r/TUPLE (update tail-dom :arglist #(->> %
                                                               (cons head-dom)
                                                               (apply vector)))
                       r/LIST (update tail-dom :type #(r/->OneOfSpec (hash-set % head-dom)))
                       term-dom)]
    (add-to-dom env term new-dom EMPTY-DEFS)))


(defmethod process-edge :arg-at-pos [env edge]
  (let [child (uber/src edge)
        child-dom (utils/get-dom-of-term env child)
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
    (add-to-dom env parent new-dom EMPTY-DEFS)))

(defmethod process-edge :default [env edge]
  env)

(defn process-edges [env]
  (reduce process-edge env (uber/edges env)))

(defn post-process [env]
  (loop [res env]
    (let [next (process-edges res)]
      (if (utils/same? next res)
        res
        (recur next)))))

(defn get-env
  "Calculates an environment from the header terms and the prespec"
  [data {arglist :arglist :as clause} pre-spec]
  (-> (uber/digraph)
      (add-to-dom (apply ru/to-head-tail-list arglist) pre-spec EMPTY-DEFS)
      dom/add-structural-edges
      post-process
      ))
