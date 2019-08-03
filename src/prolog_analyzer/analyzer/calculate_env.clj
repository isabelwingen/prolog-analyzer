(ns prolog-analyzer.analyzer.calculate-env
  (:require [prolog-analyzer.analyzer.domain :as dom]
            [prolog-analyzer.utils :as utils :refer [case+]]
            [prolog-analyzer.records :as r]
            [prolog-analyzer.record-utils :as ru]
            [clojure.pprint :refer [pprint]]
            [ubergraph.core :as uber]
            [prolog-analyzer.analyzer.post-specs :as post-specs]))

(defn add-to-dom [env term spec {defs :defs initial? :initial overwrite? :overwrite}]
  (if overwrite?
    (dom/add-to-dom env (dom/intersect-with-overwrite defs) term spec)
    (if initial?
      (dom/add-to-dom env (dom/intersect-with-initial defs) term spec)
      (dom/add-to-dom env (dom/intersect defs) term spec))))

(defmulti process-edge (fn [_ env edge] (uber/attr env edge :relation)))

(defn- compatible-with-head [{defs :defs initial? :initial :as parameters} head-dom term-dom]
  (case+ (r/safe-spec-type term-dom "compatible-with-head")
         r/TUPLE (let [new-dom (update term-dom :arglist #(assoc % 0 head-dom))]
                   (if (ru/non-empty-intersection new-dom term-dom defs initial?)
                     new-dom
                     nil))
         r/OR (let [new-dom (-> term-dom
                                (update :arglist (partial filter (partial compatible-with-head parameters head-dom)))
                                (update :arglist set)
                                (ru/simplify defs initial?))]
                (if (ru/non-empty-intersection new-dom term-dom defs initial?)
                  new-dom
                  nil))
         r/LIST (if (ru/non-empty-intersection head-dom (:type term-dom) defs initial?)
                  term-dom
                  nil)
         (if (ru/non-empty-intersection (r/->ListSpec head-dom) term-dom defs initial?)
           term-dom
           nil)))

(defmethod process-edge :is-head [parameters env edge]
  (let [head (uber/src edge)
        term (uber/dest edge)
        head-dom (utils/get-dom-of-term env head)
        term-dom (utils/get-dom-of-term env term)
        filtered-dom (compatible-with-head parameters head-dom term-dom)]
    (add-to-dom env term (or filtered-dom (r/DISJOINT)) parameters)))

(defn get-matching-head [pair-id env]
  (let [head (some->> env
                      uber/edges
                      (filter #(= :is-head (uber/attr env % :relation)))
                      (filter #(= pair-id (uber/attr env % :pair)))
                      first
                      uber/src)]
    (assert (not (nil? head)))
    head))

(defmethod process-edge :is-tail [parameters env edge]
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
    (add-to-dom env term new-dom parameters)))


(defmethod process-edge :arg-at-pos [parameters env edge]
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
    (add-to-dom env parent new-dom parameters)))

(defmethod process-edge :default [defs env edge]
  env)

(defn process-edges [env parameters]
  (reduce (partial process-edge parameters) env (uber/edges env)))

(defn- post-process-step [env parameters]
  (-> env
      (process-edges parameters)))

(defn- post-process [env {defs :defs :as parameters}]
  (loop [res env]
    (let [next (post-process-step res parameters)]
      (if (utils/same? next res)
        res
        (recur next)))))

(defn get-env-for-header
  "Calculates an environment from the header terms and the prespec"
  [defs arglist pre-spec]
  (let [parameters {:defs defs :initial true}]
    (-> (uber/digraph)
        (add-to-dom (apply ru/to-head-tail-list arglist) pre-spec parameters)
        dom/add-structural-edges
        (post-process parameters)
        )))
