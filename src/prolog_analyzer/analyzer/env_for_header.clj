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
                    )
        new-dom (seq (utils/get-dom-of-term new-env term))]
    (assert
     (= 1 (count new-dom))
     (str "new-dom is wrong " (count new-dom) (apply vector (map r/to-string new-dom)) " " (r/to-string term)))
    new-env))

(defn shrink-domains [env]
  (loop [res env]
    (let [p (dom/get-terms-with-multiple-doms res)]
      (if (empty? p)
        res
        (recur (reduce calculate-new-dom res p))))))


(defmulti process-edge (fn [env edge] (uber/attr env edge :relation)))

(defn- compatible-with-head [head-dom term-dom]
  (case+ (r/spec-type term-dom)
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
        head-dom (first (utils/get-dom-of-term env head))
        term-dom (first (utils/get-dom-of-term env term))
        filtered-dom (compatible-with-head head-dom term-dom)]
    (dom/add-to-dom env term filtered-dom)))

(defmethod process-edge :default [env edge]
  env)

(defn process-edges [env]
  (reduce (comp shrink-domains process-edge) env (uber/edges env)))

(defn post-process [env]
  (loop [res (shrink-domains env)]
    (pprint (utils/env->map res))
    (let [next (process-edges res)]
      (pprint (utils/env->map next))
      (if (utils/same? next res)
        (do
          (println "done")
          res)
        (recur next)))))

(defn get-env
  "Calculates an environment from the header terms and the prespec"
  [data {arglist :arglist :as clause} pre-spec]
  (-> (uber/digraph)
      (dom/add-to-dom (apply ru/to-head-tail-list arglist) pre-spec)
      post-process
      ))
