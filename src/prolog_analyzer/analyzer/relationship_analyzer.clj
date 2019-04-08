(ns prolog-analyzer.analyzer.relationship-analyzer
  (:require [ubergraph.core :as uber]
            [prolog-analyzer.analyzer.domain :as dom]
            [prolog-analyzer.records :as r]
            [prolog-analyzer.utils :as utils]
            [loom.attr]
            [loom.graph]
            ))

(defn was-var? [env term]
  (contains? (set (map #(dissoc % :origin) (uber/attr env term :history))) (r/->VarSpec)))

(defn- nonvar-spec? [env term])

(defmulti process-edge (fn [env edge] (uber/attr env edge :relation)))

(defmethod process-edge :specvar [env edge]
  (let [dest (uber/dest edge)
        src (uber/src edge)]
    (if (was-var? env src)
      (dom/add-type-to-dom env dest (utils/get-dom-of-term env src) {:overwrite true})
      (dom/add-type-to-dom env dest (utils/get-dom-of-term env src)))))

(defmethod process-edge :complex-specvar [env edge]
  (let [term (uber/src edge)
        userdef (uber/dest edge)
        used-specvars (map uber/dest (uber/out-edges env userdef))
        replace-map (reduce #(assoc %1 (:name %2) (or (utils/get-dom-of-term env %2) %1)) {} used-specvars)
        new-spec (reduce-kv r/replace-specvars-with-spec userdef replace-map)]
    (dom/fill-env-for-term-with-spec env term new-spec)
    ))

(defmethod process-edge :artificial [env edge]
  (let [normal (uber/src edge)
        artifical (uber/dest edge)]
    (-> env
        (dom/fill-env-for-term-with-spec normal (utils/get-dom-of-term env artifical))
        (dom/fill-env-for-term-with-spec artifical (utils/get-dom-of-term env normal)))))



(defmethod process-edge :default [env edge]
  env)


(defn- same [env other-env]
  (= env other-env))


(defn- step [env]
  (reduce process-edge env (uber/edges env)))


(defn fixpoint-analysis [env]
  (loop [in env]
    (let [next (step in)]
      (if (same in next)
        next
        (recur next)))))
