(ns prolog-analyzer.analyzer.relationship-analyzer
  (:require [ubergraph.core :as uber]
            [prolog-analyzer.analyzer.domain :as dom]
            [prolog-analyzer.records :as r]
            [prolog-analyzer.utils :as utils :refer [case+]]
            [clojure.tools.logging :as log]
            [loom.attr]
            [loom.graph]
            ))


(defmulti process-edge (fn [env edge] (uber/attr env edge :relation)))

(defmethod process-edge :specvar [env edge]
  (let [dest (uber/dest edge)
        src (uber/src edge)]
    (dom/add-type-to-dom env dest (utils/get-dom-of-term env src) {:overwrite true})))

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

(defmethod process-edge :is-head [env edge]
  (let [list (uber/dest edge)
        head (uber/src edge)
        head-dom (utils/get-dom-of-term env head)
        tail (some->> list
                      (uber/in-edges env)
                      (filter #(= :is-tail (uber/attr env % :relation)))
                      first
                      uber/src)
        tail-dom (some->> tail (utils/get-dom-of-term env))
        overwrite true]
    (if (nil? tail-dom)
      (dom/fill-env-for-term-with-spec env list (r/->TupleSpec [head-dom]) {:overwrite overwrite})
      (case+ (r/spec-type tail-dom)
             r/TUPLE (dom/fill-env-for-term-with-spec env list (update tail-dom :arglist #(cons head-dom %)) {:overwrite overwrite})
             r/LIST (dom/fill-env-for-term-with-spec env list (update tail-dom :type #(r/->OneOfSpec (hash-set % head-dom))) {:overwrite overwrite})
             env))))


(defmethod process-edge :arg-at-pos [env edge]
  (let [compound (uber/dest edge)
        part (uber/src edge)
        part-dom (utils/get-dom-of-term env part)
        compound-dom (utils/get-dom-of-term env compound)
        pos (uber/attr env edge :pos)]
    (if (= r/COMPOUND (r/spec-type compound-dom))
      (if (>= pos (count (:arglist compound-dom)))
        (do
          (log/debug (str "ERROR found: Found a compound with size " (count (:arglist compound-dom)) " but need at least size " (inc pos)))
          env)
        (dom/fill-env-for-term-with-spec env compound (update compound-dom :arglist #(assoc (apply vector %) pos part-dom)) {:overwrite true}))
      env)))

(defmethod process-edge :default [env edge]
  env)


(defn- same [env other-env]
  (= env other-env))


(defn- step [env]
  (reduce process-edge env (uber/edges env)))


(defn fixpoint-analysis [env]
  (log/debug "Start Fixpoint Analysis")
  (loop [in env
         counter 0]
    (let [next (step in)]
      (if (same in next)
        (do (log/debug "Fixpoint Analysis is done") next)
        (if (> counter 50)
          (do
            (log/error "infinite loop?")
            next)
          (recur next (inc counter)))))))
