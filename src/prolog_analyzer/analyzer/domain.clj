(ns prolog-analyzer.analyzer.domain
  (:require [prolog-analyzer.utils :as utils :refer [case+]]
            [prolog-analyzer.records :as r]
            [prolog-analyzer.record-utils :as ru]
            [ubergraph.core :as uber]))

(declare add-to-dom)
(declare merge-envs)

(def DOM :dom)
(def HIST :history)

(defmulti edges (fn [term] (r/term-type term)))

(defmethod edges :list [term]
  (let [pair (gensym)]
    [[(ru/head term) term {:relation :is-head :pair pair}]
     [(ru/tail term) term {:relation :is-tail :pair pair}]]))

(defmethod edges :compound [term]
  (map-indexed #(vector %2 term {:relation :arg-at-pos :pos %1}) (:arglist term)))

(defmethod edges :default [term]
  [])


(defn add-structural-edges [env]
  (loop [res env]
    (let [next (reduce #(apply uber/add-edges %1 (edges %2)) res (utils/get-terms res))]
      (if (utils/same? next res)
        next
        (recur next)))))

(defmulti next-steps
  (fn [term spec]
    [(if (ru/nonvar-term? term) :nonvar :var)
     (case+ (r/safe-spec-type spec (str "next-steps" (r/to-string term)))
            r/TUPLE :tuple
            r/COMPOUND :compound
            r/LIST :list
            r/USERDEFINED :userdef
            r/GROUND :ground
            :unnested)]))

(defn- split-spec-for-list [spec]
  (case+ (r/safe-spec-type spec "split-spec-for-list")
         r/LIST [(:type spec) spec]
         r/TUPLE [(first (:arglist spec)) (-> spec
                                              (update :arglist rest)
                                              (update :arglist (partial apply vector)))]
         [spec spec]))


(defmethod next-steps [:nonvar :tuple] [term spec]
  (if (ru/list-term? term)
    [[(ru/head term) (first (:arglist spec))]
     [(ru/tail term) (update spec :arglist rest)]]
    []))

(defmethod next-steps [:nonvar :compound] [term spec]
  (if (ru/compound-term? term)
    (apply vector (map vector (:arglist term) (:arglist spec)))
    []))

(defmethod next-steps [:nonvar :list] [term spec]
  (if (ru/list-term? term)
    [[(ru/head term) (:type spec)]
     [(ru/tail term) spec]]
    []))

(defmethod next-steps [:nonvar :ground] [term spec]
  (cond
    (ru/list-term? term) [[(ru/head term) spec]
                          [(ru/tail term) spec]]
    (ru/compound-term? term) (apply vector (map #(vector % spec) (:arglist term)))
    :else []))


(defmethod next-steps [:nonvar :userdef] [term spec]
  [])

(defmethod next-steps [:nonvar :unnested] [term spec]
  [])

(defmethod next-steps [:var :tuple] [term spec] [])
(defmethod next-steps [:var :compound] [term spec] [])
(defmethod next-steps [:var :list] [term spec] [])
(defmethod next-steps [:var :userdef] [term spec] [])
(defmethod next-steps [:var :ground] [term spec] [])
(defmethod next-steps [:var :unnested] [term spec] [])


(defn process-next-steps [env term spec]
  (if (ru/error-spec? spec)
    env
    (let [steps (next-steps term spec)]
      (reduce
       #(apply add-to-dom %1 %2)
       env
       steps))))

(defn add-or-to-dom [env term spec]
  (let [arglist (:arglist spec)
        envs (map (partial add-to-dom env term) arglist)]
    (if (zero? (count envs))
      env
      (apply merge-envs envs))))

(defn debug [term spec]
  (if (or (nil? term) (nil? spec))
    nil
    (println "add-to-dom for term " (r/to-string term) " and " (r/to-string spec))))

(defn add-to-dom [env term spec]
  (cond
    (not (uber/has-node? env term))  (-> env
                                         (uber/add-nodes term)
                                         (add-to-dom term (r/initial-spec term))
                                         (add-to-dom term spec))
    (ru/or-spec? spec)               (add-or-to-dom env term spec)
    (ru/and-spec? spec)              (reduce #(add-to-dom %1 term %2) env (:arglist spec))
    :default                         (-> env
                                         (utils/update-attr term DOM #(set (conj %1 %2)) spec)
                                         (uber/add-attr term :changed true)
                                         (process-next-steps term spec))))


(defn reset-changed-marker [env]
  (reduce #(uber/add-attr %1 %2 :changed false) env (utils/get-terms env)))

(defn something-changed [env]
  (some #(uber/attr env % :changed) (utils/get-terms env)))

(defn changed-terms [env]
  (filter #(uber/attr env % :changed) (utils/get-terms env)))


(defn get-terms-with-multiple-doms [env]
  (->> env
       utils/get-terms
       (filter #(> (count (utils/get-dom-of-term env % [])) 1))))

(defn- and-or-single [specs]
  (let [ds (->> specs
                (remove ru/any-spec?)
                distinct)]
    (if (= 1 (count ds))
      (first ds)
      (r/->AndSpec (set ds)))))

(defn- one-of-or-single [specs]
  (let [ds (distinct specs)]
    (if (= 1 (count ds))
      (first ds)
      (r/->OneOfSpec (set ds)))))

(defn- create-one-of-from-envs [term envs]
  (->> envs
       (map #(utils/get-dom-of-term % term))
       (map and-or-single)
       one-of-or-single))


(defn- remove-doms [env]
  (reduce #(uber/remove-attr %1 %2 DOM) env (utils/get-terms env)))

(defn- merge-envs [& envs]
  (let [terms (distinct (mapcat utils/get-terms envs))
        new-doms (map #(create-one-of-from-envs % envs) terms)]
    (reduce
     (fn [env [term spec]]
       (-> env
           (uber/add-nodes term)
           (uber/add-attr term DOM [spec])))
     (remove-doms (first envs))
     (map vector terms new-doms))))
