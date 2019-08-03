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
     (case+ (r/safe-spec-type spec (str "next-steps"))
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

(defmulti process-next-steps (fn [_ _ _ spec] (ru/or-spec? spec)))

(defmethod process-next-steps false [env intersect-fn term spec]
  (if (ru/error-spec? spec)
    env
    (let [steps (next-steps term spec)]
      (reduce
       #(apply add-to-dom %1 intersect-fn %2)
       env
       steps))))

(defmethod process-next-steps true [env intersect-fn term spec]
  (let [arglist (:arglist spec)
        envs (map (partial add-to-dom env intersect-fn term) arglist)
        terms-with-doms (apply merge-envs intersect-fn envs)]
    (reduce #(apply add-to-dom %1 intersect-fn %2) env terms-with-doms)))

(defn debug [term spec]
  (if (or (nil? term) (nil? spec))
    nil
    (println "add-to-dom for term " (r/to-string term) " and " (r/to-string spec))))


(defn add-to-dom [env intersect-fn term spec]
  (cond
    (not (uber/has-node? env term))  (-> env
                                         (uber/add-nodes term)
                                         (add-to-dom intersect-fn term (r/initial-spec term))
                                         (add-to-dom intersect-fn term spec))
    (ru/and-spec? spec)              (reduce #(add-to-dom %1 intersect-fn term %2) env (:arglist spec))
    :default                         (let [before (uber/attr env term DOM)
                                           new (intersect-fn before spec)]
                                       (if (= before new)
                                         env
                                         (-> env
                                             (uber/add-attr term DOM new)
                                             (process-next-steps intersect-fn term spec))))))




(defn- one-of-or-single [specs]
  (let [ds (distinct specs)]
    (if (= 1 (count ds))
      (first ds)
      (r/->OneOfSpec (set ds)))))

(defn- create-one-of-from-envs [term envs]
  (->> envs
       (map #(utils/get-dom-of-term % term))
       (remove ru/error-spec?)
       one-of-or-single))

(defn- merge-envs [intersect-fn & envs]
  (let [terms (distinct (mapcat utils/get-terms envs))
        new-doms (map #(create-one-of-from-envs % envs) terms)]
    (map vector terms new-doms)))

(defn intersect-with-initial [defs]
  #(ru/intersect (or %1 (r/->AnySpec)) %2 defs true))

(defn intersect-with-overwrite [defs]
  (fn [current-dom new-dom]
    (ru/intersect (ru/replace-var-with-any (or current-dom (r/->AnySpec))) new-dom defs false)))

(defn intersect [defs]
  #(ru/intersect (or %1 (r/->AnySpec)) %2 defs false))
