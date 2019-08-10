(ns prolog-analyzer.analyzer.domain
  (:require [prolog-analyzer.utils :as utils :refer [case+]]
            [prolog-analyzer.records :as r]
            [prolog-analyzer.record-utils :as ru]
            [ubergraph.core :as uber]))

(declare add-to-dom)
(declare merge-envs)

(def DOM :dom)

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
  (loop [res env
         terms (vec (utils/get-terms env))]
    (if-let [first-term (first terms)]
      (let [edges (edges first-term)
            new-terms (map first edges)
            queue (vec (rest terms))]
        (recur (apply uber/add-edges res edges) (apply conj queue new-terms)))
      res)))

(defmulti next-steps
  (fn [term spec]
    (case+ (r/safe-spec-type spec (str "next-steps"))
           r/TUPLE :tuple
           r/COMPOUND :compound
           r/USERDEFINED :userdef
           r/LIST :list
           r/GROUND :ground
           :other)))

(defn- split-spec-for-list [spec]
  (case+ (r/safe-spec-type spec "split-spec-for-list")
         r/LIST [(:type spec) spec]
         r/TUPLE [(first (:arglist spec)) (-> spec
                                              (update :arglist rest)
                                              (update :arglist (partial apply vector)))]
         [spec spec]))


(defmethod next-steps :tuple [term spec]
  (if (ru/list-term? term)
    [[(ru/head term) (first (:arglist spec))]
     [(ru/tail term) (update spec :arglist rest)]]
    []))

(defmethod next-steps :list [term spec]
  (if (ru/list-term? term)
    [[(ru/head term) (:type spec)]
     [(ru/tail term) spec]]
    []))

(defmethod next-steps :compound [term spec]
  (cond
    (ru/compound-term? term) (apply vector (map vector (:arglist term) (:arglist spec)))
    (and (ru/list-term? term) (r/incomplete-list-spec? spec)) [[(ru/head term) (first (:arglist spec))]]
    :else []))

(defmethod next-steps :ground [term spec]
  (cond
    (ru/list-term? term) [[(ru/head term) spec]
                          [(ru/tail term) spec]]
    (ru/compound-term? term) (apply vector (map #(vector % spec) (:arglist term)))
    :else []))

(defmethod next-steps :userdef [term spec]
  (let [resolved (ru/resolve-definition-with-parameters spec)]
    [[term resolved]]))

(defmethod next-steps :default [term spec]
  [])

(defn- fully-qualified?
  ([spec]
   (case+ (r/spec-type spec)
          (r/TUPLE, r/LIST, r/COMPOUND) true
          r/OR (every? fully-qualified? (:arglist spec))
          false))
  ([env list-term]
   (let [tail (ru/tail list-term)
         tail-dom (utils/get-dom-of-term env tail)]
     (or (ru/nonvar-term? tail)
         (fully-qualified? tail-dom)))))

(defn next-steps-of-list-term [env term spec]
  (if (or
       (fully-qualified? env term)
       (fully-qualified? spec))
    (next-steps term spec)
    []))

(defn add-steps [env steps initial?]
  (reduce #(apply add-to-dom %1 initial? %2) env steps))

(defmulti process-next-steps (fn [_ _ spec _] (ru/or-spec? spec)))

(defmethod process-next-steps false [env term spec initial?]
  (cond
    (ru/error-spec? spec) env
    (ru/list-term? term) (add-steps env (next-steps-of-list-term env term spec) initial?)
    :ese  (add-steps env (next-steps term spec) initial?)))

(defmethod process-next-steps true [env term spec initial?]
  (let [arglist (:arglist spec)
        envs (map (partial add-to-dom (uber/digraph) initial? term) arglist)
        terms-with-doms (merge-envs envs)]
    (reduce #(apply add-to-dom %1 initial? %2) env terms-with-doms)))


(defn- has-dom? [env term]
  (and (uber/has-node? env term) (uber/attr env term :dom)))

(defn add-to-dom
  ([env term spec]
   (add-to-dom env false term spec))
  ([env initial? term spec]
   (letfn [(intersect-fn [a b] (ru/intersect (or a (r/->AnySpec)) b initial?))]
     (cond
       (not (has-dom? env term))  (-> env
                                      (uber/add-nodes term)
                                      (uber/add-attr term DOM (r/->AnySpec))
                                      (add-to-dom initial? term (r/initial-spec term))
                                      (add-to-dom initial? term spec))
       (ru/and-spec? spec)         (reduce #(add-to-dom %1 initial? term %2) env (:arglist spec))
       (and
        (ru/var-spec? spec)
        (ru/list-term? term))      (add-to-dom env initial? term (r/create-incomplete-list-spec))
       :default                    (let [before (uber/attr env term DOM)
                                         new (intersect-fn before spec)]
                                     (if (= before new)
                                       env
                                       (-> env
                                           (uber/add-attr term DOM new)
                                           (utils/update-attr term :history conj spec)
                                           (process-next-steps term spec initial?))))))))

(-> (uber/digraph)
    (add-to-dom true (r/->ListTerm (r/->IntegerTerm 1) (r/->VarTerm "X")) (r/->VarSpec))
    utils/env->map)

(defn add-to-dom-post-spec [env term spec]
  (add-to-dom env term (ru/replace-var-with-any (or spec (r/->AnySpec)))))


(defn- one-of-or-single [specs]
  (let [ds (distinct specs)]
    (if (= 1 (count ds))
      (first ds)
      (r/->OneOfSpec (set ds)))))

(defn- create-one-of-from-envs [term envs]
  (->> envs
       (map #(utils/get-dom-of-term % term))
       (remove ru/error-spec?)
       set
       r/->OneOfSpec
       ru/simplify))

(defn- merge-envs [envs]
  (let [terms (distinct (mapcat utils/get-terms envs))
        new-doms (map #(create-one-of-from-envs % envs) terms)]
    (map vector terms new-doms)))
