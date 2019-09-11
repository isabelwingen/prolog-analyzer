(ns prolog-analyzer.analyzer.domain
  (:require [prolog-analyzer.utils :as utils :refer [case+]]
            [prolog-analyzer.records :as r]
            [clojure.tools.logging :as log]
            [prolog-analyzer.record-utils :as ru]
            [ubergraph.core :as uber]
            [clojure.spec.alpha :as s]
            [prolog-analyzer.specs :as specs]
            [orchestra.spec.test :as stest]
            [orchestra.core :refer [defn-spec]]
            ))

(declare add-to-dom)
(declare merge-envs)

(def DOM :dom)
(def HIST :history)

(s/fdef next-steps
  :args (s/cat
         :term ::specs/term
         :spec ::specs/spec)
  :ret (s/coll-of (s/tuple ::specs/term ::specs/spec)))

(defmulti ^{:private true} next-steps
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
  []
  (let [resolved (ru/resolve-definition-with-parameters spec)]
    [[term resolved]]))

(defmethod next-steps :default [term spec]
  [])

(defn- fully-qualified-spec? [spec]
  (case+ (r/spec-type spec)
         (r/TUPLE, r/LIST, r/COMPOUND) true
         r/OR (every? fully-qualified-spec? (:arglist spec))
         false))

(defn- fully-qualified-term? [env term]
  (if (ru/empty-list-term? term)
    true
    (if (ru/list-term? term)
      (recur env (ru/tail term))
      false)))

(defn- fully-qualified? [env term spec]
  (or
   (fully-qualified-term? env term)
   (fully-qualified-spec? spec)))

(defn- next-steps-of-list-term [env term spec]
  (if (or
       (fully-qualified-term? env term)
       (fully-qualified-spec? spec))
    (next-steps term spec)
    []))

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

(s/fdef add-steps
  :args (s/cat
         :env utils/is-graph?
         :steps (s/coll-of (s/tuple ::specs/term ::specs/spec))
         :initial? boolean?)
  :ret utils/is-graph?)

(defn- add-steps [env steps initial?]
  (reduce #(apply add-to-dom %1 initial? %2) env steps))

(defmulti ^{:private true} process-next-steps (fn [_ _ spec _] (ru/or-spec? spec)))

(defmethod process-next-steps false [env term spec initial?]
  (log/trace (utils/format-log env "process-next-steps - not or"))
  (cond
    (ru/error-spec? spec) env
    (ru/list-term? term) (add-steps env (next-steps-of-list-term env term spec) initial?)
    :else  (add-steps env (next-steps term spec) initial?)))

(defmethod process-next-steps true [env term spec initial?]
  (log/trace (utils/format-log env "process-next-steps - or"))
  (let [arglist (:arglist spec)
        envs (map (partial add-to-dom env initial? term) arglist)
        terms-with-doms (->> envs
                             merge-envs
                             (remove (fn [[a b]] (= a term))))]
    (reduce #(apply add-to-dom %1 initial? %2) env terms-with-doms)))

(defn- has-dom? [env term]
  (and (uber/has-node? env term) (uber/attr env term :dom)))


(defn- first-add [env initial? term spec]
  (-> env
      (uber/add-nodes term)
      (uber/add-attr term DOM (r/->AnySpec))
      (add-to-dom initial? term (r/initial-spec term))
      (add-to-dom initial? term spec)))

(defn- incomplete-list? [spec term]
  (and
   (not (fully-qualified-spec? spec))
   (ru/list-term? term)))

(defn- old? [new env term]
  (contains? (uber/attr env term HIST) new))

(defn create-incomplete-list-spec
  ([] (create-incomplete-list-spec (r/->AnySpec)))
  ([head-dom]
   (r/->CompoundSpec "." [head-dom (r/->AnySpec)])))


(defn-spec add-to-dom utils/is-graph?
  ([env utils/is-graph?, term ::specs/term spec ::specs/spec]
   (add-to-dom env false term spec))
  ([env utils/is-graph?,
    initial? boolean?,
    term ::specs/term,
    spec ::specs/spec]
   (letfn [(intersect-fn [a b]
             (let [res (ru/intersect (or a (r/->AnySpec)) b initial?)]
               res))]
     (cond
       (not (has-dom? env term))      (first-add env initial? term spec)
       (ru/and-spec? spec)            (reduce #(add-to-dom %1 initial? term %2) env (:arglist spec))
       (incomplete-list? spec term)   (add-to-dom env initial? term (create-incomplete-list-spec))
       :default                       (let [before (uber/attr env term DOM)
                                            new (intersect-fn before spec)]
                                        (if
                                            (or
                                             (= before new)
                                             (old? new env term))
                                          env
                                          (-> env
                                              (uber/add-attr term DOM new)
                                              (utils/update-attr term HIST conj new)
                                              (utils/update-attr term HIST set)
                                              (process-next-steps term new initial?)
                                              )))))))

(defn add-to-dom-post-spec [env term spec]
  (add-to-dom env term (ru/replace-var-with-any (or spec (r/->AnySpec)))))



;;; Add structural Edges
(s/fdef edges
  :args (s/cat :term ::specs/term)
  :ret (s/coll-of (s/tuple ::specs/term ::specs/term map?)))

(defmulti ^{:private true} edges (fn [term] (r/term-type term)))

(defmethod edges :list [term]
  (let [pair (gensym)]
    [[(ru/head term) term {:relation :is-head :pair pair}]
     [(ru/tail term) term {:relation :is-tail :pair pair}]]))

(defmethod edges :compound [term]
  (map-indexed #(vector %2 term {:relation :arg-at-pos :pos %1}) (:arglist term)))

(defmethod edges :default [term]
  [])

(defn add-structural-edges [env]
  (log/trace "add structural edges")
  (loop [res env
         terms (vec (utils/get-terms env))]
    (if-let [first-term (first terms)]
      (let [edges (edges first-term)
            new-terms (map first edges)
            queue (vec (rest terms))]
        (recur (apply uber/add-edges res edges) (apply conj queue new-terms)))
      res)))

(stest/instrument)
