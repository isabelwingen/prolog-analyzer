(ns prolog-analyzer.analyzer.next-steps
  (:require
   [prolog-analyzer.utils :as utils :refer [case+]]
   [prolog-analyzer.records :as r]
   [clojure.tools.logging :as log]
   [prolog-analyzer.record-utils :as ru]
   [ubergraph.core :as uber]
   [clojure.spec.alpha :as s]
   [prolog-analyzer.specs :as specs]
   [orchestra.spec.test :as stest]
   [orchestra.core :refer [defn-spec]]
   ))


(declare next-steps)

(s/fdef process-or
  :args (s/cat :term ::specs/term :spec ::specs/spec)
  :ret ::specs/steps)

(defmulti process-or (fn [term spec] (ru/term-type term)))

(defmethod process-or :list [term spec]
  (let [{head :head tail :tail} term
        {grounds :ground lists :list tuples :tuple compounds :compound} (group-by ru/spec-type (:arglist spec))
        terms [head tail]
        build-arglists #(->> % (apply map vector) (interleave terms) (apply hash-map))
        ground-steps (if (nil? grounds)
                       {}
                       (build-arglists [[(r/->GroundSpec) (r/->GroundSpec)]]))
        list-steps (if (nil? lists)
                     {}
                     (->> lists
                          (map #(vector (:type %) %))
                          build-arglists))
        tuple-steps (if (nil? tuples)
                      {}
                      (->> tuples
                           (map #(vector (-> % :arglist first) (update % :arglist rest)))
                           build-arglists))
        compound-steps (if (nil? compounds)
                         {}
                         (->> compounds
                              (map :arglist)
                              (map #(vector (first %) (second %)))
                              build-arglists))]
    (->> (merge-with concat list-steps ground-steps tuple-steps compound-steps)
         (reduce-kv #(assoc %1 %2 (r/->OneOfSpec (set %3))) {})
         (map identity)
         vec)))

(defmethod process-or r/COMPOUND [term spec]
  (let [terms (:arglist term)
        arity (count terms)
        {grounds :ground compounds :compound} (group-by ru/spec-type (:arglist spec))
        build-arglists #(->> % (apply map vector) (interleave terms) (apply hash-map))
        ground-steps (if (nil? grounds) {} (build-arglists [(repeat arity (r/->GroundSpec))]))
        compound-steps (if (nil? compounds)
                         {}
                         (->> compounds
                              (map :arglist)
                              (build-arglists)))]
    []
    (->> (merge-with concat ground-steps compound-steps)
         (reduce-kv #(assoc %1 %2 (r/->OneOfSpec (set %3))) {})
         (map identity)
         vec)))

(defmethod process-or :default [term spec]
  [])

(s/fdef next-steps
  :args (s/cat
         :term ::specs/term
         :spec ::specs/spec)
  :ret ::specs/steps)

(defmulti ^{:private true} next-steps
  (fn [term spec]
    (case+ (ru/spec-type spec)
           r/TUPLE :tuple
           r/COMPOUND :compound
           r/USERDEFINED :userdef
           r/LIST :list
           r/GROUND :ground
           r/OR :or
           :other)))

(defn- split-spec-for-list [spec]
  (case+ (ru/spec-type spec)
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

(defmethod next-steps :or [term spec]
  (process-or term spec))

(defmethod next-steps :default [term spec]
  [])

(defn- fully-qualified-spec? [spec]
  (case+ (ru/spec-type spec)
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


(defn-spec get-steps ::specs/steps
  [env ::specs/env,
   term ::specs/term,
   spec ::specs/spec]
  (cond
    (ru/error-spec? spec) []
    (ru/list-term? term) (next-steps-of-list-term env term spec)
    :else (next-steps term spec)))


(stest/instrument)
