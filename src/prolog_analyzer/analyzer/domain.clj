(ns prolog-analyzer.analyzer.domain
  (:require [clojure.spec.alpha :as s]
            [clojure.tools.logging :as log]
            [flatland.ordered.set :refer [ordered-set]]
            [orchestra.core :refer [defn-spec]]
            [prolog-analyzer.analyzer.next-steps :as next-steps]
            [prolog-analyzer.record-utils :as ru]
            [prolog-analyzer.records :as r]
            [prolog-analyzer.specs :as specs]
            [prolog-analyzer.utils :as utils :refer [case+]]
            [ubergraph.core :as uber]))

(declare add-to-dom)

(def DOM :dom)
(def HIST :history)

(defn-spec ^:private execute-step ::specs/env
  [initial? boolean?,
   env ::specs/env
   [term spec] ::specs/step]
  (add-to-dom env initial? term (ru/simplify spec initial?)))

(defn-spec ^:private process-next-steps ::specs/env
  [env ::specs/env
   term ::specs/term
   spec ::specs/spec
   initial? boolean?]
  (reduce
   (partial execute-step initial?)
   env
   (next-steps/get-steps env term (ru/simplify spec initial?))))

(defn-spec ^:private has-dom? boolean?
  [env ::specs/env, term ::specs/term]
  (and (uber/has-node? env term) (uber/attr env term :dom) true))

(defn-spec ^:private first-add ::specs/env
  [env ::specs/env
   initial? boolean?
   term ::specs/term
   spec ::specs/spec]
  (let [initial-spec (ru/initial-spec term)]
    (if (= initial-spec spec)
      (-> env
          (uber/add-nodes term)
          (uber/add-attr term DOM (r/->AnySpec))
          (uber/add-attr term HIST (ordered-set))
          (add-to-dom initial? term spec))
      (-> env
          (uber/add-nodes term)
          (uber/add-attr term DOM (r/->AnySpec))
          (uber/add-attr term HIST (ordered-set))
          (add-to-dom initial? term (ru/initial-spec term))
          (add-to-dom initial? term spec)))))

(defn-spec ^:private fully-qualified-spec? boolean?
  [spec ::specs/spec]
  (case+ (ru/spec-type spec)
         (r/TUPLE, r/LIST, r/COMPOUND) true
         r/OR (every? fully-qualified-spec? (:arglist spec))
         false))

(defn-spec ^:private incomplete-list? boolean?
  [spec ::specs/spec, term ::specs/term]
  (and
   (not (fully-qualified-spec? spec))
   (ru/list-term? term)))

(defn-spec ^:private old? boolean?
  [new ::specs/spec, env ::specs/env, term ::specs/term]
  (contains? (apply hash-set (uber/attr env term HIST)) new))

(defn-spec ^:private create-incomplete-list-spec ::specs/spec
  ([] (create-incomplete-list-spec (r/->AnySpec)))
  ([head-dom ::specs/spec]
   (r/->CompoundSpec "." [head-dom (r/->AnySpec)])))


(defn-spec add-to-dom ::specs/env
  "Adds a spec to a domain of a term and executes cascading steps"
  ([env ::specs/env,
    term ::specs/term
    spec ::specs/spec]
   (add-to-dom env false term spec))
  ([env ::specs/env,
    initial? boolean?,
    term ::specs/term
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
                                              (process-next-steps term new initial?)
                                              )))))))

(defn-spec add-to-dom-post-spec ::specs/env
  "Adds a spec obtained from a postspec to the environment"
  [env ::specs/env, term ::specs/term, spec ::specs/spec]
  (add-to-dom env term (ru/replace-var-with-any (or spec (r/->AnySpec)))))


;;; Add structural Edges
(s/fdef edges
  :args (s/cat :term ::specs/term)
  :ret (s/coll-of (s/tuple ::specs/term ::specs/term map?)))

(defmulti ^:private edges (fn [term] (ru/term-type term)))

(defmethod edges :list [term]
  (let [pair (gensym)]
    [[(ru/head term) term {:relation :is-head :pair pair}]
     [(ru/tail term) term {:relation :is-tail :pair pair}]]))

(defmethod edges :compound [term]
  (map-indexed #(vector %2 term {:relation :arg-at-pos :pos %1}) (:arglist term)))

(defmethod edges :default [term]
  [])

(defn-spec add-structural-edges ::specs/env
  "Adds relationships between compound terms and their children"
  [env ::specs/env]
  (log/trace "add structural edges")
  (loop [res env
         terms (vec (utils/get-terms env))]
    (if-let [first-term (first terms)]
      (let [edges (edges first-term)
            new-terms (map first edges)
            queue (vec (rest terms))]
        (recur (apply uber/add-edges res edges) (apply conj queue new-terms)))
      res)))
