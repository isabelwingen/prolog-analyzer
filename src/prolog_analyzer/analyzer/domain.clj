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
            [ubergraph.core :as uber]
            ))

(declare add-to-dom)

(def DOM :dom)
(def HIST :history)


(defn-spec ^:private execute-step ::specs/env
  [callback-fn any?,
   simplify-fn any?,
   env ::specs/env,
   [term spec] ::specs/step]
  (callback-fn env term (simplify-fn spec)))

(defn-spec ^:private process-next-steps ::specs/env
  [env ::specs/env
   term ::specs/term
   spec ::specs/spec
   callback-fn any?
   simplify-fn any?]
  (reduce
   (partial execute-step callback-fn simplify-fn)
   env
   (next-steps/get-steps env term (simplify-fn spec))))

(defn-spec ^:private has-dom? boolean?
  [env ::specs/env, term ::specs/term]
  (if (and (uber/has-node? env term) (uber/attr env term :dom))
    true
    false))

(defn-spec ^:private first-add ::specs/env
  [env ::specs/env
   term ::specs/term
   spec ::specs/spec
   callback-fn any?]
  (let [initial-spec (ru/initial-spec term)]
    (if (= initial-spec spec)
      (-> env
          (uber/add-nodes term)
          (uber/add-attr term DOM (r/->AnySpec))
          (uber/add-attr term HIST (hash-set))
          (callback-fn term spec))
      (-> env
          (uber/add-nodes term)
          (uber/add-attr term DOM (r/->AnySpec))
          (uber/add-attr term HIST (hash-set))
          (callback-fn term (ru/initial-spec term))
          (callback-fn term spec)))))

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
  (contains? (uber/attr env term HIST) new))

(defn-spec ^:private create-incomplete-list-spec ::specs/spec
  ([] (create-incomplete-list-spec (r/->AnySpec)))
  ([head-dom ::specs/spec]
   (r/->CompoundSpec "." [head-dom (r/->AnySpec)])))

(defn- check-for-errors [env term new-type]
  (if (ru/error-spec? new-type)
    (utils/store-error-reason env term)
    env))


(defn add-to-dom
  ([env term spec]
   (add-to-dom env false false term spec))
  ([env initial? overwrite? term spec]
   (let [callback (fn [env term spec] (add-to-dom env initial? overwrite? term spec))
         intersect-fn (fn [a b]
                        (let [left (or a (r/->AnySpec))]
                          (if overwrite?
                            (ru/intersect (ru/replace-var-with-any left) b initial?)
                            (ru/intersect left b initial?))))
         simplify-fn #(ru/simplify % initial?)]
     (cond
       (not (has-dom? env term))     (first-add env term spec callback)
       (ru/and-spec? spec)           (reduce #(callback %1 term %2) env (:arglist spec))
       (incomplete-list? spec term)  (callback env term (create-incomplete-list-spec))
       :default                      (let [before (uber/attr env term DOM)
                                           new (intersect-fn before spec)]
                                       (if
                                           (or
                                            (= before new)
                                            (old? new env term))
                                         env
                                         (-> env
                                             (uber/add-attr term DOM new)
                                             (utils/update-attr term HIST conj new)
                                             (check-for-errors term new)
                                             (process-next-steps term new callback simplify-fn))))))))

(defn-spec add-to-dom-post-spec ::specs/env
  "Adds a spec obtained from a postspec to the environment"
  [env ::specs/env, term ::specs/term, spec ::specs/spec]
  (add-to-dom env false true term spec))

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
