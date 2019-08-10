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
           r/LIST :list
           r/USERDEFINED :userdef
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

(defmethod next-steps :compound [term spec]
  (if (ru/compound-term? term)
    (apply vector (map vector (:arglist term) (:arglist spec)))
    []))

(defmethod next-steps :list [term spec]
  (if (ru/list-term? term)
    [[(ru/head term) (:type spec)]
     [(ru/tail term) spec]]
    []))

(defmethod next-steps :ground [term spec]
  (cond
    (ru/list-term? term) [[(ru/head term) spec]
                          [(ru/tail term) spec]]
    (ru/compound-term? term) (apply vector (map #(vector % spec) (:arglist term)))
    :else []))

(defmethod next-steps :userdef [term spec]
  (let [resolved (ru/resolve-definition-with-parameters spec)]
    [term resolved]))

(defmethod next-steps :default [term spec]
  [])

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
        envs (map (partial add-to-dom (uber/digraph) intersect-fn term) arglist)
        terms-with-doms (apply merge-envs intersect-fn envs)]
    (reduce #(apply add-to-dom %1 intersect-fn %2) env terms-with-doms)))

(defn debug [term spec]
  (if (or (nil? term) (nil? spec))
    nil
    (println "add-to-dom for term " (r/to-string term) " and " (r/to-string spec))))

(defn has-dom? [env term]
  (and (uber/has-node? env term) (uber/attr env term :dom)))

(defn add-to-dom [env intersect-fn term spec]
  (cond
    (not (has-dom? env term))  (-> env
                                   (uber/add-nodes term)
                                   (uber/add-attr term DOM (r/->AnySpec))
                                   (add-to-dom intersect-fn term (r/initial-spec term))
                                   (add-to-dom intersect-fn term spec))
    (ru/and-spec? spec)         (reduce #(add-to-dom %1 intersect-fn term %2) env (:arglist spec))
    :default                    (let [before (uber/attr env term DOM)
                                      new (intersect-fn before spec)]
                                  (if (= before new)
                                    env
                                    (-> env
                                        (uber/add-attr term DOM new)
                                        (utils/update-attr term :history conj spec)
                                        (process-next-steps intersect-fn term spec))))))


#_(-> (uber/digraph)
    (add-to-dom intersect-with-initial
                (r/->VarTerm "E")
                (r/->VarSpec))
    (uber/attr (r/->VarTerm "E") :dom))

#_(-> (uber/digraph)
    (add-to-dom intersect-with-initial
                (ru/to-head-tail-list (r/->VarTerm "E"))
                (r/->OneOfSpec #{(r/->TupleSpec [(r/->VarSpec)])}))
    (uber/attr (r/->VarTerm "E") :dom))

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

(defn- merge-envs [intersect-fn & envs]
  (let [terms (distinct (mapcat utils/get-terms envs))
        new-doms (map #(create-one-of-from-envs % envs) terms)]
    (map vector terms new-doms)))

(defn intersect-with-initial [a b]
  (ru/intersect (or a (r/->AnySpec)) b true))

(defn intersect-with-overwrite [a b]
  (ru/intersect (ru/replace-var-with-any (or a (r/->AnySpec))) b false))

(defn intersect [a b]
  (ru/intersect (or a (r/->AnySpec)) b false))
