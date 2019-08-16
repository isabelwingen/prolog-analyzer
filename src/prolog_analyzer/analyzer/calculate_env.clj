(ns prolog-analyzer.analyzer.calculate-env
  (:require [prolog-analyzer.analyzer.domain :as dom]
            [prolog-analyzer.utils :as utils :refer [case+]]
            [prolog-analyzer.records :as r]
            [prolog-analyzer.record-utils :as ru]
            [clojure.pprint :refer [pprint]]
            [ubergraph.core :as uber]
            [prolog-analyzer.analyzer.post-specs :as post-specs]))

(defn xyzabc [env term spec {initial? :initial overwrite? :overwrite}]
  (if overwrite?
    (dom/add-to-dom-post-spec env term spec)
    (dom/add-to-dom env initial? term spec)))

(defmulti ^{:private true} process-edge (fn [_ env edge] (uber/attr env edge :relation)))

(defn- compatible-with-head [{initial? :initial :as parameters} head-dom term-dom]
  (case+ (r/safe-spec-type term-dom "compatible-with-head")
         r/TUPLE (let [new-dom (update term-dom :arglist #(assoc % 0 head-dom))]
                   (if (ru/non-empty-intersection new-dom term-dom initial?)
                     new-dom
                     nil))
         r/OR (let [new-dom (-> term-dom
                                (update :arglist (partial filter (partial compatible-with-head parameters head-dom)))
                                (update :arglist set)
                                (ru/simplify initial?))]
                (if (ru/non-empty-intersection new-dom term-dom initial?)
                  new-dom
                  nil))
         r/LIST (if (ru/non-empty-intersection head-dom (:type term-dom) initial?)
                  term-dom
                  nil)
         (if (ru/non-empty-intersection (r/->ListSpec head-dom) term-dom initial?)
           term-dom
           nil)))


(defn- singleton-list? [term]
  (and (ru/empty-list-term? (ru/tail term))))

(defmethod process-edge :is-head [parameters env edge]
  (let [head (uber/src edge)
        term (uber/dest edge)
        head-dom (utils/get-dom-of-term env head)
        term-dom (utils/get-dom-of-term env term)
        filtered-dom (compatible-with-head parameters head-dom term-dom)]
    (if (singleton-list? term)
      (xyzabc env term (r/->TupleSpec [head-dom]) parameters)
      (xyzabc env term (or filtered-dom (r/DISJOINT)) parameters))))

(def e
  (-> (uber/digraph)
      (uber/add-edges [(r/->VarTerm "X") (ru/to-head-tail-list (r/->VarTerm "X") (r/->VarTerm "Y")) {:relation :is-head}])
      (uber/add-nodes-with-attrs
       [(r/->VarTerm "X") {:dom (r/->AtomSpec)}]
       [(ru/to-head-tail-list (r/->VarTerm "X") (r/->VarTerm "Y")) {:dom (r/->OneOfSpec #{(r/->TupleSpec [(r/->AtomSpec) (r/->IntegerSpec)])
                                                                                                        (r/->TupleSpec [(r/->IntegerSpec) (r/->AtomSpec)])})}])))

(defn get-matching-head [pair-id env]
  (let [head (some->> env
                      uber/edges
                      (filter #(= :is-head (uber/attr env % :relation)))
                      (filter #(= pair-id (uber/attr env % :pair)))
                      first
                      uber/src)]
    (assert (not (nil? head)))
    head))

(defmethod process-edge :is-tail [parameters env edge]
  (let [tail (uber/src edge)
        term (uber/dest edge)
        tail-dom (utils/get-dom-of-term env tail)
        term-dom (utils/get-dom-of-term env term)
        pair-id (uber/attr env edge :pair)
        head (get-matching-head pair-id env)
        head-dom (utils/get-dom-of-term env head)
        new-dom (case+ (r/safe-spec-type tail-dom "process-tail")
                       r/TUPLE (update tail-dom :arglist #(->> %
                                                               (cons head-dom)
                                                               (apply vector)))
                       r/LIST (update tail-dom :type #(r/->OneOfSpec (hash-set % head-dom)))
                       term-dom)]
    (xyzabc env term new-dom parameters)))


(defmethod process-edge :arg-at-pos [parameters env edge]
  (let [child (uber/src edge)
        child-dom (utils/get-dom-of-term env child)
        parent (uber/dest edge)
        functor (:functor parent)
        args (count (:arglist parent))
        pos (uber/attr env edge :pos)
        new-dom (->> (r/->AnySpec)
                     (repeat args)
                     (apply vector)
                     (#(assoc % pos child-dom))
                     (apply vector)
                     (r/->CompoundSpec functor))]
    (xyzabc env parent new-dom parameters)))

(defmethod process-edge :default [defs env edge]
  env)

(defn process-edges [env parameters]
  (reduce (partial process-edge parameters) env (uber/edges env)))

(defn process-post-specs [env parameters]
  (reduce (fn [e [term spec]] (xyzabc e term spec (assoc parameters :overwrite true))) env (post-specs/get-next-steps-from-post-specs env)))

(defn- post-process-step [env parameters]
  (-> env
      (process-edges parameters)
      (process-post-specs parameters)))

(defn- post-process [env parameters]
  (loop [res env]
    (let [next (post-process-step res parameters)]
      (if (utils/same? next res)
        res
        (recur next)))))

(defn- add-args-of-head [env arglist]
  (-> env
      (uber/add-nodes :environment)
      (uber/add-attr :environment :arglist arglist)))

(defn- add-title [env title]
  (-> env
      (uber/add-nodes :environment)
      (uber/add-attr :environment :title title)))

(defn get-env-for-head
  "Calculates an environment from the header terms and the prespec"
  [title arglist pre-spec]
  (let [parameters {:initial true}]
    (-> (uber/digraph)
        (add-args-of-head arglist)
        (add-title title)
        (xyzabc (apply ru/to-head-tail-list arglist) pre-spec parameters)
        dom/add-structural-edges
        (post-process parameters)
        )))

(defn- get-env-for-pre-spec-of-subgoal
  "Calculates an environment from a subgoal and its pre-specs"
  [in-env arglist pre-spec]
  (let [parameters {:initial false}]
    (-> in-env
        (xyzabc (apply ru/to-head-tail-list arglist) pre-spec parameters)
        dom/add-structural-edges
        (post-process parameters))))

(defn- get-env-for-post-spec-of-subgoal
  [in-env arglist post-specs]
  (let [parameters {:initial false :overwrite true}]
    (-> in-env
        (post-specs/register-post-specs arglist post-specs)
        dom/add-structural-edges
        (post-process parameters))))

(defn get-env-for-subgoal
  [in-env arglist pre-spec post-specs]
  (-> in-env
      (get-env-for-pre-spec-of-subgoal arglist pre-spec)
      (get-env-for-post-spec-of-subgoal arglist post-specs)))
