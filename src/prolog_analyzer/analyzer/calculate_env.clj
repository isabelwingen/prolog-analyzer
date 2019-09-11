(ns prolog-analyzer.analyzer.calculate-env
  (:require [prolog-analyzer.analyzer.domain :as dom]
            [prolog-analyzer.utils :as utils :refer [case+]]
            [prolog-analyzer.records :as r]
            [prolog-analyzer.record-utils :as ru]
            [clojure.pprint :refer [pprint]]
            [ubergraph.core :as uber]
            [clojure.tools.logging :as log]
            [prolog-analyzer.state :as state]
            [prolog-analyzer.analyzer.post-specs :as post-specs]
            [clojure.spec.alpha :as s]
            [prolog-analyzer.specs :as specs]
            [orchestra.spec.test :as stest]
            [orchestra.core :refer [defn-spec]]
            ))

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


(defn get-matching-head [tail pair-id env]
  (let [head (some->> env
                      uber/edges
                      (filter #(= :is-head (uber/attr env % :relation)))
                      (filter #(= pair-id (uber/attr env % :pair)))
                      first
                      uber/src)]
    (assert (not (nil? head)) (str (utils/get-title env "a") " " (r/to-string tail)))
    head))

(defmethod process-edge :is-tail [parameters env edge]
  (let [tail (uber/src edge)
        term (uber/dest edge)
        tail-dom (utils/get-dom-of-term env tail)
        term-dom (utils/get-dom-of-term env term)
        head (ru/head term)
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
  (loop [counter 0
         res env]
    (log/trace (utils/format-log env "Post Process - " counter))
    (let [next (post-process-step res parameters)]
      (if (utils/same? next res)
        res
        (recur (inc counter) next)))))

(defn get-env-for-head
  "Calculates an environment from the header terms and the prespec"
  [title arglist pre-spec]
  (log/trace (utils/format-log title "Calculate env for head"))
  (let [parameters {:initial true}]
    (-> (uber/digraph)
        (utils/set-arguments arglist)
        (utils/set-title title)
        (xyzabc (apply ru/to-head-tail-list arglist) pre-spec parameters)
        dom/add-structural-edges
        (post-process parameters)
        )))

(defn- get-env-for-pre-spec-of-subgoal
  "Calculates an environment from a subgoal and its pre-specs"
  [in-env arglist pre-spec]
  (log/trace (utils/format-log in-env "Calculate env for pre spec"))
  (let [parameters {:initial false}]
    (-> in-env
        (xyzabc (apply ru/to-head-tail-list arglist) pre-spec parameters)
        dom/add-structural-edges
        (post-process parameters))))

(defn- get-env-for-post-spec-of-subgoal
  [in-env arglist post-specs]
  (log/trace (utils/format-log in-env "Calculate env for post spec"))
  (let [parameters {:initial false :overwrite true}]
    (-> in-env
        (post-specs/register-post-specs arglist post-specs)
        dom/add-structural-edges
        (post-process parameters))))

(defn mark-self-calling [in-env subgoal-id]
  (let [pred-id (vec (drop-last (utils/get-title in-env "mark-self-calling")))]
    (when (= pred-id subgoal-id)
      (swap! state/self-calling update pred-id #(inc (or % 0))))
    (get @state/self-calling pred-id 0)))

(defn get-env-for-subgoal
  [in-env subgoal-id arglist pre-spec post-specs]
  (log/trace (utils/format-log in-env "Calculate env for subgoal"))
  (if (< (mark-self-calling in-env subgoal-id) 3)
    (-> in-env
        (get-env-for-pre-spec-of-subgoal arglist pre-spec)
        (get-env-for-post-spec-of-subgoal arglist post-specs))
    in-env))

(stest/instrument)
