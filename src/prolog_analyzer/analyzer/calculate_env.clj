(ns prolog-analyzer.analyzer.calculate-env
  (:require [clojure.spec.alpha :as s]
            [orchestra.core :refer [defn-spec]]
            [orchestra.spec.test :as stest]
            [clojure.tools.logging :as log]
            [prolog-analyzer.analyzer.domain :as dom]
            [prolog-analyzer.analyzer.post-specs :as post-specs]
            [prolog-analyzer.record-utils :as ru]
            [prolog-analyzer.records :as r]
            [prolog-analyzer.state :as state]
            [prolog-analyzer.utils :as utils :refer [case+]]
            [ubergraph.core :as uber]
            [prolog-analyzer.specs :as specs]))

(def ^:private DEEPNESS 3)

(defn ^:private add-to-dom [env term spec {initial? :initial overwrite? :overwrite}]
  (if overwrite?
    (dom/add-to-dom-post-spec env term spec)
    (dom/add-to-dom env initial? false term spec)))

(defn ^:private singleton-list? [term]
  (and (ru/empty-list-term? (ru/tail term))))

(defn ^:private deepness [spec]
  (case+ (ru/spec-type spec)
         (r/USERDEFINED, r/COMPOUND, r/TUPLE) (->> spec
                                                   :arglist
                                                   (map deepness)
                                                   (apply max 0)
                                                   inc)
         r/OR (->> spec
                   :arglist
                   (map deepness)
                   (apply max 0))
         r/LIST (-> spec :type deepness inc)
         1))
(defmulti ^:private process-edge (fn [_ env edge] (uber/attr env edge :relation)))

(defmethod process-edge :is-tail [parameters env edge]
  (let [tail (uber/src edge)
        term (uber/dest edge)
        tail-dom (utils/get-dom-of-term env tail)
        term-dom (utils/get-dom-of-term env term)
        head (ru/head term)
        head-dom (utils/get-dom-of-term env head)
        new-dom (case+ (ru/spec-type tail-dom)
                       r/TUPLE (update tail-dom :arglist #(->> %
                                                               (cons head-dom)
                                                               (apply vector)))
                       r/LIST (update tail-dom :type #(r/->OneOfSpec (hash-set % head-dom)))
                       term-dom)]
    (add-to-dom env term new-dom parameters)))

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
    (if (> (deepness new-dom) DEEPNESS)
      env
      (add-to-dom env parent new-dom parameters))))

(defmethod process-edge :default [defs env edge]
  env)

(defn ^:private process-edges [env parameters]
  (reduce (partial process-edge parameters) env (uber/edges env)))

(defn ^:private process-post-specs [env parameters]
  (let [{new-env :new-env steps :steps} (post-specs/apply-post-specs env)]
    (reduce (fn [e [term spec]] (add-to-dom e term spec (assoc parameters :overwrite true))) new-env steps)))

(defn ^:private post-process-step [env parameters]
  (-> env
      (process-edges parameters)
      (process-post-specs parameters)))

(defn ^:private post-process [env parameters]
  (loop [counter 0
         res env]
    (log/trace (utils/format-log env "Post Process - " counter))
    (let [next (post-process-step res parameters)]
      (if (utils/same? next res)
        res
        (recur (inc counter) next)))))

(defn-spec get-env-for-head ::specs/env
  "Calculates an environment from the header terms and the prespecs"
  [clause-id ::specs/clause-id, arglist ::specs/arglist, prespec-as-spec ::specs/spec]
  (log/trace (utils/format-log clause-id "Calculate env for head"))
  (let [parameters {:initial true}]
    (-> (uber/digraph)
        (utils/set-arguments arglist)
        (utils/set-title clause-id)
        (utils/change-current-step :head)
        (add-to-dom (apply ru/to-head-tail-list arglist) prespec-as-spec parameters)
        dom/add-structural-edges
        (post-process parameters)
        )))

(defn-spec ^:private get-env-for-pre-spec-of-subgoal ::specs/env
  "Takes an environment and adds the information from the prespecs"
  [in-env ::specs/env, arglist ::specs/arglist, prespec-as-spec ::specs/spec]
  (log/trace (utils/format-log in-env "Calculate env for pre spec"))
  (let [parameters {:initial false}]
    (-> in-env
        (add-to-dom (apply ru/to-head-tail-list arglist) prespec-as-spec parameters)
        dom/add-structural-edges
        (post-process parameters))))

(defn-spec ^:private get-env-for-post-spec-of-subgoal ::specs/env
  "Takes an environment and adds the information from the postspecs"
  [in-env ::specs/env, arglist ::specs/arglist, post-specs ::specs/post-specs]
  (log/trace (utils/format-log in-env "Calculate env for post spec"))
  (let [parameters {:initial false :overwrite true}]
    (-> in-env
        (post-specs/register-post-specs arglist post-specs)
        dom/add-structural-edges
        (post-process parameters))))

(defn-spec get-env-for-subgoal ::specs/env
  "Takes an environment and adds the information gained from the given subgoal."
  [in-env ::specs/env,
   subgoal-id ::specs/pred-id,
   arglist ::specs/arglist,
   prespec-as-spec ::specs/spec,
   post-specs ::specs/post-specs]
  (log/trace (utils/format-log in-env "Calculate env for subgoal"))
  (-> in-env
      (utils/change-current-step subgoal-id)
      (get-env-for-pre-spec-of-subgoal arglist prespec-as-spec)
      (get-env-for-post-spec-of-subgoal arglist post-specs)))
