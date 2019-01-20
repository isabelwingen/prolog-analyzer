(ns prolog-analyzer.analyzer.core
  (:require
   [prolog-analyzer.analyzer.domain :as dom]
   [prolog-analyzer.analyzer.validator :as validator]
   [prolog-analyzer.utils :as utils]
   [ubergraph.core :as uber]
   [ubergraph.protocols]
   [loom.graph]
   [loom.attr]
   [clojure.set]
   [clojure.pprint :as pp]
   ))


(defmulti add-relationships-aux (comp :type second))
(defmethod add-relationships-aux :list [[env {head :head tail :tail :as term}]]
  (if (utils/empty-list? tail)
    (-> env
        (dom/fill-env-for-term-with-spec head {:spec :any})
        (uber/add-edges [head term {:relation :is-head}]))
    (-> env
        (dom/fill-env-for-term-with-spec head {:spec :any})
        (dom/fill-env-for-term-with-spec tail {:spec :list :type {:spec :any}})
        (uber/add-edges [head term {:relation :is-head}] [tail term {:relation :is-tail}]))))

(defmethod add-relationships-aux :default [[env _]]
  env)

(defn add-relationships [env]
  (reduce #(add-relationships-aux [%1 %2]) env (uber/nodes env)))

(defn evaluate-goal [data env {goal-name :goal module :module arity :arity arglist :arglist :as goal}]
  (let [goal-specs (:pre-specs (utils/get-specs-of-pred [module goal-name arity] data))
        [term goal-specs-as-tuple] [(if (= arity 1) (first arglist) (apply utils/to-head-tail-list arglist))
                                    (if (= arity 1) (map first goal-specs) (map (partial apply utils/to-tuple-spec) goal-specs))]]
    (if (> arity 0)
      (if (= 1 (count goal-specs))
        (dom/fill-env-for-term-with-spec env term (first goal-specs-as-tuple))
        (dom/fill-env-for-term-with-spec env term (apply utils/to-or-spec goal-specs-as-tuple)))
      env)))

(defn evaluate-body [env body data]
  (reduce (partial evaluate-goal data) env body))

(defn- add-index-to-input-arguments [env arglist]
  (apply uber/add-nodes-with-attrs env (map-indexed #(vector %2 {:index %1}) arglist)))

(defn initial-env [arglist pre-spec]
  (let [step1 (reduce #(apply dom/fill-env-for-term-with-spec %1 %2) (uber/digraph) (map vector arglist pre-spec))
        step2 (apply uber/add-nodes-with-attrs step1 (map-indexed #(vector %2 {:index %1}) arglist))
        step3 (add-index-to-input-arguments step2 arglist)]
    step3))


(defn analyzing [{arglist :arglist body :body :as clause} pre-spec data]
  (-> (initial-env arglist pre-spec)
      (evaluate-body body data)
      (add-relationships)
      ))

(defn complete-analysis [data]
  (for [pred-id (utils/get-pred-identities data)
        clause-id (utils/get-clause-identities-of-pred pred-id data)
        pre-spec (:pre-specs (utils/get-specs-of-pred pred-id data))]
    [[clause-id pre-spec] (analyzing (utils/get-clause clause-id data) pre-spec data)]))

