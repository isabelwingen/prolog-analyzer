(ns prolog-analyzer.analyzer.core
  (:require
   [prolog-analyzer.parser :refer [process-prolog-file process-prolog-snippets process-prolog-files]] ;; used only during development
   [prolog-analyzer.analyzer.domain :as dom]
   [prolog-analyzer.analyzer.validator :as validator]
   [prolog-analyzer.utils :as utils]
   [prolog-analyzer.analyzer.pretty-printer :as my-pp]
   [ubergraph.core :as uber]
   [ubergraph.protocols]
   [loom.graph]
   [loom.attr]
   [clojure.set]
   [clojure.pprint :as pp]
   ))

(def data (atom {}))

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

(defmethod add-relationships-aux :compound [[env {functor :functor arglist :arglist :as term}]]
  (apply
   uber/add-edges
   (dom/fill-env-for-terms-with-specs env arglist (repeat (count arglist) {:spec :any}))
   (map-indexed #(vector %2 term {:relation :arg-at-pos :pos %1}) arglist)))

(defmethod add-relationships-aux :default [[env _]]
  env)

(defn add-relationships [env]
  (reduce #(add-relationships-aux [%1 %2]) env (uber/nodes env)))

(defn evaluate-goal [env {goal-name :goal module :module arity :arity arglist :arglist :as goal}]
  (let [goal-specs (:pre-specs (utils/get-specs-of-pred [module goal-name arity] @data))
        [term goal-specs-as-tuple] [(if (= arity 1) (first arglist) (apply utils/to-head-tail-list arglist))
                                    (if (= arity 1) (map first goal-specs) (map (partial apply utils/to-tuple-spec) goal-specs))]]
    (if (> arity 0)
      (if (= 1 (count goal-specs))
        (dom/fill-env-for-term-with-spec env term (first goal-specs-as-tuple))
        (dom/fill-env-for-term-with-spec env term (apply utils/to-or-spec goal-specs-as-tuple)))
      env)))

(defn evaluate-body [env body]
  (reduce evaluate-goal env body))

(defn- add-index-to-input-arguments [env arglist]
  (apply uber/add-nodes-with-attrs env (map-indexed #(vector %2 {:index %1}) arglist)))

(defn initial-env [arglist pre-spec]
  (-> (uber/digraph)
      (uber/add-nodes-with-attrs [:ENVIRONMENT {:user-defined-specs (get @data :specs)}])
      (dom/fill-env-for-terms-with-specs arglist pre-spec)
      (add-index-to-input-arguments arglist)))

(defn analyzing [{arglist :arglist body :body :as clause} pre-spec]
  (-> (initial-env arglist pre-spec)
      (evaluate-body body)
      (add-relationships)
      ))

(defn complete-analysis [input-data]
  (reset! data input-data)
  (for [pred-id (utils/get-pred-identities @data)
        clause-id (utils/get-clause-identities-of-pred pred-id @data)
        pre-spec (:pre-specs (utils/get-specs-of-pred pred-id @data))]
    [[clause-id pre-spec] (analyzing (utils/get-clause clause-id @data) pre-spec)]))

(defn example []
  (->> "resources/analysis.pl"
       process-prolog-file
       complete-analysis
       my-pp/pretty-print-analysis-result
       ))

(defn example2 []
  (->> ["resources/module1.pl" "resources/module2.pl"]
       (apply process-prolog-files)
       complete-analysis
       my-pp/pretty-print-analysis-result
       ))
