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

(defn id [arg]
  (hash arg))

(defmulti add-relationships-for-type (comp :type second))

(defn add-relationships-between-arguments [env]
  (reduce #(add-relationships-for-type [%1 %2]) env (uber/nodes env)))

(defmethod add-relationships-for-type :list [[env {head :head tail :tail :as l}]]
  (uber/add-edges env [head l {:relation :is-head-of}] [tail l {:relation :is-tail-of}]))
(defmethod add-relationships-for-type :default [[env _]] env)

(defn add-to-dom [env term dom]
  (if (uber/has-node? env term)
    (let [attrs (uber/attrs env term)]
      (uber/set-attrs env term (update attrs :dom #(conj % dom))))
    (uber/add-nodes-with-attrs env [term {:dom [dom]}])))


(defn initialize-env [arglist spec]
  (let [dom (apply merge-with (comp (partial apply vector) concat) (map dom/get-initial-dom-from-spec arglist spec))
        arglist-nodes (map-indexed #(vector  %2 {:index %1}) arglist)]
    (reduce #(apply add-to-dom %1 %2) (apply uber/add-nodes-with-attrs (uber/digraph) arglist-nodes) dom)))

(defn- to-or-spec [& specs]
  (if (= 1 (count specs))
    (first specs)
    {:spec :one-of :arglist specs}))

(defn to-tuple-spec [& specs]
  {:spec :tuple :arglist specs})

(defn evaluate-goal [env {goal-name :goal module :module arity :arity arglist :arglist :as goal}]
  (let [goal-id [module goal-name arity]
        pre-specs (:pre-specs (utils/get-specs-of-pred goal-id @data))
        ors (apply map to-or-spec pre-specs)
        pairs (map vector arglist ors)]
    (->
     (reduce #(apply add-to-dom %1 %2) env pairs)
     (add-to-dom (apply utils/to-head-tail-list arglist) (apply to-or-spec (map (partial apply to-tuple-spec) pre-specs))))
    ))

(defn evaluate-body [env body]
  (loop [result env
         goals body]
    (if (empty? goals)
      result
      (recur (evaluate-goal result (first goals)) (rest goals)))))

(defn analyzing [{arglist :arglist body :body} pre-spec]
  (-> (initialize-env arglist pre-spec)
      (evaluate-body body)
      (add-relationships-between-arguments)
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
       my-pp/pretty-print-analysis-result))

(defn example2 []
  (->> ["resources/module1.pl" "resources/module2.pl"]
       (apply process-prolog-files)
       complete-analysis
       my-pp/pretty-print-analysis-result))
