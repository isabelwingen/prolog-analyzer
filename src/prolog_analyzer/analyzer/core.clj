(ns prolog-analyzer.analyzer.core
  (:require
   [prolog-analyzer.parser :refer [process-prolog-file process-prolog-snippets process-prolog-files]] ;; used only during development
   [prolog-analyzer.analyzer.domain :as dom]
   [prolog-analyzer.analyzer.validator :as validator]
   [prolog-analyzer.utils :as utils]
   [prolog-analyzer.analyzer.pretty-printer :as my-pp]
   [ubergraph.core :as uber]
   [loom.graph]
   [loom.attr]
   [clojure.set]
   [clojure.pprint :as pp]
   ))

(def data (atom {}))

(defn id [arg]
  (hash arg))

(defn initialize-env [env arglist spec]
  (let [dom (apply merge-with (comp (partial apply vector) concat) (map dom/get-initial-dom-from-spec arglist spec))
        nodes (map #(vector % {:dom (get dom %)}) (keys dom))
        arglist-nodes (map-indexed #(vector  %2 {:index %1}) arglist)]
    (apply uber/add-nodes-with-attrs env (concat nodes arglist-nodes))))

(defmulti add-relationships-for-type (comp :type second))
(defn add-relationships-between-arguments [env]
  (reduce #(add-relationships-for-type [%1 %2]) env (uber/nodes env)))
(defmethod add-relationships-for-type :list [[env {head :head tail :tail :as l}]]
  (uber/add-edges env [head l {:relation :is-head-of}] [tail l {:relation :is-tail-of}]))
(defmethod add-relationships-for-type :default [[env _]] env)


(defn analyzing [{arglist :arglist body :body} pre-spec]
  (-> (uber/digraph)
      (initialize-env arglist pre-spec)
      (add-relationships-between-arguments)))


(defn complete-analysis [input-data]
  (reset! data input-data)
  (for [pred-id (utils/get-pred-identities @data)
        clause-id (utils/get-clause-identities-of-pred pred-id @data)
        pre-spec (:pre-specs (utils/get-specs-of-pred pred-id @data))]
    [[clause-id pre-spec] (analyzing (utils/get-clause clause-id @data) pre-spec)]))

(defn example []
  (->> "resources/simple-example.pl"
       process-prolog-file
       complete-analysis
       my-pp/pretty-print-analysis-result))

(defn example2 []
  (->> ["resources/module1.pl" "resources/module2.pl"]
       (apply process-prolog-files)
       complete-analysis
       my-pp/pretty-print-analysis-result))

(example)

(map identity {:a :b :c :d})
