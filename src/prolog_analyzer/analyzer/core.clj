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

(defn analyzing [{arglist :arglist body :body} pre-spec]
  (let [env (uber/digraph)
        dom (apply merge-with concat (map dom/get-initial-dom-from-spec arglist pre-spec))
        nodes (map #(vector (id %) {:original % :dom (get dom %)}) (keys dom))]
    (apply uber/add-nodes-with-attrs env nodes)))


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
