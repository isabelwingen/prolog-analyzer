(ns prolog-analyzer.analyzer.core
  (:require
   [prolog-analyzer.parser :refer [process-prolog-file process-prolog-snippets process-prolog-files]] ;; used only during development
   [prolog-analyzer.analyzer.domain :refer [merge-dom]]
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


(defn merge-nodes-with-attrs [env & nodes-with-attributes]
  (loop [result env
         nodes nodes-with-attributes]
    (if-let [[node {new-dom :dom :as attr}] (first nodes)]
      (if (uber/has-node? result node)
        (recur (uber/add-nodes-with-attrs result [node (-> (uber/attrs result node)
                                                           (update :dom (comp (partial apply vector) distinct (partial concat new-dom))))])
               (rest nodes))
        (recur (uber/add-nodes-with-attrs result [node attr]) (rest nodes)))
      result)))

(defmulti add-to-env-aux #(:type (second %)))
(defmethod add-to-env-aux :list [[env {head :head tail :tail :as arg} {t :type :as spec}]]
  (-> env
      (merge-nodes-with-attrs
       [(id arg) {:original arg :dom [spec]}]
       [(id head) {:original head :dom [t]}]
       [(id tail) {:original tail :dom [spec]}])
      (uber/add-directed-edges
       [(id arg) (id head) {:kind :has-head}]
       [(id arg) (id tail) {:kind :has-tail}]
       [(id head) (id arg) {:kind :is-head-of}]
       [(id tail) (id arg) {:kind :is-tail-of}])))

(defmethod add-to-env-aux :default [[env arg spec]]
  (-> env
      (uber/add-nodes-with-attrs [(id arg) {:original arg :dom [spec]}])))

(defn add-to-env [env [arg spec]]
  (add-to-env-aux [env arg spec]))

(defn analyzing [{arglist :arglist body :body} pre-spec]
  (let [env (uber/digraph)]
    (reduce add-to-env env (partition 2 (interleave arglist pre-spec)))))

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
