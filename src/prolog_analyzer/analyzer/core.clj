(ns prolog-analyzer.analyzer.core
  (:require
   [prolog-analyzer.parser :refer [process-prolog-file process-prolog-snippets]] ;; used only during development
   [prolog-analyzer.analyzer.pretty-printer :refer [to-pretty-map]]
   [prolog-analyzer.analyzer.domain :refer [merge-dom]]
   [prolog-analyzer.utils :as utils]
   [clojure.set]
   [clojure.pprint :as pp]
   ))

(defn id [arg]
  (hash arg))

(def data (atom {}))

(defn merge-into-env [env arg new-value]
  (if (contains? env (id arg))
    (-> env
        (update-in [(id arg) :dom] (partial merge-dom (:dom new-value)))
        (update-in [(id arg) :relations] (partial clojure.set/union (:relations new-value))))
    (-> env
        (assoc-in [:id-mapping (id arg)] arg)
        (assoc (id arg) new-value))))

(defmulti add-to-env-aux #(:type (second %)))
(defmethod add-to-env-aux :head-tail-list [[env {head :head tail :tail :as arg} {t :type :as spec}]]
  (-> env
      (merge-into-env arg {:dom spec :relations #{{:head (id head)} {:tail (id tail)}}})
      (merge-into-env head {:dom t :relations #{{:head-of (id arg)}}})
      (merge-into-env tail {:dom spec :relations #{{:tail-of (id arg)}}})))

(defmethod add-to-env-aux :default [[env arg spec]]
  (merge-into-env env arg {:dom spec :relations #{}}))

(defn add-to-env [env [arg spec]]
  (let [new-env (assoc-in env [:id-mapping (id arg)] arg)]
    (add-to-env-aux [new-env arg spec])))

(defn analyzing [{arglist :arglist body :body} pre-spec]
  (let [env {:id-mapping {} :args (zipmap (range 0 (count arglist)) (map id arglist))}]
    (reduce add-to-env env (partition 2 (interleave arglist pre-spec)))))

(defn complete-analysis [input-data]
  (reset! data input-data)
  (for [pred-id (utils/get-pred-identities @data)
        impl (utils/get-impls-of-pred pred-id @data)
        pre-spec (:pre-specs (utils/get-specs-of-pred pred-id @data))]
    (analyzing impl pre-spec)))

(comment (-> "resources/module1.pl"
             process-prolog-file
             complete-analysis
             (#(map to-pretty-map %))
             pp/pprint
             ))
