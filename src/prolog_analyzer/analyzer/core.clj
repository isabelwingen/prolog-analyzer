(ns prolog-analyzer.analyzer.core
  (:require
   [prolog-analyzer.parser :refer [process-prolog-file process-prolog-snippets]] ;; used only during development
   [prolog-analyzer.analyzer.pretty-printer :refer [to-pretty-map]]
   [prolog-analyzer.analyzer.domain :refer [merge-dom]]
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

;; utils
(defn get-specs-of-pred [pred-identity]
  (let [spec-identity (rest pred-identity)
        specs @data]
    (-> specs
        (select-keys [:pre-specs :post-specs :inv-specs])
        (update :pre-specs #(get-in % spec-identity))
        (update :post-specs #(get-in % spec-identity))
        (update :inv-specs #(get-in % spec-identity))
        )))

(defn get-impls-of-pred [pred-identity]
  (vals (get-in @data (apply vector :preds pred-identity))))

(defn get-pred-identities []
  (for [module (keys (:preds @data))
        pred-name (keys (get-in @data [:preds module]))
        arity (keys (get-in @data [:preds module pred-name]))]
    [module pred-name arity]))

(defn complete-analysis [input-data]
  (reset! data input-data)
  (for [pred-id (get-pred-identities)
        impl (get-impls-of-pred pred-id)
        pre-spec (:pre-specs (get-specs-of-pred pred-id))]
    (analyzing impl pre-spec)))

(comment (-> "resources/module1.pl"
             process-prolog-file
             complete-analysis
             (#(map to-pretty-map %))
             pp/pprint
             ))
