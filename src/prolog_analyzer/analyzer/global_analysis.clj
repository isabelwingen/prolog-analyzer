(ns prolog-analyzer.analyzer.global-analysis
  (:require [prolog-analyzer.state :as state]
            [prolog-analyzer.utils :as utils]
            [prolog-analyzer.record-utils :as ru]
            [clojure.tools.logging :as log]
            [prolog-analyzer.parser :as parser]
            [prolog-analyzer.analyzer.core :as clause-analysis]))

(defn- log-if-empty [data]
  (when (empty? (utils/get-pred-identities data))
    (println (pr-str "No predicates found"))))

(defn- group-envs-by-pred-id [envs]
  (group-by #(vec (drop-last (utils/get-title %))) envs))

(defn- create-single-conclusion [env]
  (->> env
       utils/get-arguments
       (map-indexed (fn [i elem] {:id i :type (ru/simplify (utils/get-dom-of-term env elem))}))
       vec))

(defn- create-post-spec [envs]
  (->> envs
       (map create-single-conclusion)
       vec
       (hash-map :guard [] :conclusion)))

(defn contains-postspec? [data pred-id post-spec]
  (contains? (set (utils/get-post-specs pred-id data)) post-spec))

(defn add-if-new [data [_ _ arity :as pred-id] post-spec]
  (if (or
       (contains-postspec? data pred-id post-spec)
       (zero? arity))
    data
    (update-in data [:post-specs pred-id] #(conj % post-spec))))

(defn- create-new-data [in-data envs]
  (->> envs
       group-envs-by-pred-id
       (reduce-kv #(assoc %1 %2 (create-post-spec %3)) {})
       (reduce-kv add-if-new in-data)))

(defn fixpoint [writer counter in-data]
  (log/info "Fixpoint: Step " counter)
  (let [envs (clause-analysis/complete-analysis in-data)
        new-data (create-new-data in-data envs)]
    (if (= in-data new-data)
      (do
        (log/info "Done")
        new-data)
      (recur writer (inc counter) new-data))))

(defn global-analysis [writer data]
  (reset! state/user-typedefs (:specs data))
  (let [cleared-data (dissoc data :specs)]
    (log-if-empty data)
    (fixpoint writer 0 data)))
