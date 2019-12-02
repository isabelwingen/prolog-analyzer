(ns prolog-analyzer.analyzer.global-analysis
  (:require [clojure.tools.logging :as log]
            [flatland.ordered.set :refer [ordered-set]]
            [prolog-analyzer.analyzer.core :as clause-analysis]
            [prolog-analyzer.record-utils :as ru]
            [prolog-analyzer.records :as r]
            [prolog-analyzer.state :as state]
            [prolog-analyzer.utils :as utils]
            [prolog-analyzer.result-visualizer :refer [print-type-information]]))

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
       (r/->Postspec [])))

(defn length-of-post-spec [{guard :guard concl :conclusion}]
  (let [a (->> guard
               (map :type)
               (map r/length)
               (apply +))
        b (->> concl
               (map (partial map :type))
               (map (partial map r/length))
               (map (partial apply +))
               (apply +))]
    (+ a b)))



(defn add-if-new [data [_ _ arity :as pred-id] post-spec]
  (if (> (length-of-post-spec post-spec) 1000)
    data
    (-> data
        (update-in [:post-specs pred-id] #(if (nil? %)
                                            (ordered-set)
                                            (apply ordered-set %)))
        (update-in [:post-specs pred-id] #(conj % post-spec)))))

(defn- create-new-post-specs [in-data envs]
  (->> envs
       group-envs-by-pred-id
       (reduce-kv #(assoc %1 %2 (create-post-spec %3)) {})
       (reduce-kv add-if-new in-data)))

(defn- add-errors [in-data envs]
  (->> envs
       (map utils/errors)
       (apply merge-with merge in-data)))

(defn- create-new-data [in-data envs]
  (-> in-data
      (create-new-post-specs envs)
      (add-errors envs)))

(defn same [data-a data-b]
  (and
   (= (:post-specs data-a) (:post-specs data-b))
   (= (:pre-specs data-a) (:pre-specs data-b))
   (= (:errors data-a) (:errors data-b))))

(defn fixpoint [write counter in-data]
  (log/info "Fixpoint: Step " counter)
  (write in-data counter)
  (let [envs (clause-analysis/complete-analysis in-data)
        new-data (create-new-data in-data envs)]
    (if (same in-data new-data)
      (do
        (log/info "Done")
        (print-type-information counter envs)
        new-data)
      (recur write (inc counter) new-data))))

(defn- dummy-post-spec [arity]
  (r/->Postspec [] [(vec (map #(hash-map :id % :type (r/->AnySpec)) (range 0 arity)))]))

(defn- add-dummy-post-specs [data]
  (reduce (fn [d [_ _ arity :as pred-id]] (update-in d [:post-specs pred-id] #(vec (conj % (dummy-post-spec arity))))) data (utils/get-pred-identities data)))

(defn global-analysis [write data]
  (let [cleared-data (add-dummy-post-specs data)]
    (log-if-empty cleared-data)
    (fixpoint write 0 cleared-data)))
