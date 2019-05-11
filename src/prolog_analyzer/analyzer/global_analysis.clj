(ns prolog-analyzer.analyzer.global-analysis
  (:require
   [prolog-analyzer.analyzer.core :as core]
   [prolog-analyzer.records :as r]
   [prolog-analyzer.utils :as utils]
   [ubergraph.core :as uber]
   [ubergraph.protocols]
   [loom.graph]
   [loom.attr]
   ))

(defn- create-post-spec [env]
  (let [arglist (uber/attr env :ENVIRONMENT :arglist)
        premise (->> arglist
                     (map #(uber/attrs env %))
                     (map :dom)
                     (map #(if (nil? %) (r/->AnySpec) %))
                     (apply r/to-tuple-spec))
        condition (->> arglist
                       (map #(assoc (uber/attrs env %) :pre (r/maybe-spec (r/initial-spec %))))
                       (map :pre)
                       (apply vector))]
    {condition premise}))

(defn- valid-env? [env]
  (->> env
       utils/get-terms
       (map #(utils/get-dom-of-term env % (r/->AnySpec)))
       (every? (complement r/error-spec?))))


(defn process-predicate-envs [data pred-id envs]
  (if (every? valid-env? envs)
    (let [post-specs-map (apply merge-with #(r/simplify-or (r/->OneOfSpec (hash-set %1 %2)) (get data :spec)) (map create-post-spec envs))]
      (update-in data [:post-specs pred-id] (partial merge-with #(r/simplify-and-without-intersect (r/->AndSpec (hash-set %1 %2))) post-specs-map)))
    data))


(defn- add-new-knowledge [data envs]
  (->> envs
       (group-by #(apply vector (drop-last (uber/attr % :ENVIRONMENT :pred-id))))
       (reduce-kv process-predicate-envs data)))


(defn global-analysis
  ([data] (global-analysis data 0))
  ([data counter]
   (println (str "Step " counter))
   (let [envs (core/complete-analysis-parallel data)
         new-data (add-new-knowledge data envs)]
     (if (or (= data new-data) (> counter 4))
       envs
       (global-analysis new-data (inc counter))))))
