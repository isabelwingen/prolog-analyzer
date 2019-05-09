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
  (let [indexed-terms (->> env
                           utils/get-terms
                           (filter #(uber/attr env % :index)))
        premise (->> indexed-terms
                     (map #(uber/attrs env %))
                     (sort-by :index)
                     (map :dom)
                     (map #(if (nil? %) (r/->AnySpec) %))
                     (apply r/to-tuple-spec))
        condition (->> indexed-terms
                       (map #(assoc (uber/attrs env %) :pre (r/maybe-spec (r/initial-spec %))))
                       (sort-by :index)
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


(defn global-analysis [data]
  (let [envs (core/complete-analysis-parallel data)
        new-data (add-new-knowledge data envs)]
    (if (= data new-data)
      envs
      (global-analysis new-data))))
