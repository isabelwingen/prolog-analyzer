(ns prolog-analyzer.analyzer.global-analysis
  (:require
   [prolog-analyzer.analyzer.core :as core]
   [prolog-analyzer.records :as r]
   [prolog-analyzer.utils :as utils]
   [ubergraph.core :as uber]
   [ubergraph.protocols]
   [simple-time.core :as time]
   [loom.graph]
   [loom.attr]
   ))

(defn timestamp []
  (let [now (time/now)]
    (str (time/datetime->hour now) ":" (time/datetime->minute now))))

(defn- create-post-spec [env]
  (let [arglist (uber/attr env :ENVIRONMENT :arglist)
        premise (->> arglist
                     (map #(uber/attr env % :dom))
                     (map #(if (nil? %) (r/->AnySpec) %))
                     (apply r/to-tuple-spec))
        condition (->> arglist
                       (map #(assoc (uber/attrs env %) :pre (r/->AnySpec)))
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
    (let [post-specs-map (apply merge-with #(r/simplify-or (r/->OneOfSpec (hash-set %1 %2)) (get data :specs)) (map create-post-spec envs))]
      (update-in data [:post-specs pred-id] (partial merge-with #(r/simplify-and-without-intersect (r/->AndSpec (hash-set %1 %2))) post-specs-map)))
    data))


(defn add-new-knowledge [data envs]
  (->> envs
       (group-by #(apply vector (drop-last (uber/attr % :ENVIRONMENT :pred-id))))
       (reduce-kv process-predicate-envs data)))


(defn pretty-str-postspec [[k v]]
  (hash-map :condition (apply vector (map r/to-string k)) :premise (r/to-string v)))


(defn- not-the-same [new-data old-data]
  (if (not= (keys (:post-specs new-data)) (keys (:post-specs old-data)))
    true
    (loop [[pred-id & pred-ids] (keys (:post-specs old-data))]
      (if (nil? pred-id)
        false
        (if (= (get-in new-data [:post-specs pred-id]) (get-in old-data [:post-specs pred-id]))
          (recur pred-ids)
          (do
            ;(clojure.pprint/print-table (map pretty-str-postspec (get-in new-data [:post-specs pred-id])))
            true)
          )))))

(defn step [data]
  (core/complete-analysis-parallel data))

(defn global-analysis
  ([data] (global-analysis data 0))
  ([data counter]
   (println (pr-str (str (timestamp) ": Step " counter)))
   (let [envs (step data)
         new-data (add-new-knowledge data envs)]
     (if (and (not-the-same new-data data) (< counter 6))
       (global-analysis new-data (inc counter))
       envs))))
