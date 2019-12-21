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

;; (defn- add-errors [in-data envs]
;;   (->> envs
;;        (map utils/errors)
;;        (apply merge-with merge in-data)))


(defn add-postspecs [in-data results]
  (->> results
       (map #(hash-map (:pred-id %) (:conclusion %)))
       (apply merge-with clojure.set/union)
       (reduce-kv #(assoc %1 %2 (r/->Postspec [] (vec %3))) {})
       (reduce-kv add-if-new in-data)))

(defn add-errors [in-data results]
  (assoc in-data :errors (->> results
                              (remove (comp empty? :errors))
                              (map #(hash-map (:clause-id %) (:errors %)))
                              (apply merge))))

(defn create-new-data [in-data results]
  (-> in-data
      (add-postspecs results)
      (add-errors results)))

(defn same [data-a data-b]
  (and
   (= (:post-specs data-a) (:post-specs data-b))
   (= (:pre-specs data-a) (:pre-specs data-b))
   (= (:errors data-a) (:errors data-b))))

(defn fixpoint [write counter in-data]
  (log/info "Fixpoint: Step " counter)
  (write in-data counter)
  (let [results (clause-analysis/complete-analysis in-data)
        new-data (create-new-data in-data results)]
    (if (same in-data new-data)
      (do
        (log/info "Done")
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
