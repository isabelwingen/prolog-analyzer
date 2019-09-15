(ns prolog-analyzer.analyzer.global-analysis
  (:require [prolog-analyzer.state :as state]
            [prolog-analyzer.utils :as utils :refer [case+]]
            [prolog-analyzer.record-utils :as ru]
            [prolog-analyzer.records :as r]
            [clojure.tools.logging :as log]
            [prolog-analyzer.result-visualizer :refer [print-intermediate-result]]
            [flatland.ordered.set :refer [ordered-set]]
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
  (contains? (apply hash-set (utils/get-post-specs pred-id data)) post-spec))


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

(defn- create-new-data [in-data envs]
  (->> envs
       group-envs-by-pred-id
       (reduce-kv #(assoc %1 %2 (create-post-spec %3)) {})
       (reduce-kv add-if-new in-data)))

(defn post-spec-to-string [{guard :guard concl :conclusion}]
  (let [a (->> guard
               (map #(str "$" (:id %) ":" (r/to-string (:type %))))
               (clojure.string/join ", "))
        b (->> concl
               (map (partial map #(str "$" (:id %) ":" (r/to-string (:type %)))))
               (map (partial clojure.string/join ", "))
               (clojure.string/join "\n\t"))]
    (str "\nguard:\n\t" a "\n\n" "concl:\n\t" b "\n")))


(defn same [data-a data-b]
  (and
   (= (:post-specs data-a) (:post-specs data-b))
   (= (:pre-specs data-a) (:pre-specs data-b))))

(defn fixpoint [writer counter in-data]
  (log/info "Fixpoint: Step " counter)
  (writer in-data)
  (print-intermediate-result counter in-data)
  (let [envs (clause-analysis/complete-analysis in-data)
        new-data (create-new-data in-data envs)]
    (if (same in-data new-data)
      (do
        (log/info "Done")
        new-data)
      (recur writer (inc counter) new-data))))

(defn- dummy-post-spec [arity]
  (hash-map :guard [] :conclusion [(vec (map #(hash-map :id % :type (r/->AnySpec)) (range 0 arity)))]))

(defn- add-dummy-post-specs [data]
  (reduce (fn [d [_ _ arity :as pred-id]] (update-in d [:post-specs pred-id] #(vec (conj % (dummy-post-spec arity))))) data (utils/get-pred-identities data)))

(defn global-analysis [writer data]
  (let [cleared-data (add-dummy-post-specs data)]
    (reset! state/self-calling {})
    (log-if-empty cleared-data)
    (fixpoint writer 0 cleared-data)))
