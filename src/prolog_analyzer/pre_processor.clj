(ns prolog-analyzer.pre-processor
  (:require [prolog-analyzer.utils :as utils :refer [case+]]
            [prolog-analyzer.records :as r]
            [prolog-analyzer.record-utils :as ru]
            [clojure.tools.logging :as log]
            ))



(defn- set-correct-goal-module [pred->module-map {goal-name :goal arity :arity goal-module :module :as goal}]
  (if (= "self" goal-module)
    (assoc goal :module (get pred->module-map goal-name "user"))
    goal))


(defn- calculate-pred-to-module-map [pred->module-maps caller-module imports]
  (let [used-modules (-> imports
                         (get caller-module)
                         (assoc "user" :all)
                         (assoc caller-module :all))
        import-all-modules (reduce-kv #(if (= :all %3) (conj %1 %2) %1) [] used-modules)
        weak-mappings (->> (select-keys pred->module-maps import-all-modules)
                           vals
                           (apply merge))
        strong-mappings (->> (select-keys pred->module-maps (keys (apply dissoc used-modules import-all-modules)))
                             (reduce-kv (fn [m k v] (assoc m k (select-keys v (get used-modules k)))) {})
                             vals
                             (apply merge))]
    (merge weak-mappings strong-mappings)))

(defn calculate-pred-to-module-maps [data]
  (let [m (->> (select-keys data [:pre-specs :post-specs :preds])
               (vals)
               (mapcat keys)
               (group-by first)
               (reduce-kv (fn [m k v] (assoc m k (reduce #(assoc %1 (second %2) (first %2)) {} v))) {}))]
    (reduce #(assoc %1 %2 (calculate-pred-to-module-map %1 %2 (:imports data))) m (keys m))))

(defn set-correct-modules [data]
  (let [pred->module-maps (calculate-pred-to-module-maps data)]
    (reduce
     (fn [result [[source-module & _ :as pred-id] clause-number]]
       (update-in result [:preds pred-id clause-number :body] (partial map (partial set-correct-goal-module (get pred->module-maps source-module)))))
     data
     (utils/get-clause-identities data))))

(defn- create-pre-spec [pred-id data]
  (->> data
       (utils/get-clauses-of-pred pred-id)
       (map :arglist)
       (map (partial map (comp ru/maybe-spec r/initial-spec)))
       (map (partial apply vector))
       (apply vector)
       ))


(defn- create-post-spec [[_ _ arity :as pred-id] data]
  (->> data
       (utils/get-clauses-of-pred pred-id)
       (map :arglist)
       (map (partial map r/initial-spec))
       (map (partial apply vector))
       (map (partial vector (apply vector (repeatedly arity r/->AnySpec))))
       (apply vector)
       (group-by first)
       (reduce-kv (fn [m k v] (assoc m k (->> v
                                             (map second)
                                             (map (partial apply ru/to-tuple-spec))
                                             set
                                             r/->OneOfSpec
                                             )))
                  {})))


(defn- add-any-pre-specs
  "If there are no pre-specs, add one"
  [data]
  (loop [pred-ids (utils/get-pred-identities data)
         result data]
    (if-let [[module pred-name arity :as pred-id] (first pred-ids)]
      (if (nil? (:pre-specs (utils/get-specs-of-pred pred-id data)))
        (recur (rest pred-ids) (assoc-in result [:pre-specs [module pred-name arity]] (create-pre-spec pred-id data)))
        (recur (rest pred-ids) result))
      result)))

(defn- add-any-post-specs
  "If there are no post-specs, add one"
  [data]
  (loop [pred-ids (utils/get-pred-identities data)
         result data]
    (if-let [[module pred-name arity :as pred-id] (first pred-ids)]
      (if (nil? (:post-specs (utils/get-specs-of-pred pred-id data)))
        (recur (rest pred-ids) (assoc-in result [:post-specs [module pred-name arity]] (create-post-spec pred-id data)))
        (recur (rest pred-ids) result))
      result)))

(defn- transform-arglist [singletons args]
  (->> args
       (map (partial r/map-to-term singletons))
       (apply vector)))

(declare transform-body)

(defn- transform-body-elements [singletons {goal-name :goal arglist :arglist :as goal}]
  (case goal-name
    (:or, :if) (-> goal
                   (update :arglist (partial map (partial transform-body singletons)))
                   (assoc :module :built-in))
    (update goal :arglist (partial transform-arglist singletons))))

(defn- transform-body [singletons body]
  (map (partial transform-body-elements singletons) body))

(defn transform-args-to-term-records [data]
  (reduce (fn [data [pred-id clause-number]]
            (let [singletons (get-in data [:singletons pred-id clause-number])]
              (-> data
                  (update-in [:preds pred-id clause-number :arglist] (partial transform-arglist singletons))
                  (update-in [:preds pred-id clause-number :body] (partial transform-body singletons))
                  ))
            )
          data
          (utils/get-clause-identities data)))

(defn transform-singleton-lists [data]
  (reduce (fn [d [pred-id clause-number]] (update-in d [:singletons pred-id clause-number] #(apply vector (map r/map-to-term %)))) data (utils/get-clause-identities data)))

(defn- mark-self-calling-clause
  "Marks clauses that are calling themselves"
  [[_ pred-name arity] {body :body :as clause}]
  (if (some #(and (= pred-name (:goal %)) (= arity (:arity %))) body)
    (assoc clause :self-calling? true)
    (assoc clause :self-calling? false)))

(defn mark-self-calling-clauses [data]
  (loop [clause-ids (utils/get-clause-identities data)
         result data]
    (if-let [[pred-id clause-number] (first clause-ids)]
      (recur (rest clause-ids) (update-in result [:preds pred-id clause-number] (partial mark-self-calling-clause pred-id)))
      result)))


(defn pre-process-single [data]
  (println (pr-str "Start Pre Process Single"))
  (-> data
      mark-self-calling-clauses
      transform-singleton-lists
      transform-args-to-term-records
      add-any-pre-specs
      add-any-post-specs
      set-correct-modules
      ))
