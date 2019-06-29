(ns prolog-analyzer.analyzer.global-analysis
  (:require
   [prolog-analyzer.analyzer.core :as core]
   [prolog-analyzer.analyzer.pretty-printer :as my-pprint]
   [prolog-analyzer.records :as r]
   [prolog-analyzer.utils :as utils :refer [case+]]
   [ubergraph.core :as uber]
   [ubergraph.protocols]
   [simple-time.core :as time]
   [loom.graph]
   [loom.attr]

   [clojure.java.io :as io]))

(defn timestamp []
  (let [now (time/now)]
    (str (time/format now))))

(defn- create-premise [env]
  (let [arglist (uber/attr env :ENVIRONMENT :arglist)]
    (assert arglist "arglist not nil")
    (assert (not (empty? arglist)) "arglist is empty")
    (->> arglist
         (map #(utils/get-dom-of-term env % (r/->AnySpec)))
         (apply r/to-tuple-spec))))

(defn valid-spec? [spec]
  (cond
    (nil? spec) false
    (= r/LIST (r/spec-type spec)) (valid-spec? (:type spec))
    (= r/COMPOUND (r/spec-type spec)) (if (:functor spec)
                                        (if (:arglist spec) (every? valid-spec? (:arglist spec)) false)
                                        (nil? (:arglist spec)))
    (= r/TUPLE (r/spec-type spec)) (if (:arglist spec) (every? valid-spec? (:arglist spec)) false)
    (contains? #{r/OR, r/AND} (r/spec-type spec)) (if (and (:arglist spec) (not-empty (:arglist spec))) (every? valid-spec? (:arglist spec)) false)
    (= r/USERDEFINED (r/spec-type spec)) (if (:arglist spec) (if (not-empty (:arglist spec)) (every? valid-spec? (:arglist spec)) false) true)
    :else true
    ))


(defn- valid-env? [env]
  (let [doms (->> env
                  utils/get-terms
                  (map #(utils/get-dom-of-term env % (r/->AnySpec))))]
    (and
     (every? (complement r/error-spec?) doms)
     (every? valid-spec? doms))))

(defn- depth [type]
  (case+ (r/spec-type type)
         (r/OR,r/AND,r/TUPLE) (inc (apply max 0 (map depth (:arglist type))))
         (r/USERDEFINED,r/COMPOUND) (if (:arglist type) (inc (apply max 0 (map depth (:arglist type)))) 0)
         r/LIST (inc (depth (:type type)))
         0))

(defn- reached-limit? [data env]
  (get-in data [:premise (utils/get-pred-id env) (utils/get-clause-number env) :reached-limit] false))

(defn- get-hash [data env]
  (get-in data [:premise (utils/get-pred-id env) (utils/get-clause-number env) :hash]))

(defn- get-premise [data env]
  (get-in data [:premise (utils/get-pred-id env) (utils/get-clause-number env) :premise]))

(defn- set-premise [data env premise]
  (let [reached-limit (> (depth premise) 4)
        pred-id (utils/get-pred-id env)
        clause-number (utils/get-clause-number env)]
    (-> data
        (assoc-in [:premise pred-id clause-number :premise] premise)
        (assoc-in [:premise pred-id clause-number :hash] (hash premise))
        (assoc-in [:premise pred-id clause-number :reached-limit] reached-limit)
        )))


(defn- store-new-premise [data env]
  (if (reached-limit? data env)
    data
    (let [new-premise (create-premise env)
          new-hash (hash new-premise)]
      (if (= new-hash (get-hash data env))
        data
        (set-premise data env new-premise)))))

(defn- collect-premises [data pred-id]
  (let [premises (->> (get-in data [:premise pred-id])
                      vals
                      (map :premise)
                      set)
        defs (:specs data)
        res (r/->OneOfSpec premises)]
    (r/simplify-or res defs)))

(defn- get-post-spec-hash [data pred-id]
  (get-in data [:hashs pred-id]))

(defn- set-post-spec-hash [data pred-id premise]
  (assoc-in data [:hashs pred-id] (hash premise)))


(defmulti process-predicate-envs (fn [data pred-id envs]
                                   (and (not (contains? #{"user","avl","lists"} (first pred-id)))
                                        (not (zero? (last pred-id)))
                                        (every? valid-env? envs))))

(defmethod process-predicate-envs true [data pred-id envs]
  (let [new-data (reduce store-new-premise data envs)
        new-premise (collect-premises new-data pred-id)
        condition (apply vector (repeat (last pred-id) (r/->AnySpec)))
        new-post-spec (hash-map condition new-premise)
        new-hash (hash new-premise)]
    (if (= new-hash (get-post-spec-hash new-data pred-id))
      new-data
      (-> new-data
          (update-in [:post-specs pred-id] (partial merge-with #(r/simplify-and-without-intersect (r/->AndSpec (hash-set %1 %2))) new-post-spec))
          (set-post-spec-hash pred-id new-premise)))))

(defmethod process-predicate-envs false [data pred-id envs]
  data)


(defn add-new-knowledge [data envs]
  (->> envs
       (group-by #(apply vector (utils/get-pred-id %)))
       (reduce-kv process-predicate-envs data)))

(defn- not-the-same [new-data old-data]
  (if (not= (keys (:post-specs new-data)) (keys (:post-specs old-data)))
    true
    (loop [[pred-id & pred-ids] (keys (:post-specs old-data))]
      (if (nil? pred-id)
        false
        (if (= (get-in new-data [:post-specs pred-id]) (get-in old-data [:post-specs pred-id]))
          (recur pred-ids)
          true
          )))))

(defn step [data]
  (core/complete-analysis-parallel data))


(defn create-pre-and-postspecs [{post-spec-map :post-specs pre-spec-map :pre-specs defs :specs}]
  (let [nono-keys (fn [[m _ _]] (contains? #{"user" "lists" "avl"} m))
        post-specs (my-pprint/create-post-specs (select-keys post-spec-map (remove nono-keys (keys post-spec-map))) defs)
        pre-specs (my-pprint/create-pre-specs (select-keys pre-spec-map (remove nono-keys (keys pre-spec-map))))]
    (when (.exists (io/file "data.tmp"))
      (io/delete-file (io/file "data.tmp")))
    (spit "data.tmp" post-specs :append true)
    (spit "data.tmp" "\n\n" :append true)
    (spit "data.tmp" pre-specs :append true)
    ))


(defn global-analysis
  ([data] (global-analysis data 0))
  ([data counter]
   (println (pr-str (str (timestamp) ": Step " counter)))
   (let [envs (step data)
         new-data (add-new-knowledge data envs)]
     (when (.exists (io/file "plstatic.tmp"))
       (io/delete-file (io/file "plstatic.tmp")))
     (spit "bla.tmp" (keys data))
     (doseq [env envs]
       (spit "plstatic.tmp" (with-out-str (my-pprint/print-types-and-errors-v3 env)) :append true))
     (if (and (not-the-same new-data data) (< counter 6))
       (global-analysis new-data (inc counter))
       (do
         (create-pre-and-postspecs data)
         envs)))))
