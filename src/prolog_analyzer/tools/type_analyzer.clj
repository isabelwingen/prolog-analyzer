(ns prolog-analyzer.tools.type-analyzer
  (:require
   [clojure.string]
   [clojure.java.io :as io]
   [clojure.set]))


(defn get-data [file]
  (read-string (str \[ (slurp file) \])))

(defn mean [l]
  (let [c (count l)
        sum (apply +' l)]
    (float (/ sum c))))


(defn- psize [f]
  (if (.isDirectory f)
    (apply + (pmap psize (.listFiles f)))
    (if (.endsWith (.getName f) ".pl")
      (.length f)
      0)))



;; Analysis for prints of print-types-and-errors-v2
(defn artifical? [spec]
  (or
   (.startsWith (:name spec) "A~~")
   (.startsWith (:name spec) "T~~")
   (.startsWith (:name spec) "ID~~")
   ))


(defn sort-v2 [remove-singletons? data]
  (let [vars1 (->> data
                   (mapcat keys)
                   (filter #(= :var (:record-type %)))
                   (remove artifical?))
        vars (if remove-singletons? (remove :singleton? vars1) vars1)
        freq (->> data
                  (map #(select-keys % vars))
                  (mapcat vals)
                  (map (fn [{dom :dom compatible-edge? :compatible-edge?}] (if (= :any (:record-type dom)) (if compatible-edge? :any-with-compatible :any) :not-any)))
                  frequencies)
        total (apply +' (vals freq))]
    (-> freq
        (update :not-any #(or % 0))
        (update :any #(or % 0))
        (update :any-with-compatible #(or % 0))
        (assoc :total total)
        )))

(defn analysis-v2-single-pack [remove-singletons? {filename :filename data :data :as res}]
  (-> res
      (update :filename (partial filter #(.startsWith % "/home")))
      (update :filename last)
      (update :filename #(.getName (io/file %)))
      (update :data (partial take-nth 2))
      (update :data (partial sort-v2 remove-singletons?))
      (clojure.set/rename-keys {:data :vars})
      ))

(defn analysis-v2 [remove-singletons? file]
  (->> file
       get-data
       (remove #(and (string? %) (not (.startsWith % "/home"))))
       (partition-by string?)
       (partition 2)
       (map #(hash-map :filename (first %) :data (second %)))
       (map (partial analysis-v2-single-pack remove-singletons?))
       ))

(defn prob-type-analysis-v2 [path]
  (->> path
       get-data
       (remove string?)
       (hash-map :filename ["/home"] :data)
       (analysis-v2-single-pack false)))

(spit "useful-results/prob-types" "")

(defn analysis-v3-single-pack [remove-singletons? {data :data :as m}]
  (-> m
      (update :filename (partial filter #(.startsWith % "/home")))
      (update :filename last)
      (update :filename #(.getName (io/file %)))
      (update :data (partial drop 1))
      (update :data (partial take-nth 3))
      (update :data (partial sort-v2 remove-singletons?))
      (assoc :calls data)
      (update :calls (partial take-nth 3))
      (update :calls (partial apply merge-with +'))
      (clojure.set/rename-keys {:data :vars})
      ))

(defn analysis-v3
  "With call counters"
  [remove-singletons? file]
  (->> file
       get-data
       (remove #(and (string? %) (not (.startsWith % "/home"))))
       (partition-by string?)
       (partition 2)
       (map #(hash-map :filename (first %) :data (second %)))
       (map (partial analysis-v3-single-pack remove-singletons?))
       ))

(defn prob-type-analysis-v3 [path]
  (->> path
       get-data
       (remove string?)
       (hash-map :filename ["/home"] :data)
       (analysis-v3-single-pack false)))


(defn percents-of-single-pack [m]
  (let [total (get-in m [:vars :total])
        vars (-> m
                 (update-in [:vars :any] #(float (/ % total)))
                 (update-in [:vars :not-any] #(float (/ % total)))
                 (update-in [:vars :any-with-compatible] #(float (/ % total))))
        total-calls (+ (get-in m [:calls :known] 0) (get-in m [:calls :unknown] 1))]
    (if (or (contains? (:calls vars) :unknown) (or (contains? (:calls vars) :known)))
      (-> vars
          (update-in [:calls :known] #(or % 0))
          (update-in [:calls :unknown] #(or % 0))
          (update-in [:calls :known] #(float (/ % total-calls)))
          (update-in [:calls :unknown] #(float (/ % total-calls))))
      vars)
    ))

(defn percents [maps]
  (let [without-filenames (map #(dissoc % :filename) maps)]
    (->> without-filenames
         (apply merge-with (partial merge-with +'))
         percents-of-single-pack)))



(defn swi-packs-complete []
  (let [file-list ["useful-results/swi-pack-types-20000-0"
                   "useful-results/swi-pack-types-20000-1"
                   "useful-results/swi-pack-types-20000-3"
                   "useful-results/swi-pack-types-20000-6"
                   "useful-results/swi-pack-types-20000-12"
                   ]
        percents-v2 (->> file-list
                         (map (partial analysis-v2 false))
                         (map #(map (comp :vars percents-of-single-pack :data) %))
                         (map (partial map vals))
                         (map (partial map (partial map #(if (float? %) (format "%.2f" %) (str %)))))
                         (map (partial map (partial clojure.string/join ";")))
                         (apply map vector)
                         (map (partial clojure.string/join "; ;"))
                         (clojure.string/join "\n")
                         )
        percents-v3 (->> file-list
                         (map (partial analysis-v2 true))
                         (map #(map (comp :vars percents-of-single-pack :data) %))
                         (map (partial map vals))
                         (map (partial map (partial map #(if (float? %) (format "%.2f" %) (str %)))))
                         (map (partial map (partial clojure.string/join ";")))
                         (apply map vector)
                         (map (partial clojure.string/join "; ;"))
                         (clojure.string/join "\n")
                         )
        ]
    (spit "useful-results/table-v2.csv" percents-v2)
    (spit "useful-results/table-v3.csv" percents-v3)
    (hash-map :v2 (map (partial percents analysis-v2 false) file-list)
              :v3 (map (partial percents analysis-v2 true) file-list))
    ))
