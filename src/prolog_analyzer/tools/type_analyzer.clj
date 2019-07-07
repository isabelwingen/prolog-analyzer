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


(defn safe-div [a b]
  (if (zero? b)
    -1
    (/ a b)))

(defn percents-of-single-pack [m]
  (let [total (get-in m [:vars :total] 1)
        vars (-> m
                 (update-in [:vars :any] #(float (safe-div % total)))
                 (update-in [:vars :not-any] #(float (safe-div % total)))
                 (update-in [:vars :any-with-compatible] #(float (safe-div % total))))
        total-calls (+ (get-in m [:calls :known] 0) (get-in m [:calls :unknown] 0))]
    (if (or (contains? (:calls vars) :unknown) (or (contains? (:calls vars) :known)))
      (-> vars
          (assoc-in [:calls :total] total-calls)
          (update-in [:calls :known] #(or % 0))
          (update-in [:calls :unknown] #(or % 0))
          (update-in [:calls :known] #(float (safe-div % total-calls)))
          (update-in [:calls :unknown] #(float (safe-div % total-calls))))
      vars)
    ))

(defn percents [maps]
  (let [without-filenames (map #(dissoc % :filename) maps)]
    (->> without-filenames
         (apply merge-with (partial merge-with +'))
         percents-of-single-pack)))


(defn swi-packs-complete []
  (let [file-list ["useful-results/swi-pack-types-20000-0"
                   ;; "useful-results/swi-pack-types-20000-1"
                   ;; "useful-results/swi-pack-types-20000-3"
                   ;; "useful-results/swi-pack-types-20000-6"
                   "useful-results/swi-pack-types-20000-12"
                   ]
        headline1 (->> file-list
                      (map #(drop 36 %))
                      (map #(apply str %))
                      (map #(conj '("" "" "") %))
                      (map (partial clojure.string/join ";"))
                      (#(conj % ""))
                      )
        headline2 (->> "any;not any;restricted any;total"
                       (repeat (count file-list))
                       (#(conj % "")))
        names (->> file-list
                   (map (partial analysis-v2 false))
                   (map (partial map :filename))
                   first)

        percents-v2 (->> file-list
                         (map (partial analysis-v2 false)))
        modifier (fn [x] (->> x
                             (map #(map (comp :vars percents-of-single-pack) %))
                             (map (partial map vals))
                             (map (partial map (partial map #(if (float? %) (format "%.4f" %) (str %)))))
                             (map (partial map (partial clojure.string/join ";")))
                             (cons names)
                             (apply map vector)
                             (cons headline2)
                             (cons headline1)
                             (map (partial clojure.string/join "; ;"))
                             (clojure.string/join "\n")))
        percents-v2 (modifier (map (partial analysis-v2 false) file-list))
        percents-v3 (modifier (map (partial analysis-v2 true) file-list))
        ]
    (spit "useful-results/with-singletons.csv" percents-v2)
    (spit "useful-results/without-singletons.csv" percents-v3)
    percents-v3
    ))


(defn swi-packs []
  (let [file "useful-results/swi-pack-types-100000-with-calls"
        headline ";any;not any;restricted any;total vars;unknown calls;total calls"
        names (->> file
                   (analysis-v3 false)
                   (map :filename))
        modifier (fn [x] (->> x
                             (map percents-of-single-pack)
                             (map (juxt :filename (comp :any :vars) (comp :not-any :vars) (comp :any-with-compatible :vars) (comp :total :vars) (comp :unknown :calls) (comp :total :calls)))
                             (map (partial map #(if (float? %) (format "%.4f" %) (str %))))
                             (map (partial clojure.string/join "&"))
                             (cons headline)
                             (clojure.string/join "\\\\\n")
                             ))
        with-singletons (modifier (analysis-v3 false file))
        without-singletons (modifier (analysis-v3 true file))
        ]
    (spit "useful-results/calls-and-singletons.csv" with-singletons)
    (spit "useful-results/calls-and-no-singletons.csv" without-singletons)
    without-singletons
    ))

(swi-packs)
