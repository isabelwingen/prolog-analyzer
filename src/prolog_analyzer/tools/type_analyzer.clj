(ns prolog-analyzer.tools.type-analyzer
  (:require
   [clojure.string]
   [clojure.java.io :as io]))


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


(defn analysis-v2-include-singletons
  "Can handle print from print-type-and-errors. Do not exclude singletons"
  [file]
  (->> (get-data file)
       (remove #(and (string? %) (not (.startsWith % "/home"))))
       (partition-by string?)
       (partition 2)
       (map #(hash-map :size (first %) :data (second %)))
       (map (fn [t] (update t :size (partial filter #(.startsWith % "/home")))))
       (map #(update % :size last))
       (map #(update % :size (comp psize io/file)))
       (map #(update % :data (partial take-nth 2))) ; filters out errors
       (map #(update % :data (sort-v2 false)))
       ))

(defn analysis-v2-exclude-singletons
  "Exclude singletons."
  [file]
  (->> (get-data file)
       (remove #(and (string? %) (not (.startsWith % "/home")))) ; splits for packages
       (partition-by string?)
       (partition 2)
       (map #(hash-map :size (first %) :data (second %)))
       (map (fn [t] (update t :size (partial filter #(.startsWith % "/home")))))
       (map #(update % :size last))
       (map #(update % :size (comp psize io/file)))
       (map #(update % :data (partial take-nth 2))) ; filters out errors
       (map #(update % :data (sort-v2 true)))
       ))

(defn percents-of-single-pack [{total :total :as data}]
  (-> data
      (update :any #(float (/ % total)))
      (update :not-any #(float (/ % total)))
      (update :any-with-compatible #(float (/ % total)))))

(defn percents
  "fn must be analysis-v2 or analysis-v2"
  [fn file]
  (->> file
       fn
       (map :data)
       (apply merge-with +')
       percents-of-single-pack))


(defn swi-packs-complete []
  (let [file-list ["useful-results/swi-pack-types-20000-0"
                   "useful-results/swi-pack-types-20000-1"
                   "useful-results/swi-pack-types-20000-3"
                   "useful-results/swi-pack-types-20000-6"
                   "useful-results/swi-pack-types-20000-12"
                   ]
        percents-v2 (->> file-list
                         (map analysis-v2)
                         (map #(map (comp percents-of-single-pack :data) %))
                         (map (partial map vals))
                         (map (partial map (partial map #(if (float? %) (format "%.2f" %) (str %)))))
                         (map (partial map (partial clojure.string/join ";")))
                         (apply map vector)
                         (map (partial clojure.string/join "; ;"))
                         (clojure.string/join "\n")
                         )
        percents-v3 (->> file-list
                         (map analysis-v3)
                         (map #(map (comp percents-of-single-pack :data) %))
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
    (hash-map :v2 (map (partial percents analysis-v2) file-list)
              :v3 (map (partial percents analysis-v3) file-list))
    ))


(defn prob-type-analysis [path]
  (let [m (->> path
               get-data
               (remove string?)
               (take-nth 2)
               (remove empty?)
               (mapcat vals)
               (map #(if (= :any (:record-type %)) :any :not-any))
               frequencies)
        total (apply +' (vals m))]
    (-> m
        (assoc :total total)
        (update :any #(float (/ % total)))
        (update :not-any #(float (/ % total)))
        )
    ))
