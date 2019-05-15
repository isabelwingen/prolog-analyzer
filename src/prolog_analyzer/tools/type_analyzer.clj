(ns prolog-analyzer.tools.type-analyzer
  (:require
   [clojure.string]
   [clojure.java.io :as io]))


(defn get-data [file]
  (read-string (str \[ (slurp file) \])))

(defn create-map-for-vars [data]
  (->> data
       (remove string?)
       (mapcat vals)
       frequencies
       ))

(defn bla [freq-map]
  (let [total (apply +' (vals freq-map))
        nils (get freq-map nil 0)
        anys (get freq-map {:record-type :any} 0)]
    {:total total :any (+ anys nils)}))

(defn percent [data]
  (let [freq-map (create-map-for-vars data)
        total (apply +' (vals freq-map))
        nils (get freq-map nil 0)
        anys (get freq-map {:record-type :any} 0)]
    (if (zero? total)
      0
      (float (/ (+ anys nils) total)))))

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

(defn- sort-asc [dirs limit]
  (->> dirs
       (map #(hash-map :size (psize %) :dir %))
       (sort-by :size)
       (take-while #(< (:size %) limit))))

(defn analysis [file]
  (->> (get-data file)
       (remove #(and (string? %) (not (.startsWith % "/home"))))
       (partition-by string?)
       (partition 2)
       (map #(hash-map :size (first %) :data (second %)))
       (map (fn [t] (update t :size (partial filter #(.startsWith % "/home")))))
       (map #(update % :size last))
       (map #(update % :size (comp psize io/file)))
       (map #(update % :data percent))
       ))


;; Analysis for prints of print-types-and-errors-v2

(defn sort [data]
  (let [vars (->> data
                  (mapcat keys)
                  (filter #(= :var (:record-type %))))
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


(defn analysis-v2 [file]
  (->> (get-data file)
       (remove #(and (string? %) (not (.startsWith % "/home"))))
       (partition-by string?)
       (partition 2)
       (map #(hash-map :size (first %) :data (second %)))
       (map (fn [t] (update t :size (partial filter #(.startsWith % "/home")))))
       (map #(update % :size last))
       (map #(update % :size (comp psize io/file)))
       (map #(update % :data sort))
       ))

(defn percents [file]
  (let [m (->> file
               analysis-v2
               (map :data)
               (apply merge-with +'))
        total (:total m)]
    (-> m
        (update :any #(float (/ % total)))
        (update :not-any #(float (/ % total)))
        (update :any-with-compatible #(float (/ % total))))))
