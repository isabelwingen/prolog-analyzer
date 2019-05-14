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


(defn analysis [file]
  (->> (get-data file)
       (remove #(and (string? %) (not= "Call prolog" %)))
       (partition-by string?)
       (partition 2)
       (map second)
       (map percent)
       ))


(defn mean [l]
  (let [c (count l)
        sum (apply +' l)]
    (float (/ sum c))))



(defn compare [l1 l2 index]
  (let [res1 (nth l1 index)
        res2 (nth l2 index)
        perc1 (percent res1)
        perc2 (percent res2)
        diff (- perc1 perc2)]
    (when (not= perc1 perc2)
      (print index ": ")
      (println diff))
    ;(clojure.pprint/pprint res1)
    ;(clojure.pprint/pprint res2)
    ))

(defn compare2 [index]
  (let [l1 (analysis "results/type-analysis4")
        l2 (analysis "results/type-analysis3")
        k1 (map (comp percent vector) (nth l1 14))
        k2 (map (comp percent vector) (nth l2 14))
        m1 (nth (nth l1 14) 6)
        m2 (nth (nth l2 14) 6)]
    (doseq [x (keys m1)]
      (clojure.pprint/pprint x)
      (clojure.pprint/pprint (get m1 x))
      (clojure.pprint/pprint (get m2 x))
      (println "-----------------"))
    ))

(mean (analysis "results/with-lists"))
(mean (analysis "results/without-lists"))
