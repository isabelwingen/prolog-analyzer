(ns prolog-analyzer.analyzer.type-analyzer
  (:require
   [clojure.string]))


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
    (float (/ (+ anys nils) total))))
