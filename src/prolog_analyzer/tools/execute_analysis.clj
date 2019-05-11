(ns prolog-analyzer.tools.execute-analysis
  (:require
   [clojure.java.io :as io])
    )


(defn psize [f]
  (if (.isDirectory f)
    (apply + (pmap psize (.listFiles f)))
    (.length f)))

(defn sort-asc [dirs limit]
  (->> dirs
       (map #(hash-map :size (psize %) :dir %))
       (sort-by :size)
       (take-while #(< (:size %) limit))))

(defn execute-run [dir f]
  (try
    (do
      (f dir)
      (str dir ": ok\n"))
    (catch Exception e (str dir ": " (.getMessage e) "\n"))))


(defn test-dir [dir limit func]
  (let [subdirs (.listFiles (io/file dir))
        res (apply vector (for [f (sort-asc subdirs limit)]
                            (execute-run (.getAbsolutePath (:dir f)) func)))]
    res))
