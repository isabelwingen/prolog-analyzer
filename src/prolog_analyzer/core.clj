(ns prolog-analyzer.core
  (:gen-class)
  (:require [prolog-analyzer.parser :as parser]
            [prolog-analyzer.analyzer.core :as analyzer]
            [prolog-analyzer.analyzer.global-analysis :as global]
            [prolog-analyzer.analyzer.pretty-printer :as my-pp]
            [clojure.java.io :as io]
            [clojure.pprint :refer [pprint]]
            [tableflisp.core :refer :all]
            ))

(declare analyze-swi-packs)

(defn print-result [results]
  (doseq [res results]
    (my-pp/print-domains-of-variables res)))

(defn run
  ([edn]
   (print-result
    (->> edn
         parser/process-edn
         global/global-analysis
         )))
  ([dir limit]
   (analyze-swi-packs dir limit))
  ([dialect term-expander file prolog-exe]
   (print-result
    (if (.isDirectory (io/file file))
      (->> file
           (parser/process-prolog-directory dialect term-expander prolog-exe)
           global/global-analysis
           )
      (->> file
           (parser/process-prolog-file dialect term-expander prolog-exe)
           global/global-analysis
           )))))

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


(defn analyze-swi-packs [dir limit]
  (let [packs (->> (read-string limit)
                   (sort-asc (.listFiles (io/file dir)))
                   (map :dir)
                   (map #(.getAbsolutePath %)))]
    (doseq [pack packs]
      (run "swipl" "prolog/prolog_analyzer.pl" pack "swipl"))))

(defn -main [& args]
  (apply run args)
  (<╯°□°>╯︵┻━┻))
