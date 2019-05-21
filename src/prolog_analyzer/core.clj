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

(defn print-result [dir edn? results]
  (println (pr-str (.getAbsolutePath (io/file dir))))
  (let [printer (if edn? my-pp/print-types-and-errors-v3 my-pp/pretty-print-graph)]
    (doseq [res results]
      (printer res))))

(defn run
  ([edn edn?]
   (print-result edn edn?
    (->> edn
         parser/process-edn
         global/global-analysis
         )))
  ([dialect term-expander file prolog-exe]
   (run dialect term-expander file prolog-exe "false"))
  ([dialect term-expander file prolog-exe edn?]
   (print-result file (read-string edn?)
    (if (.isDirectory (io/file file))
      (->> file
           (parser/process-prolog-directory dialect term-expander prolog-exe)
           global/global-analysis
           )
      (->> file
           (parser/process-prolog-file dialect term-expander prolog-exe)
           global/global-analysis
           )))))

(defn -main [& args]
  (apply run args)
  (<╯°□°>╯︵┻━┻))
