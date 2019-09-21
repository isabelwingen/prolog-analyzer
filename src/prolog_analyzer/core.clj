(ns prolog-analyzer.core
  (:gen-class)
  (:require [prolog-analyzer.parser.parser :as parser]
            [prolog-analyzer.analyzer.global-analysis :as global]
            [clojure.java.io :as io]
            [clojure.pprint :refer [pprint]]
            [tableflisp.core :refer :all]
            [clojure.java.io :refer [writer]]
            [prolog-analyzer.result-visualizer :as visualizer]
            ))

(declare analyze-swi-packs)

(defn write [data counter]
  (visualizer/htmlify-data data)
  (visualizer/print-intermediate-result counter data)
  (visualizer/print-errors counter data))


(defn run
  ([edn]
   (->> edn
        parser/process-edn
        (global/global-analysis write)
        ))
  ([dialect term-expander file prolog-exe]
   (run dialect term-expander file prolog-exe "false"))
  ([dialect term-expander file prolog-exe edn?]
   (if (.isDirectory (io/file file))
     (->> file
          (parser/process-prolog-directory dialect term-expander prolog-exe)
          (global/global-analysis write)
          )
     (->> file
          (parser/process-prolog-file dialect term-expander prolog-exe)
          (global/global-analysis write)
          ))))


(defn -main [& args]
  (apply run args)
  (<╯°□°>╯︵┻━┻))
