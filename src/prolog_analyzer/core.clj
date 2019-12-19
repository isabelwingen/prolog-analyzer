(ns prolog-analyzer.core
  (:gen-class)
  (:require [clojure.java.io :as io]
            [prolog-analyzer.analyzer.global-analysis :as global]
            [prolog-analyzer.parser.parser :as parser]
            [prolog-analyzer.result-visualizer :as visualizer]
            [tableflisp.core :refer :all]))

(defn write [data counter]
  (visualizer/htmlify-data data)
  (visualizer/print-intermediate-result counter data)
  (visualizer/print-errors counter data))

(defn properties []
  (read-string (slurp (io/file "properties.edn"))))

(defn run
  ([file]
   (if (.isDirectory (io/file file))
     (->> file
          (parser/process-prolog-directory (properties))
          (global/global-analysis write)
          )
     (->> file
          (parser/process-prolog-file (properties))
          (global/global-analysis write)
          ))))

(defn -main [& args]
  (apply run args)
  (<╯°□°>╯︵┻━┻))
