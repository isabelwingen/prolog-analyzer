(ns prolog-analyzer.core
  (:gen-class)
  (:require [prolog-analyzer.parser :as parser]
            [prolog-analyzer.analyzer.core :as analyzer]
            [clojure.java.io :as io]
            [clojure.pprint :refer [pprint]]
            [tableflisp.core :refer :all]
            [clojure.java.io :refer [writer]]
            [prolog-analyzer.result-visualizer :as visualizer]
            ))

(declare analyze-swi-packs)

(defn run
  ([edn edn?]
   (->> edn
        parser/process-edn
        analyzer/complete-analysis
        ))
  ([dialect term-expander file prolog-exe]
   (run dialect term-expander file prolog-exe "false"))
  ([dialect term-expander file prolog-exe edn?]
   (if (.isDirectory (io/file file))
     (->> file
          (parser/process-prolog-directory dialect term-expander prolog-exe)
          analyzer/complete-analysis
          )
     (->> file
          (parser/process-prolog-file dialect term-expander prolog-exe)
          analyzer/complete-analysis
          ))))

(defn write [envs]
  (visualizer/to-json envs "tmp/result.tmp"))

(defn -main [& args]
  (write (apply run args))
  (<╯°□°>╯︵┻━┻))
