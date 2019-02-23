(ns prolog-analyzer.core
  (:gen-class)
  (:require [prolog-analyzer.parser :as parser]
            [prolog-analyzer.analyzer.core :as analyzer]
            [prolog-analyzer.analyzer.pretty-printer :as my-pp]
            [clojure.pprint :refer [pprint]]))

(defn -main
  "Start analyzing of source file"
  [file]
  (-> file
      parser/process-prolog-file
      analyzer/complete-analysis
      my-pp/pretty-print-analysis-result
      ))
