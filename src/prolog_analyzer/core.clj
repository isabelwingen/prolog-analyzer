(ns prolog-analyzer.core
  (:gen-class)
  (:require [prolog-analyzer.parser :as parser]
            [prolog-analyzer.analyzer.core :as analyzer]
            [prolog-analyzer.analyzer.pretty-printer :as my-pp]
            [clojure.java.io :as io]
            [clojure.pprint :refer [pprint]]))

(defn -main
  "Start analyzing of source file"
  [file]
  (if (.isDirectory (io/file file))
    (-> file
        parser/process-prolog-directory
        analyzer/complete-analysis
        (my-pp/short-result file))
    (-> file
        parser/process-prolog-file
        analyzer/complete-analysis
        (my-pp/short-result file)
        )))
