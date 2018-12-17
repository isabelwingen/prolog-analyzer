(ns prolog-analyzer.core
  (:gen-class)
  (:require [prolog-analyzer.parser :as parser]
            [prolog-analyzer.analyzer :as analyzer]
            [clojure.pprint :refer [pprint]]))

(defn -main
  "Start analyzing of source file"
  [file]
  (-> file
      parser/process-prolog-file
      analyzer/complete-analysis))

(-main "resources/spec-test.pl")
