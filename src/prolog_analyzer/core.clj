(ns prolog-analyzer.core
  (:gen-class)
  (:require [prolog-analyzer.parser :as parser]
            [clojure.pprint :refer [pprint]]))

(defn -main
  "Start analyzing of source file"
  [file]
  (doseq [elem (parser/read-prolog-code file)]
    (pprint elem)))
