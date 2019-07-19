(ns prolog-analyzer.test-helper
  (:require [prolog-analyzer.parser :as parser]))

(defn read-in-file [path]
  (parser/process-prolog-file "swipl" "prolog/prolog_analyzer.pl" "swipl" path))
