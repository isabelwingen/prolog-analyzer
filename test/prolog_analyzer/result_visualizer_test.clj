(ns prolog-analyzer.result-visualizer-test
  (:require [prolog-analyzer.result-visualizer :as sut]
            [prolog-analyzer.utils :as utils]
            [prolog-analyzer.parser.parser :as parser]
            [clojure.java.io :as io]
            [midje.sweet :refer :all]))


(def data (parser/process-prolog-file "swipl" "prolog/prolog_analyzer.pl" "swipl" "resources/simple-example.pl"))

(defn check-creation [d]
  (sut/htmlify-data d)
  (file-seq (io/file "doc/html")))

(fact
 (check-creation data)
 => (five-of #(= java.io.File (type %))))
