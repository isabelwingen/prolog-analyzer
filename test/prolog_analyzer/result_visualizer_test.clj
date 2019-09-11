(ns prolog-analyzer.result-visualizer-test
  (:require [prolog-analyzer.result-visualizer :as sut]
            [prolog-analyzer.utils :as utils]
            [prolog-analyzer.parser.parser :as parser]
            [clojure.java.io :as io]
            [midje.sweet :refer :all]))


(def data (parser/process-prolog-file "swipl" "prolog/prolog_analyzer.pl" "swipl" "resources/simple-example.pl"))

(defn check-creation [d]
  (doseq [x (rest (file-seq (io/file "doc")))]
    (io/delete-file (io/file x)))
  (sut/htmlify-data d)
  (file-seq (io/file "doc")))

(fact
 (check-creation data)
 => (five-of #(= java.io.File (type %))))
