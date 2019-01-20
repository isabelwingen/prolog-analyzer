(ns prolog-analyzer.core
  (:gen-class)
  (:require [prolog-analyzer.parser :as parser]
            [prolog-analyzer.analyzer.core :as analyzer]
            [prolog-analyzer.analyzer.pretty-printer :as my-pp]
            [clojure.pprint :refer [pprint]]))

(def data (atom {}))

(defn init-data-with-file [file-name]
  (->> file-name
       parser/process-prolog-file
       (reset! data)))

(defn init-data-with-files [& file-names]
  (->> file-names
       (apply parser/process-prolog-files)
       (reset! data)))

(defn -main
  "Start analyzing of source file"
  [file]
  (-> (init-data-with-file file)
      analyzer/complete-analysis
      ))

(-main "resources/spec-test.pl")
