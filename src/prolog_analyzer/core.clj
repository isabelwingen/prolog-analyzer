(ns prolog-analyzer.core
  (:gen-class)
  (:require [prolog-analyzer.parser :as parser]
            [prolog-analyzer.analyzer.core :as analyzer]
            [prolog-analyzer.analyzer.pretty-printer :as my-pp]
            [clojure.java.io :as io]
            [clojure.pprint :refer [pprint]]
            [tableflisp.core :refer :all]))



(defn print-result [results]
  (doseq [res results]
    (my-pp/print-type-analysis res)))

(defn -main
  "Start analyzing of source file"
  ([edn]
   (print-result
    (->> edn
         parser/process-edn
         analyzer/complete-analysis))
   (<╯°□°>╯︵┻━┻)
    )
   ([dialect term-expander file prolog-exe]
    (print-result
     (if (.isDirectory (io/file file))
       (->> file
            (parser/process-prolog-directory dialect term-expander prolog-exe)
            analyzer/complete-analysis
            )
       (->> file
            (parser/process-prolog-file dialect term-expander prolog-exe)
            analyzer/complete-analysis
            )))
    (<╯°□°>╯︵┻━┻)
     ))

  (def sic "/usr/local/sicstus4.4.1/bin/sicstus-4.4.1")
