(ns prolog-analyzer.core
  (:gen-class)
  (:require [prolog-analyzer.parser :as parser]
            [prolog-analyzer.analyzer.core :as analyzer]
            [prolog-analyzer.analyzer.pretty-printer :as my-pp]
            [clojure.java.io :as io]
            [clojure.pprint :refer [pprint]]
            [tableflisp.core :refer :all]))

(defn -main
  "Start analyzing of source file"
  ([edn]
   (doseq [res (->> edn
                    parser/process-edn
                    analyzer/complete-analysis)]
     (my-pp/print-with-indices res))
   ;(<╯°□°>╯︵┻━┻)
    )
   ([dialect term-expander file prolog-exe]
    (doseq [res (if (.isDirectory (io/file file))
                  (->> file
                       (parser/process-prolog-directory dialect term-expander prolog-exe)
                       analyzer/complete-analysis
                       )
                  (->> file
                       (parser/process-prolog-file dialect term-expander prolog-exe)
                       analyzer/complete-analysis
                       ))]
      (my-pp/print-with-indices res))
  ; (<╯°□°>╯︵┻━┻)
    ))

  (def sic "/usr/local/sicstus4.4.1/bin/sicstus-4.4.1")
