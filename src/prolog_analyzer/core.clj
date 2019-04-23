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
   (->> edn
        parser/process-edn
        (analyzer/complete-analysis my-pp/better-print))
   (<╯°□°>╯︵┻━┻)
    )
  ([dialect term-expander file prolog-exe]
   (->> file
        (parser/process-prolog-file dialect term-expander prolog-exe)
        (analyzer/complete-analysis my-pp/better-print)
        )
   (<╯°□°>╯︵┻━┻)
    ))

  (def sic "/usr/local/sicstus4.4.1/bin/sicstus-4.4.1")
