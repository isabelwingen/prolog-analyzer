(ns prolog-analyzer.parser
  (:require [instaparse.core :as insta]
            [clojure.pprint :refer [pprint]]
            [clojure.tools.logging :as log]
            [clojure.java.io :as io]
            [clojure.edn :as edn]
            [clojure.java.shell :as sh]))

;(defn read-prolog-code [file]
 ; (with-open [in (java.io.PushbackReader. (clojure.java.io/reader file))]
  ;  (let [edn-seq (repeatedly (partial edn/read {:eof :theend} in))]
   ;   (dorun (map println (take-while (partial not= :theend) edn-seq))))))
(defn get-clojure-file-name [file]
  (str file ".edn"))

(defn call-swipl [file]
  (sh/sh "swipl" "-f" file "-q" "-g" "halt."))

(defn- transform-to-edn [clojure-file]
  (with-open [in (java.io.PushbackReader. (clojure.java.io/reader clojure-file))]
    (let [edn-seq (repeatedly (partial edn/read {:eof :theend} in))]
      (doall (take-while (partial not= :theend) edn-seq)))))
;; https://stackoverflow.com/questions/15234880/how-to-use-clojure-edn-read-to-get-a-sequence-of-objects-in-a-file

(defn read-prolog-code [file]
  (call-swipl file)
  (let [clojure-file (get-clojure-file-name file)
        result (transform-to-edn clojure-file)]
    (io/delete-file clojure-file)
    result))


