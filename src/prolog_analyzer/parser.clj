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

(defn- split-up-error-message [msg]
  (->> msg
       clojure.string/split-lines
       (partition 2)
       (map (partial apply str))
       (apply vector)))

(defn call-swipl [file]
  (let [{err :err} (sh/sh "swipl" "-f" file "-q" "-t" "halt.")]
    {:type :error-msg
     :content (split-up-error-message err)}))


(defn- replace-backslash [clojure-file]
  (spit clojure-file (.replace (slurp clojure-file) "\\" "\\\\")))

(defn- transform-to-edn [clojure-file]
  (replace-backslash clojure-file)
  (try
    (with-open [in (java.io.PushbackReader. (clojure.java.io/reader clojure-file))]
      (let [edn-seq (repeatedly (partial edn/read {:eof :theend} in))]
        (doall (take-while (partial not= :theend) edn-seq))))
    (catch RuntimeException e
      (printf "Error parsing edn file '%s': '%s\n" clojure-file (.getMessage e)))))
;; https://stackoverflow.com/questions/15234880/how-to-use-clojure-edn-read-to-get-a-sequence-of-objects-in-a-file

(defn read-prolog-code [file]
  (let [error-msg (call-swipl file)
        clojure-file (get-clojure-file-name file)
        result (transform-to-edn clojure-file)]
    (io/delete-file clojure-file)
    (conj result error-msg)))
