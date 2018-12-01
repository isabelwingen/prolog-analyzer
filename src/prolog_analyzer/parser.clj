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

(defn read-prolog-code-as-raw-edn [file]
  (let [error-msg (call-swipl file)
        clojure-file (get-clojure-file-name file)
        result (transform-to-edn clojure-file)]
    (io/delete-file clojure-file)
    (conj result error-msg)))


(defn- apply-function-on-values [in-map func]
  (reduce-kv #(assoc %1 %2 (func %3)) {} in-map))

(defn group-by-and-apply [data f g]
  (-> (group-by f data)
      (apply-function-on-values g)
      ))

(defn interleaved-group-by
  ([coll f] (-> (group-by f coll) (apply-function-on-values (partial map #(dissoc % f)))))
  ([coll f & funcs] (-> (interleaved-group-by coll f) (apply-function-on-values #(apply interleaved-group-by % funcs)))))


(defn order-preds [preds]
  (interleaved-group-by preds :module :name :arity))

(defn order-preds2 [preds]
  (->> (group-by (juxt :module :name :arity) preds)
       ((fn [coll] (apply-function-on-values coll (partial map #(-> % (dissoc :module) (dissoc :name) (dissoc :arity))))))
       (reduce-kv (fn [m keys v] (update-in m keys #(into % v))) {})
       ))


(defn order-specs [specs]
  (->> specs
       (map :arglist)
       (map (fn [[{arglist :arglist} body]] {[(:term (first arglist)) (:value (second arglist))] [body]}))
       (apply merge-with into)
       (reduce-kv (fn [m [name arity] v] (update-in m [name arity] #(into % v))) {})))

(defn process-prolog-file [file]
  (let [raw (read-prolog-code-as-raw-edn file)]
    (-> raw
        (group-by-and-apply :type (partial map :content))
        (update :declare_spec (partial map #(get-in % [:arglist 0])))
        (update :define_spec (partial map :arglist))
        (update :spec_pre order-specs)
        (update :spec_post order-specs)
        (update :spec_inv order-specs)
        (update :pred order-preds2)
        )))


(process-prolog-file "resources/abs_int.pl")
