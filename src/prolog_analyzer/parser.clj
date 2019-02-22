(ns prolog-analyzer.parser
  (:require [prolog-analyzer.pre-processor :as pre-processor]
            [prolog-analyzer.utils :as utils]
            [prolog-analyzer.records :as r]
            [clojure.pprint :refer [pprint]]
            [clojure.tools.logging :as log]
            [clojure.java.io :as io]
            [clojure.edn :as edn]
            [clojure.java.shell :as sh]
            [clojure.set :refer [rename-keys]]
            [clojure.string]))

(defn- get-clojure-file-name [file]
  (str file ".edn"))

(defn- split-up-error-message [msg]
  (->> msg
       clojure.string/split-lines
       (partition 2)
       (map (partial apply str))
       (apply vector)))

(defn- call-swipl [file]
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

(defn read-prolog-code-as-raw-edn
  "Parses a prolog file to edn.
  No additional modification is done on the created data."
  [file]
  (let [error-msg (call-swipl file)
        clojure-file (get-clojure-file-name file)
        result (transform-to-edn clojure-file)]
    (io/delete-file clojure-file)
    (conj result error-msg)))

(defn- apply-function-on-values [func in-map]
  (reduce-kv #(assoc %1 %2 (func %3)) {} in-map))

(defn- group-by-and-apply [data f g]
  (->> data
       (group-by f)
       (apply-function-on-values g)
       ))

(defmulti transform-spec "Transforms the raw edn of specs to a better suited format."
  (juxt :type :functor))

(defmethod transform-spec [:atom nil] [{term :term}]
  (case term
    "any" (r/make-spec:any)
    "ground" (r/make-spec:ground)
    "var" (r/make-spec:var)
    "nonvar" (r/make-spec:nonvar)
    "number" (r/make-spec:number)
    "float" (r/make-spec:float)
    "integer" (r/make-spec:integer)
    "atom" (r/make-spec:atom)
    "atomic" (r/make-spec:atomic)
    "int" (r/make-spec:integer)
    (r/make-spec:user-defined term)))

(defmethod transform-spec [:compound "list"] [{[type] :arglist}]
  (r/make-spec:list (transform-spec type)))

(defmethod transform-spec [:compound "compound"] [{[{functor :functor arglist :arglist}] :arglist}]
  (r/make-spec:compound functor (map transform-spec arglist)))

(defmethod transform-spec [:compound "one_of"] [{inner-list :arglist}]
  (let [arglist (r/get-elements-of-list (first inner-list))]
    (r/make-spec:one-of (map transform-spec arglist))))

(defmethod transform-spec [:compound "and"] [{inner-list :arglist}]
  (let [arglist (r/get-elements-of-list (first inner-list))]
    (r/make-spec:and (map transform-spec arglist))))

(defmethod transform-spec [:compound "tuple"] [{inner-list :arglist}]
  (let [arglist (r/get-elements-of-list (first inner-list))]
    (r/make-spec:tuple (map transform-spec arglist))))

(defmethod transform-spec [:compound "atom"] [{arglist :arglist}]
  (r/make-spec:exact (:term (first arglist))))

(defmethod transform-spec [:compound "specvar"] [{[spec] :arglist}]
  (r/make-spec:specvar (:name spec)))

(defmethod transform-spec :default [spec]
  (r/make-spec:user-defined (:functor spec) (map transform-spec (:arglist spec))))

(defn- specs-to-map [{[outer & args] :arglist}]
  (let [module (get-in outer [:arglist 0 :term])
        functor (get-in outer [:arglist 1 :arglist 0 :term])
        arity (get-in outer [:arglist 1 :arglist 1 :value])]
    (if (= 1 (count args))
      (hash-map (vector module functor arity) (map (comp (partial map transform-spec) r/get-elements-of-list) args))
      (hash-map (vector module functor arity) (list (map (comp (partial map transform-spec) r/get-elements-of-list) args))))))

(defn- order-specs [specs]
  (->> specs
       (map specs-to-map)
       (apply merge-with into)
       (reduce-kv (fn [m keys v] (update-in m keys #(into % v))) {})
       ))

(defn- order-declare-specs [declares]
  (map (comp first :arglist) declares))

(defn- order-define-specs [defines]
  (let [keys (map (comp first :arglist) defines)]
    (if (= keys (distinct keys))
      (->> (map :arglist defines)
           (into {})
           (reduce-kv (fn [m k v] (assoc m (transform-spec k) (transform-spec v))) {}))
      (log/error "Error defining specs"))))

(defn- clean-up-spec-definitions [central-map]
  (let [specs (:define_spec central-map)]
    (-> central-map
        (dissoc :define_spec)
        (dissoc :declare_spec)
        (assoc :specs specs))))

(defn- order-preds [preds]
  (->> (group-by (juxt :module :name :arity) preds)
       (apply-function-on-values (partial map #(-> % (dissoc :module) (dissoc :name) (dissoc :arity))))
       (apply-function-on-values #(->> %
                                       (interleave (range 0 (count %)))
                                       (apply hash-map)))
       (reduce-kv (fn [m [module name arity] v]
                    (if (= name "end_of_file")
                      m
                      (assoc-in m [module name arity] v)))
                  {})))

(defn- format-and-clean-up [data]
  (-> data
      (group-by-and-apply :type (partial map :content))
      (update :declare_spec order-declare-specs)
      (update :define_spec order-define-specs)
      clean-up-spec-definitions
      (update :spec_pre order-specs)
      (update :spec_post order-specs)
      (update :spec_inv order-specs)
      (update :pred order-preds)
      (rename-keys {:spec_pre :pre-specs
                    :spec_post :post-specs
                    :spec_inv :inv-specs
                    :pred :preds})))

(defn process-prolog-file [file-name]
  (-> file-name
      read-prolog-code-as-raw-edn
      format-and-clean-up
      pre-processor/pre-process))

(def preamble
  ":- module(tmp,[]).\n:- use_module(prolog_analyzer,[enable_write_out/0,declare_spec/1,define_spec/2,spec_pre/2,spec_post/3,spec_invariant/2]).\n:- enable_write_out.\n\n")

(defn process-prolog-snippets [code]
  (spit "prolog/tmp.pl" (str preamble code))
  (let [res (process-prolog-file "prolog/tmp.pl")]
    (io/delete-file "prolog/tmp.pl")
    res))

(defn process-prolog-files [& file-names]
  (let [results (for [file-name file-names] (process-prolog-file file-name))]
    (->> results
         (map #(dissoc % :error-msg))
         (apply merge-with (partial merge-with merge)))))

(defn process-prolog-directory [dir-name]
  (->> dir-name
      io/file
      file-seq
      rest
      (filter #(clojure.string/ends-with? (str %) ".pl"))
      (map str)
      (apply process-prolog-files)))
