(ns prolog-analyzer.parser
  (:require [instaparse.core :as insta]
            [clojure.pprint :refer [pprint]]
            [clojure.tools.logging :as log]
            [clojure.java.io :as io]
            [clojure.edn :as edn]
            [clojure.java.shell :as sh]
            [clojure.set :refer [rename-keys]]))

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

(defmulti transform-spec (juxt :type :functor))

(defmethod transform-spec [:atom nil] [{term :term}]
  (let [spec (case term
               "any" :any
               "ground" :ground
               "var" :var
               "nonvar" :nonvar
               "number" :number
               "float" :float
               "integer" :integer
               "atom" :atom
               "atomic" :atomic
               "int" :integer
               term)]
    {:spec spec}))


(defmethod transform-spec [:compound "list"] [{[type] :arglist}]
  {:spec :list :type (transform-spec type)})

(defmethod transform-spec [:compound "compound"] [{[{functor :functor arglist :arglist}] :arglist}]
  {:spec :compound :functor functor :arglist (map transform-spec arglist)}
  )

(defmethod transform-spec [:compound "one_of"] [{inner-list :arglist}]
  (let [arglist (:arglist (first inner-list))]
    {:spec :one_of :arglist (map transform-spec arglist)}))

(defmethod transform-spec [:compound "and"] [{inner-list :arglist}]
  (let [arglist (:arglist (first inner-list))]
    {:spec :and :arglist (map transform-spec arglist)}))

(defmethod transform-spec [:compound "tuple"] [{inner-list :arglist}]
  (let [arglist (:arglist (first inner-list))]
    {:spec :tuple :arglist (map transform-spec arglist)}))

(defmethod transform-spec [:compound "atom"] [{arglist :arglist}]
  {:spec :exact :value (:term (first arglist))})

(defmethod transform-spec [:compound "any"] [{[spec] :arglist}]
  {:spec :any :name (:name spec)}
  )

(defmethod transform-spec :default [spec]
  {:spec (:functor spec) :arglist (map transform-spec (:arglist spec))}
  )


(defn- spec-to-map [{[pred & rest] :arglist}]
  (let [functor (get-in pred [:arglist 0 :term])
        arity (get-in pred [:arglist 1 :value])]
    (if (= 1 (count rest))
      (hash-map (vector functor arity) (map (comp #(map transform-spec %) :arglist) rest))
      (hash-map (vector functor arity) (list (map (comp #(map transform-spec %) :arglist) rest)))))
  )

(defn order-specs [specs]
  (->> specs
       (map spec-to-map)
       (apply merge-with into)
       (reduce-kv (fn [m keys v] (update-in m keys #(into % v))) {})
       ))

(defn order-declare-specs [declares]
  (map (comp first :arglist) declares))

(defn order-define-specs [defines]
  (let [keys (map (comp first :arglist) defines)]
    (if (= keys (distinct keys))
      (->> (map :arglist defines)
           (into {})
           (reduce-kv (fn [m k v] (assoc m (transform-spec k) (transform-spec v))) {}))
      (println "error defining specs")   ;; TODO: log error and better message
      )
    ))


(defn clean-up-spec-definitions [central-map]
  (let [specs (:define_spec central-map)]
    (-> central-map
        (dissoc :define_spec)
        (dissoc :declare_spec)
        (assoc :specs specs))))

(defn- transform-empty-list [arg]
  (if (= {:term "[]" :type :atomic} arg)
    {:type :list :arglist []}
    arg))

(defn order-preds [preds]
  (->> (group-by (juxt :module :name :arity) preds)
       ((fn [coll] (apply-function-on-values coll (partial map #(-> % (dissoc :module) (dissoc :name) (dissoc :arity))))))
       ((fn [coll] (apply-function-on-values coll (partial map #(update % :arglist (partial map transform-empty-list))))))
       (reduce-kv
        (fn [m [module name arity] v]
          (if (= name "end_of_file")
            m
            (update-in m [module name arity] #(into % v))))
        {})
       ))


(defn process-prolog-file [file]
  (let [raw (read-prolog-code-as-raw-edn file)]
    (-> raw
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
                                  :pred :preds})
        )))

(def preamble
  ":- module(tmp,[]).\n:- use_module(prolog_analyzer,[enable_write_out/0,declare_spec/1,define_spec/2,spec_pre/2,spec_post/3,spec_invariant/2]).\n:- enable_write_out.\n\n")

(defn process-prolog-snippets [code]
  (spit "prolog/tmp.pl" (str preamble code))
  (let [res (process-prolog-file "prolog/tmp.pl")]
    (io/delete-file "prolog/tmp.pl")
    res))
