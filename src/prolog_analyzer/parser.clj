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
  (.getAbsolutePath (io/file "source.edn")))


(defn- split-up-error-message [msg]
  (->> msg
       clojure.string/split-lines
       (partition 2)
       (map (partial apply str))
       (apply vector)))

(defn transform-to-edn [clojure-file]
  (try
    (with-open [in (java.io.PushbackReader. (clojure.java.io/reader clojure-file))]
      (let [edn-seq (repeatedly (partial edn/read {:eof :theend} in))]
        (doall (take-while (fn [e] (not= :theend e)) edn-seq))))
    (catch RuntimeException e
      (printf "Error parsing edn file '%s': '%s\n" clojure-file (.getMessage e)))))
;; https://stackoverflow.com/questions/15234880/how-to-use-clojure-edn-read-to-get-a-sequence-of-objects-in-a-file

(defmulti call-prolog (fn [dialect term-expander prolog-exe file] dialect))

(defmethod call-prolog "swipl" [dialect term-expander prolog-exe file]
  (let [clojure-file (get-clojure-file-name file)
        path-to-analyzer (str "'" term-expander "'")
        goal (str "use_module(" path-to-analyzer ", [set_file/1, enable_write_out/0]),"
                  "set_file('" clojure-file "'),"
                  "['" file "'],"
                  "halt.")
        {err :err} (sh/sh "swipl" "-g" goal "-q" :env (into {} (System/getenv)))]
    err))

(defmethod call-prolog "sicstus" [dialect term-expander prolog-exe file]
  (let [current-hash (hash (slurp file))
        clojure-file (get-clojure-file-name file)
        path-to-analyzer (str "'" term-expander "'")
        goal (str "use_module(" path-to-analyzer ", [set_file/1]),"
                  "set_file('" clojure-file "'),"
                  "['" file "'],"
                  "prolog_analyzer:close_orphaned_stream,"
                  "halt.")
        {err :err} (sh/sh prolog-exe "--goal" goal "--noinfo" :env (into {} (System/getenv)))]
    err))


(defn read-prolog-code-as-raw-edn
  "Parses a prolog file to edn.
  No additional modification is done on the created data."
  [dialect term-expander prolog-exe file]
  (log/debug (str "Dialect: " dialect))
  (log/debug (str "Start writing of file " file))
  (call-prolog dialect term-expander prolog-exe file)
  (log/debug (str "Start reading of edn"))
  (transform-to-edn (get-clojure-file-name file)))


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
    "any" (r/->AnySpec)
    "ground" (r/->GroundSpec)
    "var" (r/->VarSpec)
    "string" (r/->StringSpec)
    "nonvar" (r/->NonvarSpec)
    "number" (r/->NumberSpec)
    "float" (r/->FloatSpec)
    "integer" (r/->IntegerSpec)
    "atom" (r/->AtomSpec)
    "atomic" (r/->AtomicSpec)
    "int" (r/->IntegerSpec)
    "emptylist" (r/->EmptyListSpec)
    (r/->UserDefinedSpec term)))

(defmethod transform-spec [:compound "list"] [{[type] :arglist}]
  (r/->ListSpec (transform-spec type)))

(defmethod transform-spec [:compound "compound"] [{[{functor :functor arglist :arglist}] :arglist}]
  (r/->CompoundSpec functor (map transform-spec arglist)))

(defmethod transform-spec [:compound "one_of"] [{inner-list :arglist}]
  (let [arglist (utils/get-elements-of-list (first inner-list))]
    (r/->OneOfSpec (map transform-spec arglist))))

(defmethod transform-spec [:compound "and"] [{inner-list :arglist}]
  (let [arglist (utils/get-elements-of-list (first inner-list))]
    (r/->AndSpec (map transform-spec arglist))))

(defmethod transform-spec [:compound "tuple"] [{inner-list :arglist}]
  (let [arglist (utils/get-elements-of-list (first inner-list))]
    (r/->TupleSpec (map transform-spec arglist))))

(defmethod transform-spec [:compound "atom"] [{arglist :arglist}]
  (r/->ExactSpec (:term (first arglist))))

(defmethod transform-spec [:compound "specvar"] [{[spec] :arglist}]
  (r/->SpecvarSpec (:name spec)))

(defmethod transform-spec :default [spec]
  (assoc (r/->UserDefinedSpec (:functor spec)) :arglist (map transform-spec (:arglist spec))))

(defn- validation-spec? [{[_ & args] :arglist}]
  (= 1 (count args)))

(defn- specs-to-map [{[outer & args] :arglist :as spec}]
  (let [module (get-in outer [:arglist 0 :term])
        functor (get-in outer [:arglist 1 :arglist 0 :term])
        arity (get-in outer [:arglist 1 :arglist 1 :value])]
    (if (validation-spec? spec)
      (hash-map (vector module functor arity) (map (comp (partial map transform-spec) utils/get-elements-of-list) args))
      (hash-map (vector module functor arity) (list (map (comp (partial map transform-spec) utils/get-elements-of-list) args))))))

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
  (log/debug "Start formatting of edn")
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

(defn add-built-ins [dialect data]
  (log/debug "Add built-ins")
  (let [built-in (-> "/home/isabel/Studium/prolog-analyzer/prolog/builtins.pl"
                     (#(read-prolog-code-as-raw-edn "swipl" "prolog/prolog_analyzer.pl" "swipl" %))
                     format-and-clean-up
                     pre-processor/pre-process-single
                     (dissoc :error-msg))]
    (merge-with into data built-in)
    ))


(def preamble
  ":- module(tmp,[]).\n
  :- use_module(prolog_analyzer,[enable_write_out/0,declare_spec/1,define_spec/2,spec_pre/2,spec_post/3,spec_invariant/2]).\n
  :- enable_write_out.\n\n")

(defn process-prolog-file [dialect term-expander prolog-exe file-name]
  (when (.exists (io/file (get-clojure-file-name file-name)))
    (io/delete-file (get-clojure-file-name file-name)))
  (->> file-name
       (read-prolog-code-as-raw-edn dialect term-expander prolog-exe)
       format-and-clean-up
       pre-processor/pre-process-single
       #_(add-built-ins dialect)
       ))

(defn process-prolog-files [dialect term-expander prolog-exe & file-names]
  (->> file-names
       (filter #(.endsWith % ".pl"))
       (map (partial read-prolog-code-as-raw-edn dialect term-expander prolog-exe))
       (map format-and-clean-up)
       (apply pre-processor/pre-process-multiple)
       (add-built-ins dialect)))

(defn process-prolog-directory [dialect term-expander prolog-exe dir-name]
  (->> dir-name
       io/file
       (tree-seq #(.isDirectory %) #(.listFiles %))
       (remove #(.isDirectory %))
       (map #(.getPath %))
       (map str)
       (remove #(.endsWith % ".edn"))
       (apply process-prolog-files dialect term-expander prolog-exe)
       ))

(defn process-edn [edn]
  (-> edn
      transform-to-edn
      format-and-clean-up
      pre-processor/pre-process-single))
