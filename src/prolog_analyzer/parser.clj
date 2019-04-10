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

(defn- call-swipl [xxx file]
  (let [current-hash (hash (slurp file))
        clojure-file (get-clojure-file-name file)
        path-to-analyzer (str "'" xxx "'")
        goal (str "use_module(" path-to-analyzer ", [set_file/1, enable_write_out/0]),"
                  "set_file('" file "'),"
                  "enable_write_out,"
                  "['" file "'],"
                  "halt.")
        {err :err} (sh/sh "swipl" "-g" goal "-q")]
    (spit clojure-file
          (str "\n{:type :error-msg :content " (split-up-error-message err) "}")
          :append true)
    (spit clojure-file
          (str "\n{:hash " current-hash "}")
          :append true)))

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
  [xxx file]
  (log/debug (str "Start reading of file" file))
  (when (not (.exists (io/file (get-clojure-file-name file))))
    (do (log/debug (str "Call swipl on " file " the first time")) (call-swipl xxx file)))
  (let [pre-result (do (log/debug (str "Transform " file)) (transform-to-edn (get-clojure-file-name file)))
        current-hash (hash (slurp file))
        old-hash (:hash (last pre-result))]
    (if (= old-hash current-hash)
      pre-result
      (do
        (log/debug (str "File " file " changed, call swipl."))
        (call-swipl xxx file)
        (log/debug (str "File " file " changed, transform to edn."))
        (transform-to-edn (get-clojure-file-name file))))))


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

(defn add-built-ins [data]
  (log/debug "Add built-ins")
  (let [built-in (-> "prolog/builtins.pl"
                     (#(read-prolog-code-as-raw-edn "prolog/prolog_analyzer.pl" %))
                     format-and-clean-up
                     pre-processor/pre-process-single
                     (dissoc :error-msg))]
    (merge-with into data built-in)
    ))


(def preamble
  ":- module(tmp,[]).\n
  :- use_module(prolog_analyzer,[enable_write_out/0,declare_spec/1,define_spec/2,spec_pre/2,spec_post/3,spec_invariant/2]).\n
  :- enable_write_out.\n\n")

(defn process-prolog-file [xxx file-name]
  (->> file-name
       (read-prolog-code-as-raw-edn xxx)
       format-and-clean-up
       pre-processor/pre-process-single
       add-built-ins
       ))

(defn process-prolog-snippets [code]
  (spit "prolog/tmp.pl" (str preamble code))
  (let [res (process-prolog-file "prolog_analyzer" "prolog/tmp.pl")]
    (io/delete-file "prolog/tmp.pl")
    res))

(defn process-prolog-files [xxx & file-names]
  (->> file-names
       (filter #(.endsWith % ".pl"))
       (map (partial read-prolog-code-as-raw-edn xxx))
       (map format-and-clean-up)
       (apply pre-processor/pre-process-multiple)
       add-built-ins))

(defn process-prolog-directory [xxx dir-name]
  (->> dir-name
       io/file
       (tree-seq #(.isDirectory %) #(.listFiles %))
       (remove #(.isDirectory %))
       (map #(.getPath %))
       (map str)
       (remove #(.endsWith % ".edn"))
       (apply process-prolog-files xxx)
       ))
