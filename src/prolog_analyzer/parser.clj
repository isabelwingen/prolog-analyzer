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

(defn- get-name-without-ending [file-name]
  (first (clojure.string/split (.getName (io/file file-name)) #"\.")))

(defn- get-edn-file-name [file-name]
  (.getAbsolutePath (io/file (str "edns/" (get-name-without-ending file-name) ".edn"))))

(defn- split-up-error-message [msg]
  (->> msg
       clojure.string/split-lines
       (partition 2)
       (map (partial apply str))
       (apply vector)))

(defn transform-to-edn [clojure-file]
  (if (.exists (io/file clojure-file))
    (read-string (str \[ (clojure.string/replace (slurp clojure-file) "\\" "\\\\") \]))
    (do
      (log/warn clojure-file " No .edn file was created")
      [])))

(defmulti call-prolog (fn [dialect term-expander prolog-exe file edn-file] dialect))

(defmethod call-prolog "swipl" [dialect term-expander prolog-exe file edn-file]
  (println (pr-str "Call prolog"))
  (let [path-to-analyzer (str "'" term-expander "'")
        goal (str "use_module(" path-to-analyzer ", [set_file/1]),"
                  "set_file('" edn-file "'),"
                  "['" file "'],"
                  "prolog_analyzer:close_orphaned_stream,"
                  "halt.")
        {err :err} (sh/sh "swipl" "-g" goal "-q" :env (into {} (System/getenv)))]
    err))

(defmethod call-prolog "sicstus" [dialect term-expander prolog-exe file edn-file]
  (println (pr-str "Call prolog"))
  (let [path-to-analyzer (str "'" term-expander "'")
        goal (str "use_module(" path-to-analyzer ", [set_file/1]),"
                  "set_file('" edn-file "'),"
                  "['" file "'],"
                  "prolog_analyzer:close_orphaned_stream,"
                  "halt.")
        {err :err} (time (sh/sh prolog-exe "--goal" goal "--noinfo" :env (into {} (System/getenv))))]
    err))


(defn- apply-function-on-values [func in-map]
  (reduce-kv #(assoc %1 %2 (func %3)) {} in-map))

(defn- group-by-and-apply [data f g]
  (->> data
       (group-by f)
       (apply-function-on-values g)
       ))

(defmulti transform-spec "Transforms the raw edn of specs to a better suited format."
  :type)

(defmethod transform-spec :default [term]
  (case (:type term)
    :any (r/->AnySpec)
    :ground (r/->GroundSpec)
    :var (r/->VarSpec)
    :string (r/->StringSpec)
    :nonvar (r/->NonvarSpec)
    :number (r/->NumberSpec)
    :float (r/->FloatSpec)
    :integer (r/->IntegerSpec)
    :atom (r/->AtomSpec)
    :atomic (r/->AtomicSpec)
    :int (r/->IntegerSpec)
    :emptylist (r/->EmptyListSpec)
    (r/->UserDefinedSpec (name (:type term)))))


(defmethod transform-spec :list [{list-type :list-type}]
  (r/->ListSpec (transform-spec list-type)))

(defmethod transform-spec :compound [{functor :functor arglist :arglist}]
  (r/->CompoundSpec functor (map transform-spec arglist)))

(defmethod transform-spec :one-of [{arglist :arglist}]
  (r/->OneOfSpec (set (map transform-spec arglist))))

(defmethod transform-spec :and [{arglist :arglist}]
  (r/->AndSpec (set (map transform-spec arglist))))

(defmethod transform-spec :tuple [{arglist :arglist}]
  (r/->TupleSpec (map transform-spec arglist)))

(defmethod transform-spec :same [{term :term}]
  (r/->ExactSpec term))

(defmethod transform-spec :specvar [{n :name}]
  (r/->SpecvarSpec n))

(defmethod transform-spec :union [{n :name}]
  (r/->UnionSpec n))

(defmethod transform-spec :compatible [{n :name}]
  (r/->CompatibleSpec n))


(defmethod transform-spec :userdef [{n :name arglist :arglist}]
  (assoc (r/->UserDefinedSpec n) :arglist (map transform-spec arglist)))


(defn- validation-spec? [{[_ & args] :arglist}]
  (= 1 (count args)))

(defmulti specs-to-map :goal)

(defmethod specs-to-map :default [{module :module functor :functor arity :arity arglist :arglist}]
  {[module functor arity] (vector (map transform-spec arglist))})

(defmethod specs-to-map :spec-post [{module :module functor :functor arity :arity prem :premisse conc :conclusion}]
  {[module functor arity] (vector (vector (map transform-spec prem) (map transform-spec conc)))})


(defn- order-specs [specs]
  (->> specs
       (map specs-to-map)
       (apply merge-with into)
       (reduce-kv (fn [m keys v] (update m keys #(into % v))) {})
       ))


(defn- simple-simplify-or [spec]
  (if (= 1 (count (.arglist spec)))
    (first (.arglist spec))
    spec))

(defn- create-post-spec-map [post-specs]
  (->> post-specs
       (group-by first)
       (reduce-kv (fn [m k v] (assoc m k (->> v
                                             (map second)
                                             (map (partial apply r/to-tuple-spec))
                                             set
                                             r/->OneOfSpec
                                             simple-simplify-or
                                             ))) {})))

(defn order-post-specs [specs]
  (->> specs
       (map specs-to-map)
       (apply merge-with into)
       (reduce-kv (fn [m keys v] (update m keys #(into % v))) {})
       (reduce-kv (fn [m k v]
                    (assoc m k (create-post-spec-map v)))
                  {})
       ))

(defn- order-define-specs [define-specs]
  (reduce (fn [m {alias :alias def :definition}] (assoc m (transform-spec alias) (transform-spec def))) {} define-specs))

(defn- clean-up-spec-definitions [central-map]
  (let [specs (:define-spec central-map)]
    (-> central-map
        (dissoc :define-spec)
        (dissoc :declare-spec)
        (assoc :specs specs))))

(defn order-preds [preds]
  (->> preds
       (group-by (juxt :module :name :arity))
       (reduce-kv (fn [m k v] (assoc m k (->> v
                                             (map #(-> % (dissoc :module) (dissoc :name) (dissoc :arity)))
                                             (interleave (range))
                                             (apply hash-map)))) {})))

(defn order-imports [{module-mapping :module imports :use-module :as data}]
  (let [non-libs-all (->> imports
                          (remove :lib)
                          (filter #(= :all (:preds %)))
                          (map #(assoc % :module (get module-mapping (:path %))))
                          (map #(select-keys % [:module :in]))
                          (reduce #(assoc-in %1 [(:in %2) (:module %2)] :all) {}))
        non-libs (->> imports
                      (remove :lib)
                      (remove #(= :all (:preds %)))
                      (map #(assoc % :module (get module-mapping (:path %))))
                      (map #(select-keys % [:module :preds :in]))
                      (reduce #(assoc-in %1 [(:in %2) (:module %2)] (->> %2
                                                                         :preds
                                                                         (map first)
                                                                         set)) {}))
        libs (->> imports
                  (filter :lib)
                  (remove #(= :all (:preds %)))
                  (reduce #(assoc-in %1 [(:in %2) (:lib %2)] (->> %2
                                                                  :preds
                                                                  (map first)
                                                                  set)) {}))
        libs-all (->> imports
                      (filter :lib)
                      (filter #(= :all (:preds %)))
                      (reduce #(assoc-in %1 [(:in %2) (:lib %2)] :all) {}))]
    (-> data
        (dissoc :use-module)
        (assoc :imports (merge-with into non-libs-all non-libs libs-all libs)))))

(defn- format-and-clean-up [data]
  (println (pr-str "Start formatting of edn"))
  (-> data
      (group-by-and-apply :type (partial map :content))
      (update :define-spec order-define-specs)
      clean-up-spec-definitions
      (update :pre-spec order-specs)
      (update :post-spec order-post-specs)
      (update :inv-spec order-specs)
      (update :pred order-preds)
      (update :module (partial apply merge))
      order-imports
      (rename-keys {:pre-spec :pre-specs :post-spec :post-specs :inv-spec :inv-specs :pred :preds})
      ))

(defn add-built-ins [data]
  (println (pr-str "Add built-ins"))
  (let [built-in-edn (get-edn-file-name "prolog/builtins.pl")]
    (when (.exists (io/file built-in-edn))
      (io/delete-file built-in-edn))
    (call-prolog "swipl" "prolog/prolog_analyzer.pl" "swipl" "prolog/builtins.pl" built-in-edn)
    (let [built-in (-> built-in-edn
                       transform-to-edn
                       format-and-clean-up
                       (dissoc :error-msg))]
      (pre-processor/pre-process-single (merge-with into data built-in))
      )))

(defn process-edn
  ([edn] (process-edn "swipl" edn))
  ([dialect edn]
   (->> edn
        transform-to-edn
        format-and-clean-up
        add-built-ins
        )))

(defn process-prolog-file [dialect term-expander prolog-exe file-name]
  (let [edn-file (get-edn-file-name file-name)]
    (when (.exists (io/file edn-file))
      (io/delete-file edn-file))
    (call-prolog dialect term-expander prolog-exe file-name edn-file)
    (process-edn dialect edn-file)))

(defn process-prolog-directory [dialect term-expander prolog-exe dir-name]
  (let [edn-file (get-edn-file-name dir-name)]
    (when (.exists (io/file edn-file))
      (io/delete-file edn-file))
    (let [prolog-files (->> dir-name
                            io/file
                            (tree-seq #(.isDirectory %) #(.listFiles %))
                            (remove #(.isDirectory %))
                            (map #(.getPath %))
                            (map str)
                            (filter #(.endsWith % ".pl"))

                            )]
      (doseq [pl prolog-files]
        (call-prolog dialect term-expander prolog-exe pl edn-file))
      (process-edn dialect edn-file))))
