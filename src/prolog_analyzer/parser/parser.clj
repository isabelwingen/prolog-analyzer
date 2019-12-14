(ns prolog-analyzer.parser.parser
  (:require [clojure.java.io :as io]
            [clojure.java.shell :as sh]
            [clojure.pprint :refer [pprint]]
            [clojure.set :refer [rename-keys]]
            [clojure.string :as string]
            [clojure.tools.logging :as log]
            [prolog-analyzer.parser.pre-processor-1 :as pre1]
            [prolog-analyzer.parser.pre-processor-2 :as pre2]
            [prolog-analyzer.records :as r]
            [prolog-analyzer.parser.transform-spec :refer [transform-spec]]))

(defn- get-name-without-ending [file-name]
  (first (string/split (.getName (io/file file-name)) #"\.")))

(defn- get-edn-file-name [file-name]
  (let [dir "edns/"
        edn-file (str dir (get-name-without-ending file-name) ".edn")]
    (when-not (.exists (io/file dir))
      (.mkdirs (io/file dir)))
    (.getAbsolutePath (io/file edn-file))))


(defn- split-up-error-message [msg]
  (->> msg
       string/split-lines
       (partition 2)
       (map (partial apply str))
       (apply vector)))

(defn read-in-edn-file [path]
  (let [in (read-string (str \[ (slurp path) \]))
        file (io/file path)]
    (future
      (when (.exists file)
        (io/delete-file file))
      (doseq [x (map #(with-out-str (pprint %)) in)]
        (spit file x :append true)
        (spit file "\n" :append true)))
    in))

(defn read-in-data [clojure-file]
  (if (.exists (io/file clojure-file))
    (read-in-edn-file clojure-file)
    (do
      (log/error "No .edn file was created")
      [])))

(defmulti ^{:private true} call-prolog (fn [dialect term-expander prolog-exe file edn-file] dialect))

(defmethod call-prolog "swipl" [dialect term-expander prolog-exe file edn-file]
  (log/info "Call prolog on" file)
  (let [path-to-analyzer (str "'" term-expander "'")
        goal (str "use_module(" path-to-analyzer ", [set_file/1, close_orphaned_stream/0]),"
                  "set_file('" edn-file "'),"
                  "['" file "'],"
                  "close_orphaned_stream,"
                  "halt.")
        {err :err} (sh/sh prolog-exe "-g" goal :env (into {} (System/getenv)))]
    err))


(defmethod call-prolog "sicstus" [dialect term-expander prolog-exe file edn-file]
  (log/info "Call prolog on" file)
  (let [path-to-analyzer (str "'" term-expander "'")
        goal (str "use_module(" path-to-analyzer ", [set_file/1, close_orphaned_stream/0]),"
                  "set_file('" edn-file "'),"
                  "['" file "'],"
                  "close_orphaned_stream,"
                  "halt.")
        {err :err} (time (sh/sh prolog-exe "--goal" goal "--noinfo" :env (into {} (System/getenv))))]
    err))

(defn write-and-return-result [edn-file result]
  (spit (io/file (str edn-file ".transformed")) (with-out-str (pprint result)))
  result)


(defn process-built-ins [dialect term-expander prolog-exe]
  (let [file-name "prolog/builtins.pl"
        edn-file (get-edn-file-name file-name)]
    (when (not (.exists (io/file edn-file)))
      (call-prolog dialect term-expander prolog-exe file-name edn-file))
    (->> edn-file
         read-in-data
         pre1/format-and-clean-up
         (write-and-return-result edn-file))))

(defn process-edn
  ([edn-file]
   (let [built-ins (process-built-ins "swipl" "prolog/prolog-analyzer.pl" "swipl")]
     (process-edn edn-file built-ins)))
  ([edn-file built-ins]
   (let [data (->> edn-file
                   read-in-data
                   pre1/format-and-clean-up)]
     (->> built-ins
          (merge-with into data)
          pre2/pre-process-single
          (write-and-return-result edn-file)))))

(defn process-prolog-file [dialect term-expander prolog-exe file-name]
  (let [built-ins (process-built-ins dialect term-expander prolog-exe)
        edn-file (get-edn-file-name file-name)]
    (when (.exists (io/file edn-file))
      (io/delete-file edn-file))
    (if-let [err (call-prolog dialect term-expander prolog-exe file-name edn-file)]
      (log/warn err)
      nil)
    (process-edn edn-file built-ins)))

(defn process-prolog-directory [dialect term-expander prolog-exe dir-name]
  (let [built-ins (process-built-ins dialect term-expander prolog-exe)
        edn-file (get-edn-file-name dir-name)]
    (when (.exists (io/file edn-file))
      (io/delete-file edn-file))
    (let [prolog-files (->> dir-name
                            io/file
                            (tree-seq #(.isDirectory %) #(.listFiles %))
                            (remove #(.isDirectory %))
                            (map #(.getPath %))
                            (map str)
                            (filter #(.endsWith % ".pl")))]
      (doseq [pl prolog-files]
        (call-prolog dialect term-expander prolog-exe pl edn-file))
      (process-edn edn-file built-ins))))
