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

(defmulti ^{:private true} call-prolog (fn [dir? properties file edn-file] (:dialect properties)))


(defn prolog-goal [path-to-analyzer edn-file file dir?]
  (let [dir (if dir? "is_dir," "")]
    (str "use_module('" path-to-analyzer "', [set_file/1, is_dir/0, close_orphaned_stream/0]),"
         "set_file('" edn-file "'),"
         dir
         "['" file "'],"
         "close_orphaned_stream,"
         "halt."))
  )

(defmethod call-prolog "swipl" [dir? {expander :expander exe :exe} file edn-file]
  (log/info "Call prolog on" file)
  (let [goal (prolog-goal expander edn-file file dir?)
        {err :err} (sh/sh exe "-g" goal :env (into {} (System/getenv)))]
    err))


(defmethod call-prolog "sicstus" [dir? {expander :expander exe :exe} file edn-file]
  (log/info "Call prolog on" file)
  (let [goal (prolog-goal expander edn-file file dir?)
        {err :err} (time (sh/sh exe "--goal" goal "--noinfo" :env (into {} (System/getenv))))]
    err))

(defn write-and-return-result [edn-file result]
  (spit (io/file (str edn-file ".transformed")) (with-out-str (pprint result)))
  result)


(defn process-built-ins [{file-name :built-ins :as properties}]
  (let [edn-file (get-edn-file-name (io/file file-name))]
    (log/info (str "EDN Path " edn-file))
    (when (not (.exists (io/file edn-file)))
      (call-prolog false properties file-name edn-file))
    (->> edn-file
         read-in-data
         pre1/format-and-clean-up
         (write-and-return-result edn-file))))

(defn process-edn
  [edn-file built-ins]
  (let [data (->> edn-file
                  read-in-data
                  pre1/format-and-clean-up)]
    (->> built-ins
         (merge-with into data)
         pre2/pre-process-single
         (write-and-return-result edn-file))))

(defn process-prolog-file [properties file-name]
  (let [built-ins (process-built-ins properties)
        edn-file (get-edn-file-name file-name)]
    (when (.exists (io/file edn-file))
      (io/delete-file edn-file))
    (if-let [err (call-prolog false properties file-name edn-file)]
      (log/warn err)
      nil)
    (process-edn edn-file built-ins)))

(defn process-prolog-directory [properties dir-name]
  (let [built-ins (process-built-ins properties)
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
        (call-prolog true properties pl edn-file))
      (process-edn edn-file built-ins))))
