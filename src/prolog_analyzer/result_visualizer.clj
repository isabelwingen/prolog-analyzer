(ns prolog-analyzer.result-visualizer
  (:require [clojure.data.json :as json]
            [prolog-analyzer.records :as r]
            [prolog-analyzer.record-utils :as ru]
            [prolog-analyzer.utils :as utils]
            [clojure.java.io :refer [writer make-parents]]
            [hiccup.core :as hiccup]
            [ubergraph.core :as uber]
            [clojure.java.io :as io]))

(defn- get-error-terms [env]
  (->> env
       utils/get-terms
       (filter #(ru/error-spec? (utils/get-dom-of-term env %)))))

(defn- html-file-name [[module pred arity]]
  (str module ":" pred "(" arity ")" ".html"))

(defn- link-text [[module pred arity]]
  (str module ":" pred "/" arity))

(defn- link [pred-id]
  (hiccup/html [:a {:href (html-file-name pred-id)} (link-text pred-id)]))

(defn- htmlify-pre-spec [pre-spec]
  (hiccup/html [:p (str "[" (clojure.string/join ", " (map r/to-string pre-spec)) "]")]))

(defn- str-guard [{id :id t :type}]
  (str "$" id ":" (r/to-string t)))

(defn- str-guards [guards]
  (if (empty? guards)
    "true"
    (clojure.string/join ", " (map str-guard guards))))

(defn- str-conc [conc]
  (str "[" (str-guards conc) "]"))

(defn- str-concs [concs]
  (clojure.string/join "; " (map str-conc concs)))

(defn- htmlify-post-spec [{guard :guard conclusion :conclusion}]
  (hiccup/html [:p (str (str-guards guard) "  &#8594;  " (str-concs conclusion))]))


(defn- htmlify-post-specs [pred-id data]
  (->> data
       (utils/get-post-specs pred-id)
       (map htmlify-post-spec)
       (apply vector [:h3 "Post-Specs"])))

(defn- htmlify-pre-specs [pred-id data]
  (->> data
       (utils/get-pre-specs pred-id)
       (map htmlify-pre-spec)
       (apply vector [:h3 "Pre-Specs"])))

(defn- htmlify-pred [pred-id data]
  (concat
   [[:h2 (link-text pred-id)]]
   (htmlify-pre-specs pred-id data)
   (htmlify-post-specs pred-id data)
   [[:hr]]))

(defn- filename [s]
  (str s ".html"))

(defn- append-back-link [l]
  (concat l [[:a {:href "index.html"} "Back to main page"]]))

(defn- all-pred-identites [data]
  (let [a (->> data
               :pre-specs
               keys)]
    (set (concat (utils/get-pred-identities data) a))))

(defn- htmlify-module [module data]
  (let [pred-ids (->> data
                     all-pred-identites
                     (filter (fn [[m _ _]] (= m module))))]
    (->> pred-ids
         (mapcat #(htmlify-pred % data))
         append-back-link
         (apply vector :body [:h1 module] [:hr])
         (vector :html)
         hiccup/html)))

(defn- get-all-modules [data]
  (->> data
       all-pred-identites
       (map first)
       distinct
       (remove (set (map first (utils/get-pred-identities data))))))

(defn- get-program-modules [data]
  (->> data
       utils/get-pred-identities
       (map first)
       distinct))

(defn delete-directory-recursive
  "Recursively delete a directory.
  https://gist.github.com/olieidel/c551a911a4798312e4ef42a584677397
  "
  [^java.io.File file]
  ;; when `file` is a directory, list its entries and call this
  ;; function with each entry. can't `recur` here as it's not a tail
  ;; position, sadly. could cause a stack overflow for many entries?
  (when (.exists file)
    (when (.isDirectory file)
      (doseq [file-in-dir (.listFiles file)]
        (delete-directory-recursive file-in-dir)))
    ;; delete the file or directory. if it it's a file, it's easily
    ;; deletable. if it's a directory, we already have deleted all its
    ;; contents with the code above (remember?)
    (io/delete-file file)))

(defn- index-page [data]
  (let [index "doc/html/index.html"
        built-ins (->> data
                       get-all-modules
                       (map #(vector :a {:href (filename %)} %))
                       (map (partial vector :p)))
        program (->> data
                     get-program-modules
                     (map #(vector :a {:href (filename %)} %))
                     (map (partial vector :p)))
        content (->> (concat program [[:hr]] built-ins)
                     (apply vector :body)
                     (vector :html)
                     hiccup/html)]
    (delete-directory-recursive (io/file "doc/html"))
    (make-parents index)
    (spit index content)))

(defn- subpage [module data]
  (let [file (str "doc/html/" (filename module))
        content (htmlify-module module data)]
    (spit file content)))

(defn htmlify-data [data]
  (index-page data)
  (doseq [m (concat (get-all-modules data) (get-program-modules data))]
    (subpage m data)))

(defn pr-str-pre-spec [v]
  (str "[" (->> v
                seq
                (map pr-str)
                (clojure.string/join ", ")) "]"))

(defn pr-str-guard [{id :id type :type}]
  (str "$" id ": " (pr-str type)))

(defn pr-str-guards [guards]
  (->> guards
       (map pr-str-guard)
       (clojure.string/join ", ")))

(defn pr-str-conclusion [v]
  (->> v
       (map pr-str-guard)
       (clojure.string/join "; ")))

(defn pr-str-conclusions [v]
  (->> v
       (map pr-str-conclusion)
       (clojure.string/join "\n")))

(defn pr-str-post-spec [{guard :guard conc :conclusion}]
  (if (empty? guard)
    (str "true --> [" (pr-str-conclusions conc) "]")
    (str "[" (pr-str-guards guard) "] --> [" (pr-str-conclusions conc) "]")))

(defn print-pre-specs [counter data]
  (let [file (io/file (str "doc/pre-specs/step-" counter ".txt"))
        append #(spit file % :append true)]
    (make-parents file)
    (spit file "Pre Specs")
    (doseq [[k v] (:pre-specs data)]
      (append "\n")
      (append k)
      (doseq [x v]
        (append "\n\t")
        (append (pr-str-pre-spec x))))))

(defn print-post-specs [counter data]
  (let [file (io/file (str "doc/post-specs/step-" counter ".txt"))
        append #(spit file % :append true)]
    (make-parents file)
    (spit file "Post Specs")
    (doseq [[k v] (:post-specs data)]
      (append "\n")
      (append k)
      (doseq [x v]
        (append "\n\t")
        (append (pr-str-post-spec x))))))


(defn print-intermediate-result [counter data]
  (when (zero? counter)
    (delete-directory-recursive (io/file "doc/post-specs"))
    (delete-directory-recursive (io/file "doc/pre-specs")))
  (print-pre-specs counter data)
  (print-post-specs counter data))
