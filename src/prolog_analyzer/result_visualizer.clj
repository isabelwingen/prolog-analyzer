(ns prolog-analyzer.result-visualizer
  (:require [clojure.java.io :as io :refer [make-parents writer]]
            [hiccup.core :as hiccup]
            [prolog-analyzer.record-utils :as ru]
            [prolog-analyzer.records :as r]
            [prolog-analyzer.utils :as utils]))

(def POST_SPECS "doc/post-specs")
(def PRE_SPECS "doc/pre-specs")
(def HTML "doc/html")
(def ERRORS "doc/errors")

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
  (let [index (str HTML "/index.html")
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
    (delete-directory-recursive (io/file HTML))
    (make-parents index)
    (spit index content)))

(defn- subpage [module data]
  (let [file (str HTML "/" (filename module))
        content (htmlify-module module data)]
    (spit file content)))

(defn htmlify-data [data]
  (index-page data)
  (doseq [m (concat (get-all-modules data) (get-program-modules data))]
    (subpage m data)))

(defn pr-str-pre-spec [v]
  (vec (map r/to-string (seq v))))

(defn pr-str-guard [{id :id type :type}]
  (str "$" id ":" (r/to-string type)))

(defn pr-str-guards [guards]
  (clojure.string/join ", " (map pr-str-guard guards)))

(defn pr-str-conclusion [v]
  (vec (map pr-str-guard v)))

(defn fill [length string]
  (format (str "%1$" length "s") string))


(defn pr-str-conclusions [v]
  (let [strs (map pr-str-conclusion v)
        lengths (->> strs
                     (apply map vector)
                     (map (partial map count))
                     (map (partial apply max))
                     (map-indexed (fn [i v] #(update % i (partial fill v)))))
        p (reduce (fn [x f] (map f x)) strs lengths)]
    (vec (map (partial clojure.string/join ", ") p))))



(defn pr-str-post-spec [{guard :guard conc :conclusion}]
  (if (empty? guard)
    {:guard "true" :conclusion (pr-str-conclusions conc)}
    {:guard (pr-str-guards guard) :conclusion (pr-str-conclusions conc)}))

(defn valid-module [module]
  (and (not= module "user")
       (not= module "lists")))

(defn print-pre-specs [counter data]
  (let [file (io/file (str PRE_SPECS "/step-" counter ".txt"))
        append #(spit file % :append true)]
    (make-parents file)
    (spit file "Pre Specs")
    (doseq [[[module & _ :as k] v] (:pre-specs data)]
      (when (valid-module module)
        (append "\n\n")
        (append k)
        (doseq [x v]
          (append "\n\t")
          (append (with-out-str (clojure.pprint/pprint (pr-str-pre-spec x)))))))))


(defn as-table [postspecs]
  (let [length (apply max (map (comp count :guard) postspecs))
        fill-str (fill (+ length 5) "")
        new-maps (map #(update % :guard (partial fill length)) postspecs)]
    (clojure.string/join
     "\n"
     (for [x new-maps
           :let [{guard :guard [c & cs] :conclusion} x]]
       (clojure.string/join
        "\n"
        (cons (str guard " --> " c) (map (partial str fill-str) cs)))))))

(defn print-post-specs [counter data]
  (let [file (io/file (str POST_SPECS "/step-" counter ".txt"))
        append #(spit file % :append true)]
    (make-parents file)
    (spit file "Post Specs")
    (doseq [[[module & _ :as k] v] (:post-specs data)]
      (when (valid-module module)
        (append "\n\n")
        (append k)
        (append "\n")
        (append (as-table (map pr-str-post-spec v)))
        #_(doseq [x v]
          (append "\n\t")
          (append (with-out-str (clojure.pprint/pprint (pr-str-post-spec x)))))))))


(defn print-intermediate-result [counter data]
  (when (zero? counter)
    (delete-directory-recursive (io/file POST_SPECS))
    (delete-directory-recursive (io/file PRE_SPECS)))
  (print-pre-specs counter data)
  (print-post-specs counter data))

(defn pr-str-errors [data file]
  (let [inner-map (partial reduce-kv #(assoc %1 (r/to-string %2) (r/to-string %3)) {})
        result-map (reduce-kv #(assoc %1 %2 (inner-map %3)) {} (:errors data))]
    (clojure.pprint/pprint result-map (clojure.java.io/writer file))))

(defn print-errors [counter data]
  (when (zero? counter)
    (delete-directory-recursive (io/file ERRORS)))
  (let [file (io/file (str ERRORS "/errors_" counter ".txt"))]
    (make-parents file)
    (pr-str-errors data file)))


(defn prettify-spec [spec]
  (cond
    (:arglist spec) (-> spec
                        (assoc :spec (r/spec-type spec))
                        (update :arglist (partial map prettify-spec)))
    (:type spec) (-> spec
                     (assoc :spec (r/spec-type spec))
                     (update :type prettify-spec))
    :else (assoc spec :spec (r/spec-type spec))))

(defn prettify-term [term]
  (cond
    (:head term) (-> term
                     (assoc :term (r/term-type term))
                     (update :head prettify-term)
                     (update :tail prettify-term))
    (:arglist term) (-> term
                        (assoc :term (r/term-type term))
                        (update :arglist (partial map prettify-term)))
    :else (assoc term :term (r/term-type term))))

(defn prettify-pre-spec [pre-spec]
  (map prettify-spec pre-spec))

(defn prettify-pre-specs [pre-specs]
  (map prettify-pre-spec pre-specs))

(defn prettify-guard [guard]
  (update guard :type prettify-spec))

(defn prettify-conclusion [conclusion]
  (map prettify-guard conclusion))

(defn prettify-post-spec [post-spec]
  (-> post-spec
      (update :guard (partial map prettify-guard))
      (update :conclusion (partial map prettify-conclusion))))


(defn prettify-goal [{name :goal :as goal}]
  (if (#{:or :if :not} name)
    (update goal :arglist (partial map (partial map prettify-goal)))
    (update goal :arglist (partial map prettify-term))))

(defn prettify-clause [clause]
  (-> clause
      (update :arglist (partial map prettify-term))
      (update :body (partial map prettify-goal))))

(defn prettify-data [data]
  )
