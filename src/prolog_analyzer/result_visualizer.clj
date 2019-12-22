(ns prolog-analyzer.result-visualizer
  (:require [clojure.java.io :as io :refer [make-parents writer]]
            [hiccup.core :as hiccup]
            [prolog-analyzer.record-utils :as ru]
            [prolog-analyzer.records :as r]
            [prolog-analyzer.utils :as utils]
            [ubergraph.core :as uber]))

(def POST_SPECS "doc/post-specs")
(def PRE_SPECS "doc/pre-specs")
(def HTML "doc/html")
(def ERRORS "doc/errors")
(def TYPES "doc/types")

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

(defn- delete-directory-recursive
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

(defn- valid-module [module]
  (and (not= module "user")
       (not= module "lists")))

(defn pr-str-typing [{id :id type :type}]
  (str id ":" (r/to-prolog type)))


(defn pr-str-guard [guard]
  (clojure.string/join "," (map pr-str-typing (sort-by :id guard))))

(defn pr-str-conclusion [conclusion]
  (str "[" (clojure.string/join ", " (map pr-str-typing (sort-by :id conclusion))) "]"))

(defn pr-str-conclusions [conclusions]
  (clojure.string/join ", " (map pr-str-conclusion conclusions)))


(defn- pr-str-post-spec [[module name arity] {guard :guard conc :conclusion}]
  (str
   ":- spec_post(" module ":" name "/" arity ", ["
   (pr-str-guard guard)
   "], ["
   (pr-str-conclusions conc)
   "])."))


(defn- print-post-specs [counter data]
  (let [file (io/file (str POST_SPECS "/step-" counter ".pl"))
        append #(spit file % :append true)]
    (make-parents file)
    (spit file ":- module(postspecs, []).\n\nspec_post(_,_,_).\n")
    (doseq [[module post-specs] (group-by (fn [[[m _ _] _]] m) (:post-specs data))]
      (when (valid-module module)
        (append "\n\n%% ")
        (append module)
        (doseq [[[module & _ :as pred-id] v] post-specs]
          (doseq [x v]
            (append "\n")
            (append (pr-str-post-spec pred-id x)))
          (append "\n"))))))


(defn- pr-str-pre-spec [[module name arity] v]
  (str ":- spec_pre(" module ":" name "/" arity ", [" (clojure.string/join ", " (map r/to-prolog (seq v))) "])."))

(defn- print-pre-specs [counter data]
  (let [file (io/file (str PRE_SPECS "/step-" counter ".pl"))
        append #(spit file % :append true)]
    (make-parents file)
    (spit file ":- module(prespecs, []).\n\nspec_pre(_,_).\n")
    (doseq [[module pre-specs] (group-by (fn [[[m _ _] _]] m) (:pre-specs data))]
      (when (valid-module module)
        (append "\n\n%% ")
        (append module)
        (doseq [[[module & _ :as pred-id] v] pre-specs]
          (doseq [x v]
            (append "\n")
            (append (pr-str-pre-spec pred-id x)))
          (append "\n"))))))



(defn print-intermediate-result [counter data]
  (when (zero? counter)
    (delete-directory-recursive (io/file POST_SPECS))
    (delete-directory-recursive (io/file PRE_SPECS)))
  (print-pre-specs counter data)
  (print-post-specs counter data))

(defn pr-str-errors-of-pred [error-map]
  (reduce-kv
   (fn [m term {reason :reason location :location}]
     (assoc m (r/to-string term) (str (r/to-string reason) " - found in " location)))
   {}
   error-map))

(defn- pr-str-errors [data file]
  (let [result-map (reduce-kv (fn [m pred-id error-map] (assoc m pred-id (pr-str-errors-of-pred error-map))) {} (:errors data))]
    (clojure.pprint/pprint result-map (clojure.java.io/writer file))))

(defn print-errors [counter data]
  (if (zero? counter)
    (delete-directory-recursive (io/file ERRORS))
    (let [file (io/file (str ERRORS "/errors_" counter ".edn"))]
      (make-parents file)
      (pr-str-errors data file))))


;; TYPING
(defn- any-ratio [{any :any not-any :not-any total :total :as p}]
  (if (or (nil? total) (zero? total))
    (-> p
        (assoc :any-ratio 0.0)
        (assoc :not-any-ratio 1.0))
    (-> p
        (assoc :any-ratio (->> (/ any total) double))
        (assoc :not-any-ratio (->> (/ not-any total) double)))))

(defn- get-vars
  ([env]
   (get-vars false env))
  ([only-header? env]
   (loop [terms (vec (if only-header? (utils/get-arguments env) (utils/get-terms env)))
          vars (list)]
     (if-let [f (first terms)]
       (cond
         (ru/var-term? f) (recur (vec (rest terms)) (conj vars f))
         (contains? f :arglist) (recur (apply conj (vec (rest terms)) (:arglist f)) vars)
         (contains? f :head) (recur (conj (vec (rest terms)) (:head f) (:tail f)) vars)
         :else (recur (vec (rest terms)) vars))
       (set vars)))))

(defn- get-vars-in-head [env]
  (get-vars true env))

(defn get-non-anon-vars
  ([env] (get-non-anon-vars false env))
  ([only-header? env]
   (->> env
        (get-vars only-header?)
        (remove #(.startsWith (:name %) "_"))
        set)))

(defn- get-non-anon-vars-in-head [env]
  (get-non-anon-vars true env))

(defn- freq-types [terms env]
  (->> terms
       (map (partial utils/get-dom-of-term env))
       (map ru/any-spec?)
       frequencies))

(defn- typing-of-a-clause [env]
  (let [fun (fn [x]
              (-> x
                  (freq-types env)
                  (clojure.set/rename-keys {false :not-any true :any})
                  (update :any #(or % 0))
                  (update :not-any #(or % 0))
                  (assoc :total (count x))
                  any-ratio
                  (assoc :faulty? (utils/faulty-env? env))
                  ))]
    {:only-head
     {:all (fun (get-vars-in-head env)) :non-anon (fun (get-non-anon-vars-in-head env))}
     :all
     {:all (fun (get-vars env)) :non-anon (fun (get-non-anon-vars env))}}
    ))


(defn- typing [envs]
  (->> envs
       (map #(hash-map (utils/get-title %) (typing-of-a-clause %)))
       (apply merge)))


(defn print-type-information [counter envs]
  (when (= 1 counter)
    (delete-directory-recursive (io/file TYPES)))
  (let [file (io/file (str TYPES "/types_" counter ".edn"))
        result {:number-of-clauses (count envs)
                :distributions (typing envs)}]
    (make-parents file)
    (clojure.pprint/pprint result (clojure.java.io/writer file))))
