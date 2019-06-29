(ns prolog-analyzer.analyzer.pretty-printer
  (:require [prolog-analyzer.utils :as utils]
            [prolog-analyzer.records :as r]
            [ubergraph.core :as uber]
            [loom.graph]
            [loom.attr]
            [clojure.pprint :refer [pprint print-table]]
            [clojure.string]
            [clojure.walk]))


(defn create-map [graph node]
  (let [dom (utils/get-dom-of-term graph node (r/->AnySpec))
        id (utils/get-pred-id graph)]
    (assert (satisfies? prolog-analyzer.records/printable dom) (str "dom " id))
    (assert (satisfies? prolog-analyzer.records/printable node) (str "node " (apply vector node) " " id))
    (hash-map :term (r/to-string node) :dom (r/to-string dom))))

(defn print-nodes [graph]
  (let [nodes (utils/get-terms graph)]
    (->> nodes
         (map #(create-map graph %))
         (print-table [:term :dom]))))

(defn print-edges [graph]
  (let [edges (uber/edges graph)
        srcs (map uber/src edges)
        dests (map uber/dest edges)
        max-src-length (->> srcs
                            (map r/to-string)
                            (map count)
                            (apply max)
                            (+ 2))
        max-dest-length (->> dests
                             (map r/to-string)
                             (map count)
                             (apply max)
                             (+ 5))]
    (->> edges
        (map #(hash-map
               :src (r/to-string (uber/src %))
               :dest (r/to-string (uber/dest %))
               :attrs (str (uber/attrs graph %))))
        print-table)))

(defn- arti-term? [term]
  (or (.startsWith (str (:name term)) "A~~")
      (.startsWith (str (:name term)) "T~~")
      (.startsWith (str (:name term)) "ID~~")))

(defn- contains-arti-term? [term]
  (or (arti-term? term)
      (and (arti-term? (:head term)) (arti-term? (:tail term)))
      (and ((complement nil?) (:arglist term)) (some #(arti-term? %) (:arglist term)))))

(defn pretty-print-graph [graph]
  (println (pr-str (utils/get-pred-id graph) (utils/get-clause-number graph)))
  (let [nodes (utils/get-terms graph)
        nd-num (count nodes)
        edges (uber/edges graph)
        edg-num (count edges)]
    (println nd-num "Nodes:")
    (print-nodes graph)
    (println edg-num "Edges:")
    (when (> edg-num 0)
      (print-edges graph))
    (println "---------------------\n")))



(defn- tinker-error-message [env term]
  (str ":( There was an ERROR in term "
       (r/to-string term)
       "!\n"
       "This is the error message:\n"
       (r/to-string (utils/get-dom-of-term env term (r/->AnySpec)))
       "\n"
       (uber/attr env term :indices)
       ))

(defn derecordize
  "Returns a data structure equal (using clojure.core/=) to the
  original value with all records converted to plain maps."
  [v]
  (cond
    (record? v) (derecordize (into {} v))
    (list? v) (map derecordize v)
    (vector? v) (mapv derecordize v)
    (set? v) (set (map derecordize v))
    (map? v) (zipmap (map derecordize (keys v)) (map derecordize (vals v)))
    :else v))

(derecordize {:post-specs {["bla"] {[(r/->AnySpec)] {:arglist [(r/->AnySpec)]}}}})

(defn record-type [t]
  (cond
    (satisfies? prolog-analyzer.records/spec t) (assoc t :record-type (r/spec-type t))
    (satisfies? prolog-analyzer.records/term t) (assoc t :record-type (r/term-type t))
    :else t))

(defn transform-record-to-map [spec]
  (clojure.walk/postwalk #(if (record? %) (into {} (record-type %)) %) spec))

(defn print-with-indices
  "Pretty Printer, that prints all terms used in the header and in the subgoals.
  Prints the domain of the terms."
  [graph]
  (println (pr-str (utils/get-pred-id graph) (utils/get-clause-number graph)))
  (->> graph
       (utils/get-terms)
       (remove contains-arti-term?)
       (remove #(nil? (utils/get-dom-of-term graph % nil)))
       (remove #(= "[]" (r/to-string %)))
       (filter #(or (uber/attr graph % :index)
                    (uber/attr graph % :indices)))
       (map #(hash-map
              :term (r/to-string %)
              :index (str (uber/attr graph % :index))
              :indices (str (uber/attr graph % :indices))
              :dom (r/to-string (utils/get-dom-of-term graph % (r/->AnySpec)))))
       (print-table [:term :dom :index :indices]))
  (println))


(defn print-basics
  "Debug printer, that just prints the predicate id"
  [graph]
  (println (pr-str (utils/get-pred-id graph) (utils/get-clause-number graph)))

  )


(defn print-type-information
  "Pretty printer, which prints types and errors."
  [graph]
  (println (pr-str (clojure.string/join " " (map str (conj (utils/get-pred-id graph) (utils/get-clause-number graph))))))
  (let [error-terms (->> graph
                         (utils/get-terms)
                         (remove contains-arti-term?)
                         (remove #(nil? (utils/get-dom-of-term graph % nil)))
                         (filter #(r/error-spec? (utils/get-dom-of-term graph % (r/->AnySpec)))))
        other-terms (->> graph
                         (utils/get-terms)
                         (remove contains-arti-term?)
                         (remove #(nil? (uber/attr graph % :index))))]
    (->> other-terms
         (map #(hash-map
                :term (r/to-string %)
                :dom (r/to-string (utils/get-dom-of-term graph % (r/->AnySpec)))))
         (print-table [:term :dom]))
    (println)
    (doseq [t error-terms]
      (println (tinker-error-message graph t) "\n"))))


(defn pretty-print-errors
  "Pretty Printer, which only prints errors"
  [graph]
  (let [error-terms (->> graph
                         utils/get-terms
                         (filter #(or (uber/attr graph % :index)
                                      (uber/attr graph % :indices)))
                         (filter #(r/error-spec? (utils/get-dom-of-term graph % (r/->AnySpec))))
                         (map #(assoc (uber/attrs graph %) :term (r/to-string %)))
                         (map #(update % :dom (fn [x] (or x (r/->AnySpec)))))
                         (map #(update % :dom r/to-string)))]
    (when (not-empty error-terms)
      (println (pr-str (utils/get-pred-id graph) (utils/get-clause-number graph)))
      (print-table [:term :index :indices :dom] error-terms))))

(defn print-type-analysis
  "Edn printer, which prints a map mapping every term of the clause to its domain"
  [graph]
  (->> graph
       (utils/get-terms)
       (filter #(or (uber/attr graph % :index)
                    (uber/attr graph % :indices)))
       (reduce #(assoc %1 (transform-record-to-map %2) (transform-record-to-map (utils/get-dom-of-term graph %2 (r/->AnySpec)))) {})
       pr-str
       println))


(defn print-domains-of-variables
  "Edn printer, which prints a map mapping every variable term to its domain"
  [env]
  (->> env
       utils/get-terms
       (filter #(satisfies? prolog-analyzer.records/term %))
       (filter #(= r/VAR (r/term-type %)))
       (remove arti-term?)
       (reduce #(assoc %1 (transform-record-to-map %2) (transform-record-to-map (utils/get-dom-of-term env %2 (r/->AnySpec)))) {})
       pr-str
       println
       ))

(defn print-types-and-errors
  "Edn printer, which prints a map mapping every variable term to its domain and map mapping every term, which contains an error, to its error message"
  [env]
  (println (pr-str (clojure.string/join " " (map str (conj (utils/get-pred-id env) (utils/get-clause-number env))))))
  (print-domains-of-variables env)
  (->> env
       utils/get-terms
       (filter #(r/error-spec? (utils/get-dom-of-term env % (r/->AnySpec))))
       (reduce #(assoc %1 (transform-record-to-map %2) (transform-record-to-map (utils/get-dom-of-term env %2 (r/->AnySpec)))) {})
       pr-str
       println))


(defn- has-compatible-edge [env node]
  (->> node
       (uber/out-edges env)
       (filter #(= :compatible (uber/attr env % :relation)))
       ((complement empty?))))


(defn print-types-and-errors-v2
  "Edn printer, which prints information about variables and errors"
  [env]
  (println (pr-str (clojure.string/join " " (map str (conj (utils/get-pred-id env) (utils/get-clause-number env))))))
  (->> env
       utils/get-terms
       (filter #(satisfies? prolog-analyzer.records/term %))
       (filter #(= r/VAR (r/term-type %)))
       (remove arti-term?)
       (map #(hash-map
              (transform-record-to-map %)
              (hash-map
               :dom (transform-record-to-map (utils/get-dom-of-term env % (r/->AnySpec)))
               :compatible-edge? (has-compatible-edge env %))))
       (apply merge-with into)
       pr-str
       println
       )
  (->> env
       utils/get-terms
       (filter #(r/error-spec? (utils/get-dom-of-term env % (r/->AnySpec))))
       (reduce #(assoc %1 (transform-record-to-map %2) (transform-record-to-map (utils/get-dom-of-term env %2 (r/->AnySpec)))) {})
       pr-str
       println)
  )

(defn print-types-and-errors-v3
  "Edn printer, which prints information about calls, variables and errors"
  [env]
  (println (pr-str (clojure.string/join " " (map str (conj (utils/get-pred-id env) (utils/get-clause-number env))))))
  (println (pr-str (select-keys (uber/attrs env :ENVIRONMENT) [:known :unknown])))
  (->> env
       utils/get-terms
       (filter #(satisfies? prolog-analyzer.records/term %))
       (filter #(= r/VAR (r/term-type %)))
       (remove arti-term?)
       (map #(hash-map
              (transform-record-to-map %)
              (hash-map
               :dom (transform-record-to-map (utils/get-dom-of-term env % (r/->AnySpec)))
               :compatible-edge? (has-compatible-edge env %))))
       (apply merge-with into)
       pr-str
       println
       )
  (->> env
       utils/get-terms
       (filter #(r/error-spec? (utils/get-dom-of-term env % (r/->AnySpec))))
       (reduce #(assoc %1 (transform-record-to-map %2) (transform-record-to-map (utils/get-dom-of-term env %2 (r/->AnySpec)))) {})
       pr-str
       println)
  )


(defn prologify [spec]
  (if (record? spec)
    (utils/case+ (r/spec-type spec)
                 r/ANY "any"
                 r/ATOM "atom"
                 r/INTEGER "int"
                 r/FLOAT "float"
                 r/NUMBER "number"
                 r/ATOMIC "atomic"
                 r/GROUND "ground"
                 r/NONVAR "nonvar"
                 r/VAR "var"
                 r/STRING "string"
                 r/EXACT (str "same(" (:value spec) ")")
                 r/EMPTYLIST (prologify (r/->AndSpec [(r/->ListSpec (r/->AnySpec)) (r/->AtomicSpec)]))
                 r/LIST (str "list(" (prologify (:type spec)) ")")
                 r/COMPOUND (if (nil? (:functor spec))
                              "compound"
                              (str "compound(" (:functor spec) "(" (clojure.string/join ", " (map prologify (:arglist spec))) "))"))
                 r/TUPLE (str "tuple(" (prologify (:arglist spec)) ")")
                 r/AND (str "and(" (prologify (:arglist spec)) ")")
                 r/OR (str "or(" (prologify (:arglist spec)) ")")
                 r/UNION (str "union(" (:name spec) ")")
                 r/COMPATIBLE (str "compatible(" (:name spec) ")")
                 r/USERDEFINED (if (nil? (:arglist spec))
                                 (:name spec)
                                 (str (:name spec) "(" (prologify (:arglist spec)) ")"))
                 (str "error" (r/spec-type spec)))
    (str "[" (clojure.string/join ", " (map prologify spec)) "]"))
  )

(defn simplify-premise [premise defs]
  (utils/case+ (r/spec-type premise)
               r/TUPLE (prologify (:arglist premise))
               r/AND (let [new (r/simplify-and premise defs false)]
                       (if (= r/AND (r/spec-type new))
                         new
                         (simplify-premise new defs)))
               r/OR (let [new (r/simplify-or premise defs)]
                      (if (= r/OR (r/spec-type new))
                        new
                        (simplify-premise new defs)))))


(defn create-post-specs [post-spec-map defs ]
  (clojure.string/join
   "\n"
   (for [[[module pred arity] v] post-spec-map
         [a b] v
         :let [head (str module ":" pred "/" arity)
               condition (prologify a)
               premise (simplify-premise b defs)]]
     (str ":- spec_post(" head ", " condition ", " premise ")."))))

(defn create-pre-specs [pre-spec-map]
  (clojure.string/join
   "\n"
   (for [[[module pred arity] v] pre-spec-map
         b v
         :let [head (str module ":" pred "/" arity)
               spec (prologify b)]]
     (str ":- spec_pre(" head ", " spec ")."))))
