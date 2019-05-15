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
