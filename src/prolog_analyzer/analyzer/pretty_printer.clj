(ns prolog-analyzer.analyzer.pretty-printer
  (:require [prolog-analyzer.utils :as utils]
            [prolog-analyzer.records :as r]
            [ubergraph.core :as uber]
            [loom.graph]
            [loom.attr]
            [clojure.pprint :refer [pprint print-table]]
            [clojure.string]))


(defn print-nodes [graph]
  (let [nodes (utils/get-terms graph)]
    (->> nodes
         (remove #(= "[]" (r/to-string %)))
         (map #(hash-map
                :term (r/to-string %)
                :dom (r/to-string (utils/get-dom-of-term graph % (r/->AnySpec)))))
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
  (.startsWith (str (:name term)) "A__"))

(defn- contains-arti-term? [term]
  (or (arti-term? term)
      (and (arti-term? (:head term)) (arti-term? (:tail term)))
      (and ((complement nil?) (:arglist term)) (some #(arti-term? %) (:arglist term)))))

(defn pretty-print-graph [graph]
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
       (r/to-string (or (utils/get-dom-of-term env term) (r/->AnySpec)))
       "\n"
       (uber/attr env term :indices)
       ))

(defn better-print [graph]
  (let [error-terms (->> graph
                         (utils/get-terms)
                         (remove contains-arti-term?)
                         (remove #(nil? (utils/get-dom-of-term graph %)))
                         (filter #(r/error-spec? (utils/get-dom-of-term graph %))))]
    (if (empty? error-terms)
      (println ":) No errors found")
      (doseq [t error-terms]
        (println (tinker-error-message graph t))
        (println)))
    ))

(defn print-type-information [graph]
  (let [error-terms (->> graph
                        (utils/get-terms)
                        (remove contains-arti-term?)
                        (remove #(nil? (utils/get-dom-of-term graph %)))
                        (filter #(r/error-spec? (utils/get-dom-of-term graph %))))
        other-terms (->> graph
                         (utils/get-terms)
                         (remove contains-arti-term?)
                         (remove #(nil? (uber/attr graph % :index))))]
    (->> other-terms
         (map #(hash-map
                :term (r/to-string %)
                :dom (r/to-string (utils/get-dom-of-term graph % (r/->AnySpec)))))
         (print-table [:dom :term]))
    (println)
    (doseq [t error-terms]
      (println (tinker-error-message graph t) "\n"))))

(defn print-with-indices [graph]
  (println (pr-str (uber/attr graph :ENVIRONMENT :pred-id)))
  (->> graph
       (utils/get-terms)
       (remove contains-arti-term?)
       (remove #(nil? (utils/get-dom-of-term graph %)))
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

(defn print-type-analysis [graph]
  (->> graph
       (utils/get-terms)
       (filter #(or (uber/attr graph % :index)
                    (uber/attr graph % :indices)))
       (reduce #(assoc %1 (transform-record-to-map %2) (transform-record-to-map (uber/attr graph %2 :dom))) {})
       pr-str
       println))


(map transform-record-to-map [(r/->AnySpec) (r/->IntegerSpec)])
