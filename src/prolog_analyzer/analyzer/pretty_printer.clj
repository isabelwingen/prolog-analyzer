(ns prolog-analyzer.analyzer.pretty-printer
  (:require [prolog-analyzer.utils :as utils]
            [prolog-analyzer.records :as r]
            [ubergraph.core :as uber]
            [loom.graph]
            [loom.attr]
            [clojure.pprint :refer [pprint print-table]]
            [clojure.string]))


(defn print-in-two-columns [n str1 str2]
  (let [diff (- n (count str1))]
    (print str1)
    (doseq [x (range 0 diff)] (print " "))
    (println str2)))

(defn print-in-columns [[n & ns] str & strs]
  (let [diff (- n (count str))]
    (print str)
    (doseq [x (range 0 diff)] (print " "))
    (if (nil? ns)
      (println (clojure.string/join " " strs))
      (apply print-in-columns ns strs))))

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

(print-table [{:a 1 :b 2 :c 3} {:b 5 :a 7 :c "dog"}])

(defn pretty-print-graph [title graph]
  (println title)
  (println)
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


(defn short-print [title graph]
  ;(println title)
  (println)
  (let [error-terms (->> graph
                         (utils/get-terms)
                         (remove contains-arti-term?)
                         (filter #(r/error-spec? (utils/get-dom-of-term graph %))))]
    (if (empty? error-terms)
      (println "No errors found")
      (doseq [t error-terms]
        (print-in-columns [20] (r/to-string t) (r/to-string (or (utils/get-dom-of-term graph t) (r/->AnySpec))))))
    (println "----------------------\n")))


(defn- tinker-error-message [env term]
  (str ":( There was an ERROR in term "
       (r/to-string term)
       "!\n"
       "This is the error message:\n"
       (r/to-string (or (utils/get-dom-of-term env term) (r/->AnySpec)))
       "\n"
       (uber/attr env term :indices)
       ))

(defn better-print [title graph]
  (println title)
  (println)
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

(defn print-type-information [title graph]
  (println title "\n")
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

(defn print-with-indices [title graph]
  (println title "\n")
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
       (print-table [:term :dom :index :indices])))
