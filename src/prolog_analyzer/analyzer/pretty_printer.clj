(ns prolog-analyzer.analyzer.pretty-printer
  (:require [prolog-analyzer.utils :as utils]
            [prolog-analyzer.records :as r]
            [ubergraph.core :as uber]
            [loom.graph]
            [loom.attr]
            [clojure.pprint :refer [pprint]]
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
  (let [nodes (utils/get-terms graph)
        max-length (->> nodes
                        (map r/to-string)
                        (map count)
                        (apply max)
                        (+ 2))]
    (doseq [node nodes]
      (print "\t")
      (let [attrs (uber/attrs graph node)
            ks (keys attrs)]
        (if (empty? attrs)
          (print-in-columns [max-length] (r/to-string node) "Nothing")
          (doseq [key ks]
            (if (= key (first ks))
              (print-in-columns [max-length] (r/to-string node) key)
              (print-in-columns [(+ 2 max-length)] "" key))
            (let [value (get attrs key)]
              (cond
                (satisfies? prolog-analyzer.records/spec value) (print-in-columns [(+ 4 max-length) 15] "" (str (:origin value)) (r/to-string value))
                (sequential? value) (doseq [val (get attrs key)]
                                      (print-in-columns [(+ 4 max-length) 15] "" (str (:origin val)) (r/to-string val)))
                :else (print-in-columns [(+ 4 max-length)] "" (get attrs key))))))
      (println "-------------------------------------------")))))

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
    (doseq [edge edges]
      (print "\t")
      (print-in-columns [max-src-length 4 max-dest-length]
                        (r/to-string (uber/src edge))
                        "->"
                        (r/to-string (uber/dest edge))
                        (str (uber/attrs graph edge))))))


(defn pretty-print-graph [graph]
  (let [nodes (utils/get-terms graph)
        nd-num (count nodes)
        edges (uber/edges graph)
        edg-num (count edges)]
    (println nd-num "Nodes:")
    (print-nodes graph)
    (println edg-num "Edges:")
    (when (> edg-num 0)
      (print-edges graph))))

(defn pretty-print-analysis-result [res]
  (doseq [[[clause-id pre-spec] g] res]
    (do
      (println "#" clause-id ":" (r/to-string pre-spec))
      (pretty-print-graph g)
      (println "--------------------------------------------------------------------\n"))))


(defn- arti-term? [term]
  (.startsWith (str (:name term)) "A__"))

(defn- contains-arti-term? [term]
  (or (arti-term? term)
      (and (arti-term? (:head term)) (arti-term? (:tail term)))
      (and ((complement nil?) (:arglist term)) (every? #(not (arti-term? %)) (:arglist term)))))

(defn short-result [res]
  (doseq [[[clause-id pre-spec] graph] res]
    (do
      (println "#" clause-id ":" (r/to-string pre-spec))
      (let [error-terms (->> graph
                            (utils/get-terms)
                            (remove contains-arti-term?)
                            (filter #(r/error-spec? (utils/get-dom-of-term graph %))))]
        (if (empty? error-terms)
          (println "No errors found")
          (doseq [t error-terms]
            (print-in-columns [20] (r/to-string t) (r/to-string (utils/get-dom-of-term graph t))))))
      (println))))
