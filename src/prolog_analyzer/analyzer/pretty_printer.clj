(ns prolog-analyzer.analyzer.pretty-printer
  (:require [prolog-analyzer.utils :as utils]
            [ubergraph.core :as uber]
            [loom.graph]
            [loom.attr]
            [clojure.pprint :refer [pprint]]
            [clojure.string]))

(defmulti to-string :type)
(defmethod to-string :var [{n :name}] (str n))
(defmethod to-string :atom [{term :term}] (str term))
(defmethod to-string :atomic [{term :term}] (str term))
(defmethod to-string :integer [{value :value}] (str value))
(defmethod to-string :number [{value :value}] (str value))
(defmethod to-string :float [{value :value}] (str value))
(defmethod to-string :anon_var [{n :name}] (str n)) 
(defmethod to-string :list [{head :head tail :tail :as arg}]
  (cond 
    (= "[]" (:term tail)) (str "[" (to-string head) "]")
    (= :var (:type tail)) (str "[" (to-string head) "|" (to-string tail) "]")
    (= :anon_var (:type tail)) (str "[" (to-string head) "|" (to-string tail) "]")
    (= :list (:type tail)) (str "[" (clojure.string/join ", " (map to-string (utils/get-elements-of-list arg))) "]")
    :else "blabla"))
(defmethod to-string :compound [{functor :functor arglist :arglist}]
  (str functor "(" (clojure.string/join ", " (map to-string arglist)) ")"))
(defmethod to-string :default [arg]
  (if (contains? arg :spec)
    (str arg)
    (str arg)))

 
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
                        (map to-string)
                        (map count)
                        (apply max)
                        (+ 4))]
    (doseq [node nodes]
      (print "\t")
      (println (to-string node))
      (let [attrs (uber/attrs graph node)
            ks (keys attrs)]
        (doseq [key ks]
          (print-in-columns [(+ 2 max-length)] "" key)
          (if (coll? (get attrs key))
            (doseq [val (get attrs key)]
              (print-in-columns [(+ 4 max-length)] "" (to-string val)))
            (print-in-columns [(+ 4 max-length)] "" (get attrs key))))))))

(defn print-edges [graph]
  (let [edges (uber/edges graph)
        srcs (map uber/src edges)
        dests (map uber/dest edges)
        max-src-length (->> srcs
                            (map to-string)
                            (map count)
                            (apply max)
                            (+ 2))
        max-dest-length (->> dests
                             (map to-string)
                             (map count)
                             (apply max)
                             (+ 5))]
    (doseq [edge edges]
      (print "\t")
      (print-in-columns [max-src-length 4 max-dest-length]
                        (to-string (uber/src edge))
                        "->"
                        (to-string (uber/dest edge))
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
      (println "#" clause-id ":" pre-spec)
      (pretty-print-graph g)
      (println "--------------------------------------------------------------------\n"))))
