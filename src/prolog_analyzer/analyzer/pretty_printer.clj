(ns prolog-analyzer.analyzer.pretty-printer
  (:require [prolog-analyzer.utils :as utils]
            [clojure.pprint :refer [pprint]]
            [clojure.string]))

(defmulti to-string :type)
(defmethod to-string :var [{n :name}] n)
(defmethod to-string :anon_var [{n :name}] n) 
(defmethod to-string :list [{head :head tail :tail :as arg}]
  (cond 
    (= "[]" (:term tail)) (str "[" (to-string head) "]")
    (= :var (:type tail)) (str "[" (to-string head) "|" (to-string tail) "]")
    (= :anon_var (:type tail)) (str "[" (to-string head) "|" (to-string tail) "]")
    (= :list (:type tail)) (str "[" (clojure.string/join ", " (map to-string (utils/get-elements-of-list arg))) "]")))
(defmethod to-string :compound [{functor :functor arglist :arglist}]
  (str functor "(" (clojure.string/join ", " (map to-string arglist)) ")"))
(defmethod to-string :default [arg] arg)


(to-string {:type :list :head {:type :var :name "X"} :tail {:type :list :head {:type :var :name "Y"} :tail {:type :list :head {:type :var :name "Z"} :tail {:term "[]" :type :atomic}}}})

(defn to-pretty-map [{id-mapping :id-mapping :as result}]
  (let [cut-result (-> result (dissoc :id-mapping) (dissoc :args))]
    (reduce-kv
     (fn [m k {dom :dom relations :relations}]
       (let [new-rel (map (fn [r] (reduce-kv #(assoc %1 %2 (to-string (get id-mapping %3))) {} r)) relations)]
         (assoc m (to-string (get id-mapping k)) {:dom dom :relations new-rel})))
     {}
     cut-result)))

(defn pretty-print-result [result]
  (pprint (to-pretty-map result)))
