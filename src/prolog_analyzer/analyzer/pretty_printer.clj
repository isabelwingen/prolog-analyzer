(ns prolog-analyzer.analyzer.pretty-printer
  (:require [clojure.pprint :refer [pprint]]))

(defmulti to-string :type)
(defmethod to-string :var [{n :name}] n)
(defmethod to-string :anon_var [{n :name}] n)
(defmethod to-string :head-tail-list [{head :head tail :tail}]
  (str "[" (to-string head) "|" (to-string tail) "]"))
(defmethod to-string :default [arg] arg)


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
