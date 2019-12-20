(ns prolog-analyzer.test-helper
  (:require [prolog-analyzer.records :as r]
            [prolog-analyzer.record-utils :as ru]
            [prolog-analyzer.analyzer.domain :as dom]
            [prolog-analyzer.utils :as utils]
            [prolog-analyzer.parser.parser :as parser]
            [clojure.java.io :as io]
            [ubergraph.core :as uber]))


(defn capitalize [s]
  (str (.toUpperCase (.substring s 0 1)) (.toLowerCase (.substring s 1))))

(defn isCapitalized? [s]
  (= (capitalize s) s))


(defn to-spec-function [s]
  (if (or (= s "OneOf") (= s "And"))
    (eval (read-string (str "(comp r/->" s "Spec set)")))
    (eval (read-string (str "r/->" s "Spec")))
    ))


(declare to-term)

(defn is-compound? [s]
  (re-matches #"([a-z]*)\((.*)\)" s))

(defn to-clojure-list [list-record]
  (loop [term list-record
         res []]
    (let [head (ru/head term)
          tail (ru/tail term)]
      (if (nil? head)
        res
        (recur tail (conj res head))))))

(defn to-compound [s]
  (if-let [[_ f args :as p] (is-compound? s)]
    (r/->CompoundTerm f (to-clojure-list (to-term (str "[" args "]"))))))


(defn term-to-list [s]
  (if-let [[_ left right] (re-matches #"\[([^|]]*)\|(.*)\]" s)]
    (r/->ListTerm (to-term left) (to-term right))
    (apply ru/to-head-tail-list (map to-term (read-string s)))))

(defn to-term [s]
  (cond
    (symbol? s) (to-term (str s))
    (integer? s) (r/->IntegerTerm s)
    (float? s) (r/->FloatTerm s)
    (coll? (read-string s)) (term-to-list s)
    (is-compound? s) (to-compound s)
    (isCapitalized? s) (r/->VarTerm s)
    :else (r/->AtomTerm s)))


(declare to-spec)

(defmacro arglist [[p & q]]
  (if (empty? q)
    `[(to-spec ~p)]
    `(apply vector (concat [(to-spec ~p)] (arglist ~q)))
    ))

(defmacro spec-to-list [[_ b]]
  `(r/->ListSpec (to-spec ~b)))

(defmacro to-spec [s]
  (if (coll? s)
    (let [[p & [args & [r]]] s]
      (cond
        (= "List" p) `(spec-to-list ~s)
        (= "Compound" p) `((to-spec-function ~p) ~args (arglist ~r))
        (empty? args) `((to-spec-function ~p) [])
        :default `((to-spec-function ~p) (arglist ~args)))
      )
    `((to-spec-function (eval ~s))))
  )

(to-spec ("Tuple" [("Compound" "foo" ["Atom" "Integer"])]))

(defn properties []
  (read-string (slurp (io/file "properties.edn"))))




(defn read-in-file [path]
  (parser/process-prolog-file (properties) path))
