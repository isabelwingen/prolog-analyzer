(ns prolog-analyzer.parser.transform-spec
  (:require [prolog-analyzer.records :as r]))

(defmulti  transform-spec "Transforms the raw edn of specs to a better suited format."
  :type)

(defmethod transform-spec :default [term]
  (case (:type term)
    :any (r/->AnySpec)
    :ground (r/->GroundSpec)
    :var (r/->VarSpec)
    :string (r/->StringSpec)
    :nonvar (r/->NonvarSpec)
    :number (r/->NumberSpec)
    :float (r/->FloatSpec)
    :integer (r/->IntegerSpec)
    :atom (r/->AtomSpec)
    :atomic (r/->AtomicSpec)
    :int (r/->IntegerSpec)
    :emptylist (r/->EmptyListSpec)
    (r/->UserDefinedSpec (name (:type term)))))


(defmethod transform-spec :list [{list-type :list-type}]
  (r/->ListSpec (transform-spec list-type)))

(defmethod transform-spec :compound [{functor :functor arglist :arglist}]
  (r/->CompoundSpec functor (map transform-spec arglist)))

(defmethod transform-spec :one-of [{arglist :arglist}]
  (r/->OneOfSpec (set (map transform-spec arglist))))

(defmethod transform-spec :and [{arglist :arglist}]
  (r/->AndSpec (set (map transform-spec arglist))))

(defmethod transform-spec :tuple [{arglist :arglist}]
  (r/->TupleSpec (map transform-spec arglist)))

(defmethod transform-spec :same [{term :term}]
  (r/->ExactSpec term))

(defmethod transform-spec :specvar [{n :name}]
  (r/->SpecvarSpec n))

(defmethod transform-spec :placeholder [{n :name super-of :super-of :as a}]
  (if (nil? super-of)
    (r/->PlaceholderSpec n)
    (assoc (r/->PlaceholderSpec n) :super-of super-of)))

(defmethod transform-spec :userdef [{n :name arglist :arglist}]
  (assoc (r/->UserDefinedSpec n) :arglist (map transform-spec arglist)))
