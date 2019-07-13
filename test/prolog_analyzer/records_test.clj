(ns prolog-analyzer.records-test
  (:require [prolog-analyzer.records :as sut]
            [ubergraph.core :as uber]
            [loom.graph]
            [loom.attr]
            [ubergraph.protocols]
            [clojure.test.check :as tc]
            [clojure.test.check.generators :as gen]
            [clojure.test.check.properties :as prop]
            [clojure.test :refer [deftest are is]]
            [clojure.template :refer [do-template]]))

(def expr (sut/->UserDefinedSpec "expr"))
(def op (sut/->UserDefinedSpec "op"))
(def cst (sut/->UserDefinedSpec "cst"))

(def test-defs
  {(sut/make-spec:user-defined "tree" [(sut/->SpecvarSpec "X")])
   (sut/->OneOfSpec #{(sut/->CompoundSpec "node" [(sut/make-spec:user-defined "tree" [(sut/->SpecvarSpec "X")]) (sut/->SpecvarSpec "X") (sut/make-spec:user-defined "tree" [(sut/->SpecvarSpec "X")])]) (sut/->ExactSpec "empty")})

   (sut/make-spec:user-defined "atomOrInt")
   (sut/->OneOfSpec #{(sut/->IntegerSpec) (sut/->AtomSpec)})

   (sut/make-spec:user-defined "atomOrVar")
   (sut/->OneOfSpec #{(sut/->AtomSpec) (sut/->VarSpec)})

   (sut/make-spec:user-defined "blob")
   (sut/->ExactSpec "blob")

   (sut/make-spec:user-defined "a")
   (sut/->OneOfSpec #{(sut/->TupleSpec [(sut/make-spec:user-defined "a")]) (sut/->ExactSpec "a")})

   expr
   (sut/->OneOfSpec #{cst, (sut/->CompoundSpec "expr" [op, expr, expr]), (sut/->CompoundSpec "neg" [expr])})

   op (sut/->ExactSpec "+")
   cst (sut/->CompoundSpec "cst" [(sut/->IntegerSpec)])


   })

(def user-def-tree-int (sut/make-spec:user-defined "tree" [(sut/->IntegerSpec)]))
(def user-def-atomOrInt (sut/make-spec:user-defined "atomOrInt"))
(def user-def-blob (sut/make-spec:user-defined "blob"))
(defn user-def-tree [value]
  (sut/make-spec:user-defined "tree" [value]))
