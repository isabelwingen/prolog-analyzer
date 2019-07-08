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


(deftest to-head-tail-list
  (are [x y] (= x (apply sut/to-head-tail-list y))
    (sut/->EmptyListTerm) []

    (sut/->ListTerm (sut/->IntegerTerm 1) (sut/->EmptyListTerm)) [(sut/->IntegerTerm 1)]

    (sut/->ListTerm
     (sut/->IntegerTerm 1)
     (sut/->ListTerm (sut/->IntegerTerm 2) (sut/->EmptyListTerm)))
     [(sut/->IntegerTerm 1) (sut/->IntegerTerm 2)]
    ))

(deftest to-tuple-spec-test
  (are [x y] (= x (apply sut/to-tuple-spec y))
    (sut/->TupleSpec []) []
    (sut/->TupleSpec [(sut/->IntegerSpec)]) [(sut/->IntegerSpec)]
    (sut/->TupleSpec [(sut/->IntegerSpec) (sut/->AtomSpec)]) [(sut/->IntegerSpec) (sut/->AtomSpec)]))

(deftest has-specvars-test
  (are [in] (true? (sut/has-specvars in))
    (sut/->SpecvarSpec "X")
    (sut/->ListSpec (sut/->ListSpec (sut/->ListSpec (sut/->SpecvarSpec "X"))))
    (sut/make-spec:user-defined "tree" [(sut/->CompoundSpec "foo" [(sut/->SpecvarSpec "A")])]))
  (are [in] (nil? (sut/has-specvars in))
    (sut/->VarSpec)
    (sut/->ListSpec (sut/->AtomSpec))))

(deftest replace-specvars-with-any-test
  (are [in out] (= out (sut/replace-specvars-with-any in))
    (sut/->SpecvarSpec "X") (sut/->AnySpec)
    (sut/->ListSpec (sut/->SpecvarSpec "Y")) (sut/->ListSpec (sut/->AnySpec))
    (sut/->CompoundSpec "foo" [(sut/->SpecvarSpec "X") (sut/->SpecvarSpec "Y")]) (sut/->CompoundSpec "foo" [(sut/->AnySpec) (sut/->AnySpec)])))

(deftest length-of-list
  (are [list length] (= length (sut/length-of-list-term list))
    (sut/->EmptyListTerm) 0
    (sut/->ListTerm (sut/->IntegerTerm 1) (sut/->EmptyListTerm)) 1
    (sut/->ListTerm (sut/->IntegerTerm 1) (sut/->ListTerm (sut/->IntegerTerm 2) (sut/->EmptyListTerm))) 2
    (sut/->ListTerm (sut/->VarTerm "A") (sut/->ListTerm (sut/->VarTerm "B") (sut/->ListTerm (sut/->VarTerm "C") (sut/->EmptyListTerm)))) 3
    (sut/->ListTerm (sut/->VarTerm "H") (sut/->VarTerm "T")) :inf
    (sut/->ListTerm (sut/->VarTerm "_123") (sut/->VarTerm "_124")) :inf
    (sut/->ListTerm (sut/->IntegerTerm 1) (sut/->ListTerm (sut/->VarTerm "H") (sut/->VarTerm "T"))) :inf))
