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
   (sut/->OneOfSpec [(sut/->CompoundSpec "node" [(sut/make-spec:user-defined "tree" [(sut/->SpecvarSpec "X")]) (sut/->SpecvarSpec "X") (sut/make-spec:user-defined "tree" [(sut/->SpecvarSpec "X")])]) (sut/->ExactSpec "empty")])

   (sut/make-spec:user-defined "atomOrInt")
   (sut/->OneOfSpec [(sut/->IntegerSpec) (sut/->AtomSpec)])

   (sut/make-spec:user-defined "blob")
   (sut/->ExactSpec "blob")

   (sut/make-spec:user-defined "a")
   (sut/->OneOfSpec [(sut/->TupleSpec [(sut/make-spec:user-defined "a")]) (sut/->ExactSpec "a")])

   expr
   (sut/->OneOfSpec [cst, (sut/->CompoundSpec "expr" [op, expr, expr]), (sut/->CompoundSpec "neg" [expr])])

   op (sut/->ExactSpec "+")
   cst (sut/->CompoundSpec "cst" [(sut/->IntegerSpec)])


   })

(def user-def-tree-int (sut/make-spec:user-defined "tree" [(sut/->IntegerSpec)]))
(def user-def-atomOrInt (sut/make-spec:user-defined "atomOrInt"))
(def user-def-blob (sut/make-spec:user-defined "blob"))
(defn user-def-tree [value]
  (sut/make-spec:user-defined "tree" [value]))

(deftest resolve-definition
  (are [in expected] (= expected (sut/resolve-definition-with-parameters in test-defs))
    user-def-tree-int (sut/->OneOfSpec [(sut/->CompoundSpec "node" [user-def-tree-int (sut/->IntegerSpec) user-def-tree-int]) (sut/->ExactSpec "empty")])
    (user-def-tree (sut/->SpecvarSpec "Y")) (sut/->OneOfSpec [(sut/->CompoundSpec "node" [(user-def-tree (sut/->SpecvarSpec "Y")) (sut/->SpecvarSpec "Y") (user-def-tree (sut/->SpecvarSpec "Y"))]) (sut/->ExactSpec "empty")])
    (user-def-tree (sut/->SpecvarSpec "A")) (sut/->OneOfSpec [(sut/->CompoundSpec "node" [(user-def-tree (sut/->SpecvarSpec "A")) (sut/->SpecvarSpec "A") (user-def-tree (sut/->SpecvarSpec "A"))]) (sut/->ExactSpec "empty")])
    (user-def-tree (sut/make-spec:user-defined "blob")) (sut/->OneOfSpec [(sut/->CompoundSpec "node" [(user-def-tree (sut/make-spec:user-defined "blob")) (sut/make-spec:user-defined "blob") (user-def-tree (sut/make-spec:user-defined "blob"))]) (sut/->ExactSpec "empty")])

    (sut/make-spec:user-defined "atomOrInt") (sut/->OneOfSpec [(sut/->IntegerSpec) (sut/->AtomSpec)])
    (sut/make-spec:user-defined "blob") (sut/->ExactSpec "blob")

    ))


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

(deftest to-or-spec-test
  (are [x y] (= x (apply sut/to-or-spec nil y))
    (sut/->ErrorSpec "Cannot build empty one-of") []
    (sut/->IntegerSpec) [(sut/->IntegerSpec)]
    (sut/->OneOfSpec [(sut/->IntegerSpec) (sut/->AtomSpec)]) [(sut/->IntegerSpec) (sut/->AtomSpec)]))

(deftest intersect
  (do-template [left right result] (is (= result (sut/intersect left right test-defs)) (str "Intersect of " (sut/to-string left) " " (sut/to-string right)))
               (sut/->IntegerSpec) (sut/->IntegerSpec) (sut/->IntegerSpec)
               (sut/->IntegerSpec) (sut/->FloatSpec) sut/DISJOINT
               (sut/->IntegerSpec) (sut/->NumberSpec) (sut/->IntegerSpec)
               (sut/->IntegerSpec) (sut/->ExactSpec "cake") sut/DISJOINT
               (sut/->IntegerSpec) (sut/->AtomSpec) sut/DISJOINT
               (sut/->IntegerSpec) (sut/->AtomicSpec) (sut/->IntegerSpec)
               (sut/->IntegerSpec) (sut/->CompoundSpec "foo" [(sut/->AtomicSpec)]) sut/DISJOINT
               (sut/->IntegerSpec) (sut/->ListSpec (sut/->AtomicSpec)) sut/DISJOINT
               (sut/->IntegerSpec) (sut/->EmptyListSpec) sut/DISJOINT
               (sut/->IntegerSpec) (sut/->TupleSpec [(sut/->AtomicSpec)]) sut/DISJOINT
               (sut/->IntegerSpec) (sut/->GroundSpec) (sut/->IntegerSpec)
               (sut/->IntegerSpec) (sut/->NonvarSpec) (sut/->IntegerSpec)
               (sut/->IntegerSpec) (sut/->VarSpec) sut/DISJOINT
               (sut/->IntegerSpec) (sut/->AnySpec) (sut/->IntegerSpec)
               (sut/->IntegerSpec) (sut/make-spec:user-defined "atomOrInt") (sut/->IntegerSpec)

               (sut/->FloatSpec) (sut/->IntegerSpec) sut/DISJOINT
               (sut/->FloatSpec) (sut/->FloatSpec) (sut/->FloatSpec)
               (sut/->FloatSpec) (sut/->NumberSpec) (sut/->FloatSpec)
               (sut/->FloatSpec) (sut/->ExactSpec "cake") sut/DISJOINT
               (sut/->FloatSpec) (sut/->AtomSpec) sut/DISJOINT
               (sut/->FloatSpec) (sut/->AtomicSpec) (sut/->FloatSpec)
               (sut/->FloatSpec) (sut/->CompoundSpec "foo" [(sut/->AtomicSpec)]) sut/DISJOINT
               (sut/->FloatSpec) (sut/->ListSpec (sut/->AtomicSpec)) sut/DISJOINT
               (sut/->FloatSpec) (sut/->EmptyListSpec) sut/DISJOINT
               (sut/->FloatSpec) (sut/->TupleSpec [(sut/->AtomicSpec)]) sut/DISJOINT
               (sut/->FloatSpec) (sut/->GroundSpec) (sut/->FloatSpec)
               (sut/->FloatSpec) (sut/->NonvarSpec) (sut/->FloatSpec)
               (sut/->FloatSpec) (sut/->VarSpec) sut/DISJOINT
               (sut/->FloatSpec) (sut/->AnySpec) (sut/->FloatSpec)
               (sut/->FloatSpec) (sut/make-spec:user-defined "atomOrInt") sut/DISJOINT

               (sut/->NumberSpec) (sut/->IntegerSpec) (sut/->IntegerSpec)
               (sut/->NumberSpec) (sut/->FloatSpec) (sut/->FloatSpec)
               (sut/->NumberSpec) (sut/->NumberSpec) (sut/->NumberSpec)
               (sut/->NumberSpec) (sut/->ExactSpec "cake") sut/DISJOINT
               (sut/->NumberSpec) (sut/->AtomSpec) sut/DISJOINT
               (sut/->NumberSpec) (sut/->AtomicSpec) (sut/->NumberSpec)
               (sut/->NumberSpec) (sut/->CompoundSpec "foo" [(sut/->AtomicSpec)]) sut/DISJOINT
               (sut/->NumberSpec) (sut/->ListSpec (sut/->AtomicSpec)) sut/DISJOINT
               (sut/->NumberSpec) (sut/->EmptyListSpec) sut/DISJOINT
               (sut/->NumberSpec) (sut/->TupleSpec [(sut/->AtomicSpec)]) sut/DISJOINT
               (sut/->NumberSpec) (sut/->GroundSpec) (sut/->NumberSpec)
               (sut/->NumberSpec) (sut/->NonvarSpec) (sut/->NumberSpec)
               (sut/->NumberSpec) (sut/->VarSpec) sut/DISJOINT
               (sut/->NumberSpec) (sut/->AnySpec) (sut/->NumberSpec)
               (sut/->NumberSpec) (sut/make-spec:user-defined "atomOrInt") (sut/->IntegerSpec)

               (sut/->ExactSpec "cake") (sut/->IntegerSpec) sut/DISJOINT
               (sut/->ExactSpec "cake") (sut/->FloatSpec) sut/DISJOINT
               (sut/->ExactSpec "cake") (sut/->NumberSpec) sut/DISJOINT
               (sut/->ExactSpec "cake") (sut/->ExactSpec "cake") (sut/->ExactSpec "cake")
               (sut/->ExactSpec "cake") (sut/->ExactSpec "nocake") sut/DISJOINT
               (sut/->ExactSpec "cake") (sut/->AtomSpec) (sut/->ExactSpec "cake")
               (sut/->ExactSpec "cake") (sut/->AtomicSpec) (sut/->ExactSpec "cake")
               (sut/->ExactSpec "cake") (sut/->CompoundSpec "foo" [(sut/->AtomicSpec)]) sut/DISJOINT
               (sut/->ExactSpec "cake") (sut/->ListSpec (sut/->AtomicSpec)) sut/DISJOINT
               (sut/->ExactSpec "cake") (sut/->EmptyListSpec) sut/DISJOINT
               (sut/->ExactSpec "cake") (sut/->TupleSpec [(sut/->AtomicSpec)]) sut/DISJOINT
               (sut/->ExactSpec "cake") (sut/->GroundSpec) (sut/->ExactSpec "cake")
               (sut/->ExactSpec "cake") (sut/->NonvarSpec) (sut/->ExactSpec "cake")
               (sut/->ExactSpec "cake") (sut/->VarSpec) sut/DISJOINT
               (sut/->ExactSpec "cake") (sut/->AnySpec) (sut/->ExactSpec "cake")
               (sut/->ExactSpec "cake") (sut/make-spec:user-defined "atomOrInt") (sut/->ExactSpec "cake")

               (sut/->AtomSpec) (sut/->IntegerSpec) sut/DISJOINT
               (sut/->AtomSpec) (sut/->FloatSpec) sut/DISJOINT
               (sut/->AtomSpec) (sut/->NumberSpec) sut/DISJOINT
               (sut/->AtomSpec) (sut/->ExactSpec "cake") (sut/->ExactSpec "cake")
               (sut/->AtomSpec) (sut/->AtomSpec) (sut/->AtomSpec)
               (sut/->AtomSpec) (sut/->AtomicSpec) (sut/->AtomSpec)
               (sut/->AtomSpec) (sut/->CompoundSpec "foo" [(sut/->AtomicSpec)]) sut/DISJOINT
               (sut/->AtomSpec) (sut/->ListSpec (sut/->AtomicSpec)) sut/DISJOINT
               (sut/->AtomSpec) (sut/->EmptyListSpec) sut/DISJOINT
               (sut/->AtomSpec) (sut/->TupleSpec [(sut/->AtomicSpec)]) sut/DISJOINT
               (sut/->AtomSpec) (sut/->GroundSpec) (sut/->AtomSpec)
               (sut/->AtomSpec) (sut/->NonvarSpec) (sut/->AtomSpec)
               (sut/->AtomSpec) (sut/->VarSpec) sut/DISJOINT
               (sut/->AtomSpec) (sut/->AnySpec) (sut/->AtomSpec)
               (sut/->AtomSpec) (sut/make-spec:user-defined "atomOrInt") (sut/->AtomSpec)

               (sut/->AtomicSpec) (sut/->IntegerSpec) (sut/->IntegerSpec)
               (sut/->AtomicSpec) (sut/->FloatSpec) (sut/->FloatSpec)
               (sut/->AtomicSpec) (sut/->NumberSpec) (sut/->NumberSpec)
               (sut/->AtomicSpec) (sut/->ExactSpec "cake") (sut/->ExactSpec "cake")
               (sut/->AtomicSpec) (sut/->AtomSpec) (sut/->AtomSpec)
               (sut/->AtomicSpec) (sut/->AtomicSpec) (sut/->AtomicSpec)
               (sut/->AtomicSpec) (sut/->CompoundSpec "foo" [(sut/->AtomicSpec)]) sut/DISJOINT
               (sut/->AtomicSpec) (sut/->ListSpec (sut/->AtomicSpec)) (sut/->EmptyListSpec)
               (sut/->AtomicSpec) (sut/->EmptyListSpec) (sut/->EmptyListSpec)
               (sut/->AtomicSpec) (sut/->TupleSpec [(sut/->AtomicSpec)]) sut/DISJOINT
               (sut/->AtomicSpec) (sut/->GroundSpec) (sut/->AtomicSpec)
               (sut/->AtomicSpec) (sut/->NonvarSpec) (sut/->AtomicSpec)
               (sut/->AtomicSpec) (sut/->VarSpec) sut/DISJOINT
               (sut/->AtomicSpec) (sut/->AnySpec) (sut/->AtomicSpec)
               (sut/->AtomicSpec) (sut/make-spec:user-defined "atomOrInt") (sut/->OneOfSpec [(sut/->IntegerSpec) (sut/->AtomSpec)])


               (sut/->CompoundSpec "foo" [(sut/->IntegerSpec) (sut/->AtomSpec)]) (sut/->IntegerSpec) sut/DISJOINT
               (sut/->CompoundSpec "foo" [(sut/->IntegerSpec) (sut/->AtomSpec)]) (sut/->FloatSpec) sut/DISJOINT
               (sut/->CompoundSpec "foo" [(sut/->IntegerSpec) (sut/->AtomSpec)]) (sut/->NumberSpec) sut/DISJOINT
               (sut/->CompoundSpec "foo" [(sut/->IntegerSpec) (sut/->AtomSpec)]) (sut/->ExactSpec "cake") sut/DISJOINT
               (sut/->CompoundSpec "foo" [(sut/->IntegerSpec) (sut/->AtomSpec)]) (sut/->AtomSpec) sut/DISJOINT
               (sut/->CompoundSpec "foo" [(sut/->IntegerSpec) (sut/->AtomSpec)]) (sut/->AtomicSpec) sut/DISJOINT
               (sut/->CompoundSpec "foo" [(sut/->IntegerSpec) (sut/->AtomSpec)]) (sut/->ListSpec (sut/->AtomicSpec)) sut/DISJOINT
               (sut/->CompoundSpec "foo" [(sut/->IntegerSpec) (sut/->AtomSpec)]) (sut/->EmptyListSpec) sut/DISJOINT
               (sut/->CompoundSpec "foo" [(sut/->IntegerSpec) (sut/->AtomSpec)]) (sut/->TupleSpec [(sut/->AtomicSpec)]) sut/DISJOINT
               (sut/->CompoundSpec "foo" [(sut/->IntegerSpec) (sut/->AtomSpec)]) (sut/->VarSpec) sut/DISJOINT

               (sut/->CompoundSpec "foo" [(sut/->IntegerSpec) (sut/->VarSpec)]) (sut/->GroundSpec) sut/DISJOINT
               (sut/->CompoundSpec "foo" [(sut/->IntegerSpec) (sut/->AtomSpec)]) (sut/->GroundSpec) (sut/->CompoundSpec "foo" [(sut/->IntegerSpec) (sut/->AtomSpec)])
               (sut/->CompoundSpec "foo" [(sut/->IntegerSpec) (sut/->VarSpec)]) (sut/->NonvarSpec) (sut/->CompoundSpec "foo" [(sut/->IntegerSpec) (sut/->VarSpec)])
               (sut/->CompoundSpec "foo" [(sut/->IntegerSpec)]) (sut/->CompoundSpec "foo" [(sut/->IntegerSpec)]) (sut/->CompoundSpec "foo" [(sut/->IntegerSpec)])
               (sut/->CompoundSpec "foo" [(sut/->IntegerSpec)]) (sut/->CompoundSpec "nofoo" [(sut/->IntegerSpec)]) sut/DISJOINT
               (sut/->CompoundSpec "foo" [(sut/->IntegerSpec)]) (sut/->CompoundSpec "foo" [(sut/->AtomSpec)]) sut/DISJOINT
               (sut/->CompoundSpec "foo" [(sut/->IntegerSpec)]) (sut/->CompoundSpec "foo" [(sut/->IntegerSpec) (sut/->AtomSpec)]) sut/DISJOINT
               (sut/->CompoundSpec "foo" [(sut/->IntegerSpec) (sut/->AtomSpec)]) (sut/->AnySpec) (sut/->CompoundSpec "foo" [(sut/->IntegerSpec) (sut/->AtomSpec)])
               (sut/->CompoundSpec "foo" [(sut/->IntegerSpec) (sut/->AtomSpec)]) (sut/make-spec:user-defined "atomOrInt") sut/DISJOINT

               (sut/->ListSpec (sut/->IntegerSpec)) (sut/->IntegerSpec) sut/DISJOINT
               (sut/->ListSpec (sut/->IntegerSpec)) (sut/->FloatSpec) sut/DISJOINT
               (sut/->ListSpec (sut/->IntegerSpec)) (sut/->NumberSpec) sut/DISJOINT
               (sut/->ListSpec (sut/->IntegerSpec)) (sut/->ExactSpec "cake") sut/DISJOINT
               (sut/->ListSpec (sut/->IntegerSpec)) (sut/->AtomSpec) sut/DISJOINT
               (sut/->ListSpec (sut/->IntegerSpec)) (sut/->AtomicSpec) (sut/->EmptyListSpec)
               (sut/->ListSpec (sut/->IntegerSpec)) (sut/->CompoundSpec "foo" [(sut/->AtomicSpec)]) sut/DISJOINT
               (sut/->ListSpec (sut/->IntegerSpec)) (sut/->ListSpec (sut/->AtomicSpec)) (sut/->ListSpec (sut/->IntegerSpec))
               (sut/->ListSpec (sut/->IntegerSpec)) (sut/->ListSpec (sut/->AtomSpec)) sut/DISJOINT
               (sut/->ListSpec (sut/->IntegerSpec)) (sut/->EmptyListSpec) (sut/->EmptyListSpec)
               (sut/->ListSpec (sut/->IntegerSpec)) (sut/->TupleSpec [(sut/->AtomicSpec)]) (sut/->TupleSpec [(sut/->IntegerSpec)])
               (sut/->ListSpec (sut/->IntegerSpec)) (sut/->TupleSpec [(sut/->AtomSpec)]) sut/DISJOINT
               (sut/->ListSpec (sut/->IntegerSpec)) (sut/->GroundSpec) (sut/->ListSpec (sut/->IntegerSpec))
               (sut/->ListSpec (sut/->VarSpec)) (sut/->GroundSpec) sut/DISJOINT
               (sut/->ListSpec (sut/->IntegerSpec)) (sut/->NonvarSpec) (sut/->ListSpec (sut/->IntegerSpec))
               (sut/->ListSpec (sut/->VarSpec)) (sut/->NonvarSpec) (sut/->ListSpec (sut/->VarSpec))
               (sut/->ListSpec (sut/->IntegerSpec)) (sut/->VarSpec) sut/DISJOINT
               (sut/->ListSpec (sut/->IntegerSpec)) (sut/->AnySpec) (sut/->ListSpec (sut/->IntegerSpec))
               (sut/->ListSpec (sut/->IntegerSpec)) (sut/make-spec:user-defined "atomOrInt") sut/DISJOINT

               (sut/->EmptyListSpec) (sut/->IntegerSpec) sut/DISJOINT
               (sut/->EmptyListSpec) (sut/->FloatSpec) sut/DISJOINT
               (sut/->EmptyListSpec) (sut/->NumberSpec) sut/DISJOINT
               (sut/->EmptyListSpec) (sut/->ExactSpec "cake") sut/DISJOINT
               (sut/->EmptyListSpec) (sut/->AtomSpec) sut/DISJOINT
               (sut/->EmptyListSpec) (sut/->AtomicSpec) (sut/->EmptyListSpec)
               (sut/->EmptyListSpec) (sut/->CompoundSpec "foo" [(sut/->AtomicSpec)]) sut/DISJOINT
               (sut/->EmptyListSpec) (sut/->ListSpec (sut/->AtomicSpec)) (sut/->EmptyListSpec)
               (sut/->EmptyListSpec) (sut/->EmptyListSpec) (sut/->EmptyListSpec)
               (sut/->EmptyListSpec) (sut/->TupleSpec [(sut/->AtomicSpec)]) sut/DISJOINT
               (sut/->EmptyListSpec) (sut/->TupleSpec []) (sut/->EmptyListSpec)
               (sut/->EmptyListSpec) (sut/->GroundSpec) (sut/->EmptyListSpec)
               (sut/->EmptyListSpec) (sut/->NonvarSpec) (sut/->EmptyListSpec)
               (sut/->EmptyListSpec) (sut/->VarSpec) sut/DISJOINT
               (sut/->EmptyListSpec) (sut/->AnySpec) (sut/->EmptyListSpec)
               (sut/->EmptyListSpec) (sut/make-spec:user-defined "atomOrInt") sut/DISJOINT

               (sut/->TupleSpec [(sut/->IntegerSpec)]) (sut/->IntegerSpec) sut/DISJOINT
               (sut/->TupleSpec [(sut/->IntegerSpec)]) (sut/->FloatSpec) sut/DISJOINT
               (sut/->TupleSpec [(sut/->IntegerSpec)]) (sut/->NumberSpec) sut/DISJOINT
               (sut/->TupleSpec [(sut/->IntegerSpec)]) (sut/->ExactSpec "cake") sut/DISJOINT
               (sut/->TupleSpec [(sut/->IntegerSpec)]) (sut/->AtomSpec) sut/DISJOINT
               (sut/->TupleSpec [(sut/->IntegerSpec)]) (sut/->AtomicSpec) sut/DISJOINT
               (sut/->TupleSpec []) (sut/->AtomicSpec) (sut/->EmptyListSpec)
               (sut/->TupleSpec [(sut/->IntegerSpec)]) (sut/->CompoundSpec "foo" [(sut/->AtomicSpec)]) sut/DISJOINT
               (sut/->TupleSpec [(sut/->IntegerSpec)]) (sut/->ListSpec (sut/->AtomicSpec)) (sut/->TupleSpec [(sut/->IntegerSpec)])
               (sut/->TupleSpec [(sut/->IntegerSpec)]) (sut/->EmptyListSpec) sut/DISJOINT
               (sut/->TupleSpec []) (sut/->EmptyListSpec) (sut/->EmptyListSpec)
               (sut/->TupleSpec [(sut/->IntegerSpec)]) (sut/->TupleSpec []) sut/DISJOINT
               (sut/->TupleSpec [(sut/->IntegerSpec)]) (sut/->TupleSpec [(sut/->AtomSpec)]) sut/DISJOINT
               (sut/->TupleSpec [(sut/->IntegerSpec)]) (sut/->TupleSpec [(sut/->AtomicSpec)]) (sut/->TupleSpec [(sut/->IntegerSpec)])
               (sut/->TupleSpec [(sut/->IntegerSpec)]) (sut/->GroundSpec) (sut/->TupleSpec [(sut/->IntegerSpec)])
               (sut/->TupleSpec [(sut/->VarSpec)]) (sut/->GroundSpec) sut/DISJOINT
               (sut/->TupleSpec [(sut/->IntegerSpec)]) (sut/->NonvarSpec) (sut/->TupleSpec [(sut/->IntegerSpec)])
               (sut/->TupleSpec [(sut/->VarSpec)]) (sut/->NonvarSpec) (sut/->TupleSpec [(sut/->VarSpec)])
               (sut/->TupleSpec [(sut/->IntegerSpec)]) (sut/->VarSpec) sut/DISJOINT
               (sut/->TupleSpec [(sut/->IntegerSpec)]) (sut/->AnySpec) (sut/->TupleSpec [(sut/->IntegerSpec)])
               (sut/->TupleSpec [(sut/->IntegerSpec)]) (sut/make-spec:user-defined "atomOrInt") sut/DISJOINT


               (sut/->GroundSpec) (sut/->GroundSpec) (sut/->GroundSpec)
               (sut/->GroundSpec) (sut/->NonvarSpec) (sut/->GroundSpec)
               (sut/->GroundSpec) (sut/->AnySpec) (sut/->GroundSpec)
               (sut/->GroundSpec) (sut/->VarSpec) sut/DISJOINT
               (sut/->GroundSpec) (sut/make-spec:user-defined "atomOrInt") (sut/make-spec:user-defined "atomOrInt") ;;TODO: okay?

               (sut/->NonvarSpec) (sut/->GroundSpec) (sut/->GroundSpec)
               (sut/->NonvarSpec) (sut/->NonvarSpec) (sut/->NonvarSpec)
               (sut/->NonvarSpec) (sut/->AnySpec) (sut/->NonvarSpec)
               (sut/->NonvarSpec) (sut/->VarSpec) sut/DISJOINT
               (sut/->NonvarSpec) (sut/make-spec:user-defined "atomOrInt") (sut/->OneOfSpec [(sut/->IntegerSpec) (sut/->AtomSpec)])

               (sut/->VarSpec) (sut/->IntegerSpec) sut/DISJOINT
               (sut/->VarSpec) (sut/->FloatSpec) sut/DISJOINT
               (sut/->VarSpec) (sut/->NumberSpec) sut/DISJOINT
               (sut/->VarSpec) (sut/->ExactSpec "cake") sut/DISJOINT
               (sut/->VarSpec) (sut/->AtomSpec) sut/DISJOINT
               (sut/->VarSpec) (sut/->AtomicSpec) sut/DISJOINT
               (sut/->VarSpec) (sut/->CompoundSpec "foo" [(sut/->AtomSpec)]) sut/DISJOINT
               (sut/->VarSpec) (sut/->ListSpec (sut/->AtomicSpec)) sut/DISJOINT
               (sut/->VarSpec) (sut/->EmptyListSpec) sut/DISJOINT
               (sut/->VarSpec) (sut/->TupleSpec [(sut/->VarSpec)]) sut/DISJOINT
               (sut/->VarSpec) (sut/->GroundSpec) sut/DISJOINT
               (sut/->VarSpec) (sut/->NonvarSpec) sut/DISJOINT
               (sut/->VarSpec) (sut/->VarSpec) (sut/->VarSpec)
               (sut/->VarSpec) (sut/->AnySpec) (sut/->VarSpec)
               (sut/->VarSpec) (sut/make-spec:user-defined "atomOrInt") sut/DISJOINT

               (sut/->AnySpec) (sut/->IntegerSpec) (sut/->IntegerSpec)
               (sut/->AnySpec) (sut/->FloatSpec) (sut/->FloatSpec)
               (sut/->AnySpec) (sut/->NumberSpec) (sut/->NumberSpec)
               (sut/->AnySpec) (sut/->ExactSpec "cake") (sut/->ExactSpec "cake")
               (sut/->AnySpec) (sut/->AtomSpec) (sut/->AtomSpec)
               (sut/->AnySpec) (sut/->AtomicSpec) (sut/->AtomicSpec)
               (sut/->AnySpec) (sut/->CompoundSpec "foo" [(sut/->AtomSpec)]) (sut/->CompoundSpec "foo" [(sut/->AtomSpec)])
               (sut/->AnySpec) (sut/->ListSpec (sut/->AtomicSpec)) (sut/->ListSpec (sut/->AtomicSpec))
               (sut/->AnySpec) (sut/->EmptyListSpec) (sut/->EmptyListSpec)
               (sut/->AnySpec) (sut/->TupleSpec [(sut/->AnySpec)]) (sut/->TupleSpec [(sut/->AnySpec)])
               (sut/->AnySpec) (sut/->GroundSpec) (sut/->GroundSpec)
               (sut/->AnySpec) (sut/->NonvarSpec) (sut/->NonvarSpec)
               (sut/->AnySpec) (sut/->AnySpec) (sut/->AnySpec)
               (sut/->AnySpec) (sut/->VarSpec) (sut/->VarSpec)
               (sut/->AnySpec) (sut/make-spec:user-defined "atomOrInt") (sut/make-spec:user-defined "atomOrInt") ;;TODO: clarify behaviour

               (sut/make-spec:user-defined "blob") (sut/->IntegerSpec) sut/DISJOINT
               (sut/make-spec:user-defined "blob") (sut/->FloatSpec) sut/DISJOINT
               (sut/make-spec:user-defined "blob") (sut/->NumberSpec) sut/DISJOINT
               (sut/make-spec:user-defined "blob") (sut/->ExactSpec "cake") sut/DISJOINT
               (sut/make-spec:user-defined "blob") (sut/->ExactSpec "blob") (sut/->ExactSpec "blob")
               (sut/make-spec:user-defined "blob") (sut/->AtomSpec) (sut/->ExactSpec "blob")
               (sut/make-spec:user-defined "blob") (sut/->AtomicSpec) (sut/->ExactSpec "blob")
               (sut/make-spec:user-defined "blob") (sut/->CompoundSpec "foo" [(sut/->AtomSpec)]) sut/DISJOINT
               (sut/make-spec:user-defined "blob") (sut/->ListSpec (sut/->AtomicSpec)) sut/DISJOINT
               (sut/make-spec:user-defined "blob") (sut/->EmptyListSpec) sut/DISJOINT
               (sut/make-spec:user-defined "blob") (sut/->TupleSpec [(sut/make-spec:user-defined "blob")]) sut/DISJOINT
               (sut/make-spec:user-defined "blob") (sut/->GroundSpec) (sut/->UserDefinedSpec "blob") ;;TODO: make consistent with Nonvar
               (sut/make-spec:user-defined "blob") (sut/->NonvarSpec) (sut/->ExactSpec "blob")
               (sut/make-spec:user-defined "blob") (sut/->AnySpec) (sut/make-spec:user-defined "blob") ;;TODO: clarify behaviour
               (sut/make-spec:user-defined "blob") (sut/->VarSpec) sut/DISJOINT

               (sut/make-spec:user-defined "atomOrInt") (sut/->IntegerSpec) (sut/->IntegerSpec)
               (sut/make-spec:user-defined "atomOrInt") (sut/->FloatSpec) sut/DISJOINT
               (sut/make-spec:user-defined "atomOrInt") (sut/->NumberSpec) (sut/->IntegerSpec)
               (sut/make-spec:user-defined "atomOrInt") (sut/->ExactSpec "cake") (sut/->ExactSpec "cake")
               (sut/make-spec:user-defined "atomOrInt") (sut/->AtomSpec) (sut/->AtomSpec)
               (sut/make-spec:user-defined "atomOrInt") (sut/->AtomicSpec) (sut/->OneOfSpec [(sut/->IntegerSpec) (sut/->AtomSpec)])
               (sut/make-spec:user-defined "atomOrInt") (sut/->CompoundSpec "foo" [(sut/->AtomSpec)]) sut/DISJOINT
               (sut/make-spec:user-defined "atomOrInt") (sut/->ListSpec (sut/->AtomicSpec)) sut/DISJOINT
               (sut/make-spec:user-defined "atomOrInt") (sut/->EmptyListSpec) sut/DISJOINT
               (sut/make-spec:user-defined "atomOrInt") (sut/->TupleSpec [(sut/make-spec:user-defined "atomOrInt")]) sut/DISJOINT
               (sut/make-spec:user-defined "atomOrInt") (sut/->GroundSpec) (sut/make-spec:user-defined "atomOrInt")
               (sut/make-spec:user-defined "atomOrInt") (sut/->NonvarSpec) (sut/->OneOfSpec [(sut/->IntegerSpec) (sut/->AtomSpec)])
               (sut/make-spec:user-defined "atomOrInt") (sut/->AnySpec) (sut/make-spec:user-defined "atomOrInt") ;;TODO: clarify behaviour
               (sut/make-spec:user-defined "atomOrInt") (sut/->VarSpec) sut/DISJOINT
               (sut/make-spec:user-defined "atomOrInt") (sut/make-spec:user-defined "blob") (sut/->ExactSpec "blob")
               (sut/make-spec:user-defined "atomOrInt") (sut/make-spec:user-defined "atomOrInt") (sut/make-spec:user-defined "atomOrInt") ;;TODO: clarify behaviour

               ;; intersect One-of with One-of
               (sut/->OneOfSpec [(sut/->IntegerSpec) (sut/->AtomSpec)]) (sut/->OneOfSpec [(sut/->IntegerSpec) (sut/->AtomSpec)]) (sut/->OneOfSpec [(sut/->IntegerSpec) (sut/->AtomSpec)])
               (sut/->OneOfSpec [(sut/->IntegerSpec) (sut/->AtomSpec)]) (sut/->OneOfSpec [(sut/->NumberSpec) (sut/->AtomSpec)]) (sut/->OneOfSpec [(sut/->IntegerSpec) (sut/->AtomSpec)])
               (sut/->OneOfSpec [(sut/->IntegerSpec) (sut/->AtomSpec)]) (sut/->IntegerSpec) (sut/->IntegerSpec)
               (sut/->OneOfSpec [(sut/->NumberSpec) (sut/->AtomSpec)]) (sut/->OneOfSpec [(sut/->EmptyListSpec) (sut/->FloatSpec)]) (sut/->FloatSpec)

               ;; intersect One-of with And
               (sut/->OneOfSpec [(sut/->IntegerSpec) (sut/->AtomSpec)]) (sut/->AndSpec [(sut/->ListSpec (sut/->IntegerSpec)) (sut/->AtomicSpec)]) sut/DISJOINT
               (sut/->OneOfSpec [(sut/->ListSpec (sut/->AtomSpec)) (sut/->IntegerSpec)]) (sut/->AndSpec [(sut/->ListSpec (sut/->IntegerSpec)) (sut/->AtomicSpec)]) (sut/->EmptyListSpec)

               ;; intersect And with One-of
               (sut/->AndSpec [(sut/->ListSpec (sut/->IntegerSpec)) (sut/->AtomicSpec)]) (sut/->OneOfSpec [(sut/->IntegerSpec) (sut/->AtomSpec)]) sut/DISJOINT
               (sut/->AndSpec [(sut/->ListSpec (sut/->IntegerSpec)) (sut/->AtomicSpec)]) (sut/->OneOfSpec [(sut/->ListSpec (sut/->AtomSpec)) (sut/->IntegerSpec)]) (sut/->EmptyListSpec)
               (sut/->AndSpec [(sut/->ListSpec (sut/->IntegerSpec)) (sut/->AtomicSpec)]) (sut/->OneOfSpec [(sut/->EmptyListSpec) (sut/->AtomSpec)]) (sut/->EmptyListSpec)

               ;; intersect And with And
               (sut/->AndSpec [(sut/->IntegerSpec) (sut/->GroundSpec)]) (sut/->AndSpec [(sut/->IntegerSpec) (sut/->GroundSpec)]) (sut/->IntegerSpec)
               (sut/->AndSpec [(sut/->IntegerSpec) (sut/->GroundSpec)]) (sut/->AndSpec [(sut/->NumberSpec)]) (sut/->IntegerSpec)


               (sut/->AndSpec [(sut/->ListSpec (sut/->IntegerSpec)) (sut/->AtomicSpec)]) (sut/->EmptyListSpec) (sut/->EmptyListSpec)
               ))

(def tree-x (sut/make-spec:user-defined "tree" [(sut/->SpecvarSpec "X")]))
(deftest intersecting-user-defs
  (do-template [spec1 spec2 result] (is (= result (sut/intersect spec1 spec2 test-defs)) (clojure.string/join " " (map sut/to-string [spec1 spec2])))
               tree-x tree-x tree-x
               (sut/make-spec:user-defined "a") (sut/make-spec:user-defined "a") (sut/make-spec:user-defined "a")))

(deftest intersect-ground-with-userdef
  (do-template [in out] (do (is (= out (sut/intersect (sut/->GroundSpec) in test-defs)))
                            (is (= out (sut/intersect in (sut/->GroundSpec) test-defs))))
               expr expr
               ))

(deftest simplify-or
  (are [in out] (= out (sut/simplify-or (sut/->OneOfSpec in) test-defs))
    [(sut/->AtomSpec) (sut/->AtomSpec)] (sut/->AtomSpec)
    [(sut/->NumberSpec) (sut/->AtomSpec) (sut/->AtomicSpec)] (sut/->AtomicSpec)
    [(sut/->IntegerSpec) (sut/->AtomSpec)] (sut/->OneOfSpec [(sut/->IntegerSpec) (sut/->AtomSpec)])
    [tree-x] tree-x
    [(sut/make-spec:user-defined "tree" [(sut/->IntegerSpec)]) (sut/make-spec:user-defined "tree" [(sut/->NumberSpec)])] (sut/make-spec:user-defined "tree" [(sut/->NumberSpec)])
    [(sut/->AtomSpec) (sut/->OneOfSpec [(sut/->IntegerSpec) (sut/->FloatSpec)])] (sut/->OneOfSpec [(sut/->AtomSpec) (sut/->IntegerSpec) (sut/->FloatSpec)])
    ))

(deftest simplify-and
  (are [in out] (= out (sut/simplify-and (sut/->AndSpec in) test-defs true))

    [tree-x (sut/make-spec:user-defined "tree" [(sut/->IntegerSpec)])] (sut/make-spec:user-defined "tree" [(sut/->AndSpec [(sut/->IntegerSpec) (sut/->SpecvarSpec "X")])])
    [(sut/->SpecvarSpec "X") (sut/->IntegerSpec)] (sut/->AndSpec [(sut/->IntegerSpec) (sut/->SpecvarSpec "X")])
    [(sut/->IntegerSpec)] (sut/->IntegerSpec)))
