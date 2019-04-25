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

   (sut/make-spec:user-defined "atomOrVar")
   (sut/->OneOfSpec [(sut/->AtomSpec) (sut/->VarSpec)])

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
    (sut/->ListTerm (sut/->AnonVarTerm "_123") (sut/->AnonVarTerm "_124")) :inf
    (sut/->ListTerm (sut/->IntegerTerm 1) (sut/->ListTerm (sut/->VarTerm "H") (sut/->VarTerm "T"))) :inf))


(deftest intersect-pre-spec
    (are [a b c] (and
                  (= c (sut/intersect-pre-spec test-defs a b))
                  (= c (sut/intersect-pre-spec test-defs a b))
                  )
      (sut/->IntegerSpec)                (sut/->IntegerSpec)                                (sut/->IntegerSpec)
      (sut/->IntegerSpec)                (sut/->FloatSpec)                                  sut/DISJOINT
      (sut/->IntegerSpec)                (sut/->NumberSpec)                                 (sut/->IntegerSpec)
      (sut/->IntegerSpec)                (sut/->ExactSpec "cake")                           sut/DISJOINT
      (sut/->IntegerSpec)                (sut/->AtomSpec)                                   sut/DISJOINT
      (sut/->IntegerSpec)                (sut/->StringSpec)                                 sut/DISJOINT
      (sut/->IntegerSpec)                (sut/->AtomicSpec)                                 (sut/->IntegerSpec)
      (sut/->IntegerSpec)                (sut/->CompoundSpec nil '())                       sut/DISJOINT
      (sut/->IntegerSpec)                (sut/->CompoundSpec "foo" [(sut/->FloatSpec)])     sut/DISJOINT
      (sut/->IntegerSpec)                (sut/->ListSpec (sut/->FloatSpec))                 sut/DISJOINT
      (sut/->IntegerSpec)                (sut/->ListSpec (sut/->FloatSpec))                 sut/DISJOINT
      (sut/->IntegerSpec)                (sut/->EmptyListSpec)                              sut/DISJOINT
      (sut/->IntegerSpec)                (sut/->TupleSpec [(sut/->AtomSpec)])               sut/DISJOINT
      (sut/->IntegerSpec)                (sut/->GroundSpec)                                 (sut/->IntegerSpec)
      (sut/->IntegerSpec)                (sut/->NonvarSpec)                                 (sut/->IntegerSpec)
      (sut/->IntegerSpec)                (sut/->VarSpec)                                    sut/DISJOINT
     ; (sut/->IntegerSpec)                (sut/make-spec:user-defined "atomOrInt")           (sut/->IntegerSpec)
     ; (sut/->IntegerSpec)                (sut/make-spec:user-defined "atomOrVar")           sut/DISJOINT
      (sut/->IntegerSpec)                (sut/->SpecvarSpec "X")                            (sut/->IntegerSpec)
      (sut/->IntegerSpec)                (sut/->AnySpec)                                    (sut/->IntegerSpec)

      (sut/->FloatSpec)                (sut/->FloatSpec)                                  (sut/->FloatSpec)
      (sut/->FloatSpec)                (sut/->NumberSpec)                                 (sut/->FloatSpec)
      (sut/->FloatSpec)                (sut/->ExactSpec "cake")                           sut/DISJOINT
      (sut/->FloatSpec)                (sut/->AtomSpec)                                   sut/DISJOINT
      (sut/->FloatSpec)                (sut/->StringSpec)                                 sut/DISJOINT
      (sut/->FloatSpec)                (sut/->AtomicSpec)                                 (sut/->FloatSpec)
      (sut/->FloatSpec)                (sut/->CompoundSpec nil '())                       sut/DISJOINT
      (sut/->FloatSpec)                (sut/->CompoundSpec "foo" [(sut/->FloatSpec)])     sut/DISJOINT
      (sut/->FloatSpec)                (sut/->ListSpec (sut/->FloatSpec))                 sut/DISJOINT
      (sut/->FloatSpec)                (sut/->ListSpec (sut/->FloatSpec))                 sut/DISJOINT
      (sut/->FloatSpec)                (sut/->EmptyListSpec)                              sut/DISJOINT
      (sut/->FloatSpec)                (sut/->TupleSpec [(sut/->AtomSpec)])               sut/DISJOINT
      (sut/->FloatSpec)                (sut/->GroundSpec)                                 (sut/->FloatSpec)
      (sut/->FloatSpec)                (sut/->NonvarSpec)                                 (sut/->FloatSpec)
      (sut/->FloatSpec)                (sut/->VarSpec)                                    sut/DISJOINT
     ; (sut/->FloatSpec)                (sut/make-spec:user-defined "atomOrInt")           sut/DISJOINT
     ; (sut/->FloatSpec)                (sut/make-spec:user-defined "atomOrVar")           sut/DISJOINT
      (sut/->FloatSpec)                (sut/->SpecvarSpec "X")                            (sut/->FloatSpec)
      (sut/->FloatSpec)                (sut/->AnySpec)                                    (sut/->FloatSpec)

      (sut/->NumberSpec)                (sut/->NumberSpec)                                 (sut/->NumberSpec)
      (sut/->NumberSpec)                (sut/->ExactSpec "cake")                           sut/DISJOINT
      (sut/->NumberSpec)                (sut/->AtomSpec)                                   sut/DISJOINT
      (sut/->NumberSpec)                (sut/->StringSpec)                                 sut/DISJOINT
      (sut/->NumberSpec)                (sut/->AtomicSpec)                                 (sut/->NumberSpec)
      (sut/->NumberSpec)                (sut/->CompoundSpec nil '())                       sut/DISJOINT
      (sut/->NumberSpec)                (sut/->CompoundSpec "foo" [(sut/->FloatSpec)])     sut/DISJOINT
      (sut/->NumberSpec)                (sut/->ListSpec (sut/->FloatSpec))                 sut/DISJOINT
      (sut/->NumberSpec)                (sut/->ListSpec (sut/->FloatSpec))                 sut/DISJOINT
      (sut/->NumberSpec)                (sut/->EmptyListSpec)                              sut/DISJOINT
      (sut/->NumberSpec)                (sut/->TupleSpec [(sut/->AtomSpec)])               sut/DISJOINT
      (sut/->NumberSpec)                (sut/->GroundSpec)                                 (sut/->NumberSpec)
      (sut/->NumberSpec)                (sut/->NonvarSpec)                                 (sut/->NumberSpec)
      (sut/->NumberSpec)                (sut/->VarSpec)                                    sut/DISJOINT
     ; (sut/->NumberSpec)                (sut/make-spec:user-defined "atomOrInt")           (sut/->IntegerSpec)
      ;(sut/->NumberSpec)                (sut/make-spec:user-defined "atomOrVar")           sut/DISJOINT
     (sut/->NumberSpec)                (sut/->SpecvarSpec "X")                            (sut/->NumberSpec)
      (sut/->NumberSpec)                (sut/->AnySpec)                                    (sut/->NumberSpec)


      (sut/->ExactSpec "cake")                (sut/->ExactSpec "cake")                           (sut/->ExactSpec "cake")
      (sut/->ExactSpec "cake")                (sut/->AtomSpec)                                   (sut/->ExactSpec "cake")
      (sut/->ExactSpec "cake")                (sut/->StringSpec)                                 sut/DISJOINT
      (sut/->ExactSpec "cake")                (sut/->AtomicSpec)                                 (sut/->ExactSpec "cake")
      (sut/->ExactSpec "cake")                (sut/->CompoundSpec nil '())                       sut/DISJOINT
      (sut/->ExactSpec "cake")                (sut/->CompoundSpec "foo" [(sut/->FloatSpec)])     sut/DISJOINT
      (sut/->ExactSpec "cake")                (sut/->ListSpec (sut/->FloatSpec))                 sut/DISJOINT
      (sut/->ExactSpec "cake")                (sut/->ListSpec (sut/->FloatSpec))                 sut/DISJOINT
      (sut/->ExactSpec "cake")                (sut/->EmptyListSpec)                              sut/DISJOINT
      (sut/->ExactSpec "cake")                (sut/->TupleSpec [(sut/->AtomSpec)])               sut/DISJOINT
      (sut/->ExactSpec "cake")                (sut/->GroundSpec)                                 (sut/->ExactSpec "cake")
      (sut/->ExactSpec "cake")                (sut/->NonvarSpec)                                 (sut/->ExactSpec "cake")
      (sut/->ExactSpec "cake")                (sut/->VarSpec)                                    sut/DISJOINT
     ; (sut/->ExactSpec "cake")                (sut/make-spec:user-defined "atomOrInt")           (sut/->ExactSpec "cake")
     ; (sut/->ExactSpec "cake")                (sut/make-spec:user-defined "atomOrVar")           (sut/->ExactSpec "cake")
      (sut/->ExactSpec "cake")                (sut/->SpecvarSpec "X")                            (sut/->ExactSpec "cake")
      (sut/->ExactSpec "cake")                (sut/->AnySpec)                                    (sut/->ExactSpec "cake")


      (sut/->AtomSpec)                (sut/->AtomSpec)                                   (sut/->AtomSpec)
      (sut/->AtomSpec)                (sut/->StringSpec)                                 sut/DISJOINT
      (sut/->AtomSpec)                (sut/->AtomicSpec)                                 (sut/->AtomSpec)
      (sut/->AtomSpec)                (sut/->CompoundSpec nil '())                       sut/DISJOINT
      (sut/->AtomSpec)                (sut/->CompoundSpec "foo" [(sut/->FloatSpec)])     sut/DISJOINT
      (sut/->AtomSpec)                (sut/->ListSpec (sut/->FloatSpec))                 sut/DISJOINT
      (sut/->AtomSpec)                (sut/->ListSpec (sut/->FloatSpec))                 sut/DISJOINT
      (sut/->AtomSpec)                (sut/->EmptyListSpec)                              sut/DISJOINT
      (sut/->AtomSpec)                (sut/->TupleSpec [(sut/->AtomSpec)])               sut/DISJOINT
      (sut/->AtomSpec)                (sut/->GroundSpec)                                 (sut/->AtomSpec)
      (sut/->AtomSpec)                (sut/->NonvarSpec)                                 (sut/->AtomSpec)
      (sut/->AtomSpec)                (sut/->VarSpec)                                    sut/DISJOINT
      ;(sut/->AtomSpec)                (sut/make-spec:user-defined "atomOrInt")           (sut/->AtomSpec)
     ; (sut/->AtomSpec)                (sut/make-spec:user-defined "atomOrVar")           (sut/->AtomSpec)
      (sut/->AtomSpec)                (sut/->SpecvarSpec "X")                            (sut/->AtomSpec)
      (sut/->AtomSpec)                (sut/->AnySpec)                                    (sut/->AtomSpec)

      (sut/->StringSpec)                (sut/->StringSpec)                                 (sut/->StringSpec)
      (sut/->StringSpec)                (sut/->AtomicSpec)                                 (sut/->StringSpec)
      (sut/->StringSpec)                (sut/->CompoundSpec nil '())                       sut/DISJOINT
      (sut/->StringSpec)                (sut/->CompoundSpec "foo" [(sut/->FloatSpec)])     sut/DISJOINT
      (sut/->StringSpec)                (sut/->ListSpec (sut/->FloatSpec))                 sut/DISJOINT
      (sut/->StringSpec)                (sut/->ListSpec (sut/->FloatSpec))                 sut/DISJOINT
      (sut/->StringSpec)                (sut/->EmptyListSpec)                              sut/DISJOINT
      (sut/->StringSpec)                (sut/->TupleSpec [(sut/->AtomSpec)])               sut/DISJOINT
      (sut/->StringSpec)                (sut/->GroundSpec)                                 (sut/->StringSpec)
      (sut/->StringSpec)                (sut/->NonvarSpec)                                 (sut/->StringSpec)
      (sut/->StringSpec)                (sut/->VarSpec)                                    sut/DISJOINT
     ; (sut/->StringSpec)                (sut/make-spec:user-defined "atomOrInt")           sut/DISJOINT
    ;  (sut/->StringSpec)                (sut/make-spec:user-defined "atomOrVar")           sut/DISJOINT
      (sut/->StringSpec)                (sut/->SpecvarSpec "X")                            (sut/->StringSpec)
      (sut/->StringSpec)                (sut/->AnySpec)                                    (sut/->StringSpec)


      (sut/->AtomicSpec)                (sut/->AtomicSpec)                                 (sut/->AtomicSpec)
      (sut/->AtomicSpec)                (sut/->CompoundSpec nil '())                       sut/DISJOINT
      (sut/->AtomicSpec)                (sut/->CompoundSpec "foo" [(sut/->FloatSpec)])     sut/DISJOINT
      (sut/->AtomicSpec)                (sut/->ListSpec (sut/->FloatSpec))                 (sut/->EmptyListSpec)
      (sut/->AtomicSpec)                (sut/->ListSpec (sut/->FloatSpec))                 (sut/->EmptyListSpec)
      (sut/->AtomicSpec)                (sut/->EmptyListSpec)                              (sut/->EmptyListSpec)
      (sut/->AtomicSpec)                (sut/->TupleSpec [(sut/->AtomSpec)])               sut/DISJOINT
      (sut/->AtomicSpec)                (sut/->GroundSpec)                                 (sut/->AtomicSpec)
      (sut/->AtomicSpec)                (sut/->NonvarSpec)                                 (sut/->AtomicSpec)
      (sut/->AtomicSpec)                (sut/->VarSpec)                                    sut/DISJOINT
    ;  (sut/->AtomicSpec)                (sut/make-spec:user-defined "atomOrInt")           (sut/->OneOfSpec [(sut/->AtomSpec) (sut/->IntegerSpec)])
     ; (sut/->AtomicSpec)                (sut/make-spec:user-defined "atomOrVar")           (sut/->AtomSpec)
      (sut/->AtomicSpec)                (sut/->SpecvarSpec "X")                            (sut/->AtomicSpec)
      (sut/->AtomicSpec)                (sut/->AnySpec)                                    (sut/->AtomicSpec)


      (sut/->CompoundSpec nil '())                (sut/->CompoundSpec nil '())                       (sut/->CompoundSpec nil '())
      (sut/->CompoundSpec nil '())                (sut/->CompoundSpec "foo" [(sut/->FloatSpec)])     (sut/->CompoundSpec "foo" [(sut/->FloatSpec)])
      (sut/->CompoundSpec nil '())                (sut/->ListSpec (sut/->FloatSpec))                 sut/DISJOINT
      (sut/->CompoundSpec nil '())                (sut/->ListSpec (sut/->FloatSpec))                 sut/DISJOINT
      (sut/->CompoundSpec nil '())                (sut/->EmptyListSpec)                              sut/DISJOINT
      (sut/->CompoundSpec nil '())                (sut/->TupleSpec [(sut/->AtomSpec)])               sut/DISJOINT
      (sut/->CompoundSpec nil '())                (sut/->GroundSpec)                                 (sut/->CompoundSpec nil '())
      (sut/->CompoundSpec nil '())                (sut/->NonvarSpec)                                 (sut/->CompoundSpec nil '())
      (sut/->CompoundSpec nil '())                (sut/->VarSpec)                                    sut/DISJOINT
      ;(sut/->CompoundSpec nil '())                (sut/make-spec:user-defined "atomOrInt")           sut/DISJOINT
      ;(sut/->CompoundSpec nil '())                (sut/make-spec:user-defined "atomOrVar")           sut/DISJOINT
      (sut/->CompoundSpec nil '())                (sut/->SpecvarSpec "X")                            (sut/->CompoundSpec nil '())
      (sut/->CompoundSpec nil '())                (sut/->AnySpec)                                    (sut/->CompoundSpec nil '())

      (sut/->CompoundSpec "foo" [(sut/->FloatSpec)])                (sut/->CompoundSpec "foo" [(sut/->FloatSpec)])     (sut/->CompoundSpec "foo" [(sut/->FloatSpec)])
      (sut/->CompoundSpec "foo" [(sut/->FloatSpec)])                (sut/->CompoundSpec "foo" [(sut/->NumberSpec)])     (sut/->CompoundSpec "foo" [(sut/->FloatSpec)])
      (sut/->CompoundSpec "foo" [(sut/->FloatSpec)])                (sut/->ListSpec (sut/->FloatSpec))                 sut/DISJOINT
      (sut/->CompoundSpec "foo" [(sut/->FloatSpec)])                (sut/->ListSpec (sut/->FloatSpec))                 sut/DISJOINT
      (sut/->CompoundSpec "foo" [(sut/->FloatSpec)])                (sut/->EmptyListSpec)                              sut/DISJOINT
      (sut/->CompoundSpec "foo" [(sut/->FloatSpec)])                (sut/->TupleSpec [(sut/->AtomSpec)])               sut/DISJOINT
      (sut/->CompoundSpec "foo" [(sut/->FloatSpec)])                (sut/->GroundSpec)                                 (sut/->CompoundSpec "foo" [(sut/->FloatSpec)])
      (sut/->CompoundSpec "foo" [(sut/->VarSpec)])                  (sut/->GroundSpec)                                 sut/DISJOINT
      (sut/->CompoundSpec "foo" [(sut/->FloatSpec)])                (sut/->NonvarSpec)                                 (sut/->CompoundSpec "foo" [(sut/->FloatSpec)])
      (sut/->CompoundSpec "foo" [(sut/->VarSpec)])                  (sut/->NonvarSpec)                                 (sut/->CompoundSpec "foo" [(sut/->VarSpec)])
      (sut/->CompoundSpec "foo" [(sut/->FloatSpec)])                (sut/->VarSpec)                                    sut/DISJOINT
      ;(sut/->CompoundSpec "foo" [(sut/->FloatSpec)])                (sut/make-spec:user-defined "atomOrInt")           sut/DISJOINT
     ; (sut/->CompoundSpec "foo" [(sut/->FloatSpec)])                (sut/make-spec:user-defined "atomOrVar")           sut/DISJOINT
      (sut/->CompoundSpec "foo" [(sut/->FloatSpec)])                (sut/->SpecvarSpec "X")                            (sut/->CompoundSpec "foo" [(sut/->FloatSpec)])
      (sut/->CompoundSpec "foo" [(sut/->FloatSpec)])                (sut/->AnySpec)                                    (sut/->CompoundSpec "foo" [(sut/->FloatSpec)])


      (sut/->ListSpec (sut/->FloatSpec))                (sut/->ListSpec (sut/->FloatSpec))                 (sut/->ListSpec (sut/->FloatSpec))
      (sut/->ListSpec (sut/->FloatSpec))                (sut/->ListSpec (sut/->NumberSpec))                (sut/->ListSpec (sut/->FloatSpec))
      (sut/->ListSpec (sut/->FloatSpec))                (sut/->ListSpec (sut/->AtomSpec))                  sut/DISJOINT
      (sut/->ListSpec (sut/->FloatSpec))                (sut/->EmptyListSpec)                              (sut/->EmptyListSpec)
      (sut/->ListSpec (sut/->FloatSpec))                (sut/->TupleSpec [(sut/->AtomSpec)])               sut/DISJOINT
      (sut/->ListSpec (sut/->FloatSpec))                (sut/->TupleSpec [(sut/->NumberSpec)])             (sut/->TupleSpec [(sut/->FloatSpec)])
      (sut/->ListSpec (sut/->FloatSpec))                (sut/->GroundSpec)                                 (sut/->ListSpec (sut/->FloatSpec))
      (sut/->ListSpec (sut/->VarSpec))                  (sut/->GroundSpec)                                 sut/DISJOINT
      (sut/->ListSpec (sut/->FloatSpec))                (sut/->NonvarSpec)                                 (sut/->ListSpec (sut/->FloatSpec))
      (sut/->ListSpec (sut/->FloatSpec))                (sut/->VarSpec)                                    sut/DISJOINT
      ;(sut/->ListSpec (sut/->FloatSpec))                (sut/make-spec:user-defined "atomOrInt")           sut/DISJOINT
     ; (sut/->ListSpec (sut/->FloatSpec))                (sut/make-spec:user-defined "atomOrVar")           sut/DISJOINT
      (sut/->ListSpec (sut/->FloatSpec))                (sut/->SpecvarSpec "X")                            (sut/->ListSpec (sut/->FloatSpec))
      (sut/->ListSpec (sut/->FloatSpec))                (sut/->AnySpec)                                    (sut/->ListSpec (sut/->FloatSpec))

      (sut/->EmptyListSpec)                (sut/->EmptyListSpec)                              (sut/->EmptyListSpec)
      (sut/->EmptyListSpec)                (sut/->TupleSpec [(sut/->AtomSpec)])               sut/DISJOINT
      (sut/->EmptyListSpec)                (sut/->GroundSpec)                                 (sut/->EmptyListSpec)
      (sut/->EmptyListSpec)                (sut/->NonvarSpec)                                 (sut/->EmptyListSpec)
      (sut/->EmptyListSpec)                (sut/->VarSpec)                                    sut/DISJOINT
      ;(sut/->EmptyListSpec)                (sut/make-spec:user-defined "atomOrInt")           sut/DISJOINT
      ;(sut/->EmptyListSpec)                (sut/make-spec:user-defined "atomOrVar")           sut/DISJOINT
      (sut/->EmptyListSpec)                (sut/->SpecvarSpec "X")                            (sut/->EmptyListSpec)
      (sut/->EmptyListSpec)                (sut/->AnySpec)                                    (sut/->EmptyListSpec)


      (sut/->TupleSpec [(sut/->AtomSpec)])                (sut/->TupleSpec [(sut/->AtomSpec)])               (sut/->TupleSpec [(sut/->AtomSpec)])
      (sut/->TupleSpec [(sut/->AtomSpec)])                (sut/->TupleSpec [(sut/->FloatSpec)])              sut/DISJOINT
      (sut/->TupleSpec [(sut/->AtomSpec)])                (sut/->GroundSpec)                                 (sut/->TupleSpec [(sut/->AtomSpec)])
      (sut/->TupleSpec [(sut/->VarSpec)])                 (sut/->GroundSpec)                                 sut/DISJOINT
      (sut/->TupleSpec [(sut/->AtomSpec)])                (sut/->NonvarSpec)                                 (sut/->TupleSpec [(sut/->AtomSpec)])
      (sut/->TupleSpec [(sut/->AtomSpec)])                (sut/->VarSpec)                                    sut/DISJOINT
     ; (sut/->TupleSpec [(sut/->AtomSpec)])                (sut/make-spec:user-defined "atomOrInt")           sut/DISJOINT
     ; (sut/->TupleSpec [(sut/->AtomSpec)])                (sut/make-spec:user-defined "atomOrVar")           sut/DISJOINT
      (sut/->TupleSpec [(sut/->AtomSpec)])                (sut/->SpecvarSpec "X")                            (sut/->TupleSpec [(sut/->AtomSpec)])
      (sut/->TupleSpec [(sut/->AtomSpec)])                (sut/->AnySpec)                                    (sut/->TupleSpec [(sut/->AtomSpec)])

      (sut/->GroundSpec)                (sut/->GroundSpec)                                 (sut/->GroundSpec)
      (sut/->GroundSpec)                (sut/->NonvarSpec)                                 (sut/->GroundSpec)
      (sut/->GroundSpec)                (sut/->VarSpec)                                    sut/DISJOINT
    ;  (sut/->GroundSpec)                (sut/make-spec:user-defined "atomOrInt")           (sut/->OneOfSpec [(sut/->AtomSpec) (sut/->IntegerSpec)])
     ; (sut/->GroundSpec)                (sut/make-spec:user-defined "atomOrVar")           (sut/->AtomSpec)
      (sut/->GroundSpec)                (sut/->SpecvarSpec "X")                            (sut/->GroundSpec)
      (sut/->GroundSpec)                (sut/->AnySpec)                                    (sut/->GroundSpec)

      (sut/->NonvarSpec)                (sut/->NonvarSpec)                                 (sut/->NonvarSpec)
      (sut/->NonvarSpec)                (sut/->VarSpec)                                    sut/DISJOINT
     ; (sut/->NonvarSpec)                (sut/make-spec:user-defined "atomOrInt")           (sut/->OneOfSpec [(sut/->AtomSpec) (sut/->IntegerSpec)])
     ; (sut/->NonvarSpec)                (sut/make-spec:user-defined "atomOrVar")           (sut/->OneOfSpec [(sut/->AtomSpec) (sut/->VarSpec)])
      (sut/->NonvarSpec)                (sut/->SpecvarSpec "X")                            (sut/->NonvarSpec)
      (sut/->NonvarSpec)                (sut/->AnySpec)                                    (sut/->NonvarSpec)


      (sut/->VarSpec)                (sut/->VarSpec)                                    (sut/->VarSpec)
   ;   (sut/->VarSpec)                (sut/make-spec:user-defined "atomOrInt")           sut/DISJOINT
    ;  (sut/->VarSpec)                (sut/make-spec:user-defined "atomOrVar")           (sut/->VarSpec)
      (sut/->VarSpec)                (sut/->SpecvarSpec "X")                            (sut/->VarSpec)
      (sut/->VarSpec)                (sut/->AnySpec)                                    (sut/->VarSpec)

     ; (sut/make-spec:user-defined "atomOrInt")                (sut/make-spec:user-defined "atomOrInt")           (sut/->OneOfSpec [(sut/->AtomSpec) (sut/->IntegerSpec)])
      ;(sut/make-spec:user-defined "atomOrInt")                (sut/make-spec:user-defined "atomOrVar")           (sut/->AtomSpec)
      ;(sut/make-spec:user-defined "atomOrInt")                (sut/->SpecvarSpec "X")                            (sut/->OneOfSpec [(sut/->AtomSpec) (sut/->IntegerSpec)])
      ;(sut/make-spec:user-defined "atomOrInt")                (sut/->AnySpec)                                    (sut/->OneOfSpec [(sut/->AtomSpec) (sut/->IntegerSpec)])

      (sut/->SpecvarSpec "Y")                (sut/->SpecvarSpec "Y")                            (sut/->AnySpec)
      (sut/->SpecvarSpec "Y")                (sut/->SpecvarSpec "X")                            (sut/->AnySpec)
      (sut/->SpecvarSpec "Y")                (sut/->AnySpec)                                    (sut/->AnySpec)

      (sut/->AnySpec)                (sut/->AnySpec)                                    (sut/->AnySpec)


      ))
