(ns prolog-analyzer.record-utils-test
  (:require [prolog-analyzer.record-utils :as sut]
            [prolog-analyzer.records :as r]
            [clojure.test :refer [are deftest]]))

(def expr (r/->UserDefinedSpec "expr"))
(def op (r/->UserDefinedSpec "op"))
(def cst (r/->UserDefinedSpec "cst"))


(def test-defs
  {(r/make-spec:user-defined "tree" [(r/->SpecvarSpec "X")])
   (r/->OneOfSpec #{(r/->CompoundSpec "node" [(r/make-spec:user-defined "tree" [(r/->SpecvarSpec "X")]) (r/->SpecvarSpec "X") (r/make-spec:user-defined "tree" [(r/->SpecvarSpec "X")])]) (r/->ExactSpec "empty")})

   (r/make-spec:user-defined "atomOrInt")
   (r/->OneOfSpec #{(r/->IntegerSpec) (r/->AtomSpec)})

   (r/make-spec:user-defined "atomOrVar")
   (r/->OneOfSpec #{(r/->AtomSpec) (r/->VarSpec)})

   (r/make-spec:user-defined "blob")
   (r/->ExactSpec "blob")

   (r/make-spec:user-defined "a")
   (r/->OneOfSpec #{(r/->TupleSpec [(r/make-spec:user-defined "a")]) (r/->ExactSpec "a")})

   expr
   (r/->OneOfSpec #{cst, (r/->CompoundSpec "expr" [op, expr, expr]), (r/->CompoundSpec "neg" [expr])})

   op (r/->ExactSpec "+")
   cst (r/->CompoundSpec "cst" [(r/->IntegerSpec)])


   })


(deftest intersect-pre-spec
  (are [a b c] (if (= c :error)
                 (and
                  (r/error-spec? (sut/intersect a b test-defs))
                  (r/error-spec? (sut/intersect b a test-defs)))
                 (and
                  (= c (sut/intersect a b test-defs))
                  (= c (sut/intersect b a test-defs))))
      (r/->IntegerSpec)                (r/->IntegerSpec)                                (r/->IntegerSpec)
      (r/->IntegerSpec)                (r/->FloatSpec)                                  :error
      (r/->IntegerSpec)                (r/->NumberSpec)                                 (r/->IntegerSpec)
      (r/->IntegerSpec)                (r/->ExactSpec "cake")                           :error
      (r/->IntegerSpec)                (r/->AtomSpec)                                   :error
      (r/->IntegerSpec)                (r/->StringSpec)                                 :error
      (r/->IntegerSpec)                (r/->AtomicSpec)                                 (r/->IntegerSpec)
      (r/->IntegerSpec)                (r/->CompoundSpec nil '())                       :error
      (r/->IntegerSpec)                (r/->CompoundSpec "foo" [(r/->FloatSpec)])     :error
      (r/->IntegerSpec)                (r/->ListSpec (r/->FloatSpec))                 :error
      (r/->IntegerSpec)                (r/->ListSpec (r/->FloatSpec))                 :error
      (r/->IntegerSpec)                (r/->EmptyListSpec)                              :error
      (r/->IntegerSpec)                (r/->TupleSpec [(r/->AtomSpec)])               :error
      (r/->IntegerSpec)                (r/->GroundSpec)                                 (r/->IntegerSpec)
      (r/->IntegerSpec)                (r/->NonvarSpec)                                 (r/->IntegerSpec)
      (r/->IntegerSpec)                (r/->VarSpec)                                    :error
      (r/->IntegerSpec)                (r/make-spec:user-defined "atomOrInt")           (r/->IntegerSpec)
      (r/->IntegerSpec)                (r/make-spec:user-defined "atomOrVar")           :error
      (r/->IntegerSpec)                (r/->AnySpec)                                    (r/->IntegerSpec)

      (r/->FloatSpec)                (r/->FloatSpec)                                  (r/->FloatSpec)
      (r/->FloatSpec)                (r/->NumberSpec)                                 (r/->FloatSpec)
      (r/->FloatSpec)                (r/->ExactSpec "cake")                           :error
      (r/->FloatSpec)                (r/->AtomSpec)                                   :error
      (r/->FloatSpec)                (r/->StringSpec)                                 :error
      (r/->FloatSpec)                (r/->AtomicSpec)                                 (r/->FloatSpec)
      (r/->FloatSpec)                (r/->CompoundSpec nil '())                       :error
      (r/->FloatSpec)                (r/->CompoundSpec "foo" [(r/->FloatSpec)])     :error
      (r/->FloatSpec)                (r/->ListSpec (r/->FloatSpec))                 :error
      (r/->FloatSpec)                (r/->ListSpec (r/->FloatSpec))                 :error
      (r/->FloatSpec)                (r/->EmptyListSpec)                              :error
      (r/->FloatSpec)                (r/->TupleSpec [(r/->AtomSpec)])               :error
      (r/->FloatSpec)                (r/->GroundSpec)                                 (r/->FloatSpec)
      (r/->FloatSpec)                (r/->NonvarSpec)                                 (r/->FloatSpec)
      (r/->FloatSpec)                (r/->VarSpec)                                    :error
      (r/->FloatSpec)                (r/make-spec:user-defined "atomOrInt")           :error
      (r/->FloatSpec)                (r/make-spec:user-defined "atomOrVar")           :error
      (r/->FloatSpec)                (r/->AnySpec)                                    (r/->FloatSpec)

      (r/->NumberSpec)                (r/->NumberSpec)                                 (r/->NumberSpec)
      (r/->NumberSpec)                (r/->ExactSpec "cake")                           :error
      (r/->NumberSpec)                (r/->AtomSpec)                                   :error
      (r/->NumberSpec)                (r/->StringSpec)                                 :error
      (r/->NumberSpec)                (r/->AtomicSpec)                                 (r/->NumberSpec)
      (r/->NumberSpec)                (r/->CompoundSpec nil '())                       :error
      (r/->NumberSpec)                (r/->CompoundSpec "foo" [(r/->FloatSpec)])     :error
      (r/->NumberSpec)                (r/->ListSpec (r/->FloatSpec))                 :error
      (r/->NumberSpec)                (r/->ListSpec (r/->FloatSpec))                 :error
      (r/->NumberSpec)                (r/->EmptyListSpec)                              :error
      (r/->NumberSpec)                (r/->TupleSpec [(r/->AtomSpec)])               :error
      (r/->NumberSpec)                (r/->GroundSpec)                                 (r/->NumberSpec)
      (r/->NumberSpec)                (r/->NonvarSpec)                                 (r/->NumberSpec)
      (r/->NumberSpec)                (r/->VarSpec)                                    :error
      (r/->NumberSpec)                (r/make-spec:user-defined "atomOrInt")           (r/->IntegerSpec)
      (r/->NumberSpec)                (r/make-spec:user-defined "atomOrVar")           :error
      (r/->NumberSpec)                (r/->AnySpec)                                    (r/->NumberSpec)


      (r/->ExactSpec "cake")                (r/->ExactSpec "cake")                           (r/->ExactSpec "cake")
      (r/->ExactSpec "cake")                (r/->AtomSpec)                                   (r/->ExactSpec "cake")
      (r/->ExactSpec "cake")                (r/->StringSpec)                                 :error
      (r/->ExactSpec "cake")                (r/->AtomicSpec)                                 (r/->ExactSpec "cake")
      (r/->ExactSpec "cake")                (r/->CompoundSpec nil '())                       :error
      (r/->ExactSpec "cake")                (r/->CompoundSpec "foo" [(r/->FloatSpec)])     :error
      (r/->ExactSpec "cake")                (r/->ListSpec (r/->FloatSpec))                 :error
      (r/->ExactSpec "cake")                (r/->ListSpec (r/->FloatSpec))                 :error
      (r/->ExactSpec "cake")                (r/->EmptyListSpec)                              :error
      (r/->ExactSpec "cake")                (r/->TupleSpec [(r/->AtomSpec)])               :error
      (r/->ExactSpec "cake")                (r/->GroundSpec)                                 (r/->ExactSpec "cake")
      (r/->ExactSpec "cake")                (r/->NonvarSpec)                                 (r/->ExactSpec "cake")
      (r/->ExactSpec "cake")                (r/->VarSpec)                                    :error
      (r/->ExactSpec "cake")                (r/make-spec:user-defined "atomOrInt")           (r/->ExactSpec "cake")
      (r/->ExactSpec "cake")                (r/make-spec:user-defined "atomOrVar")           (r/->ExactSpec "cake")
      (r/->ExactSpec "cake")                (r/->AnySpec)                                    (r/->ExactSpec "cake")


      (r/->AtomSpec)                (r/->AtomSpec)                                   (r/->AtomSpec)
      (r/->AtomSpec)                (r/->StringSpec)                                 :error
      (r/->AtomSpec)                (r/->AtomicSpec)                                 (r/->AtomSpec)
      (r/->AtomSpec)                (r/->CompoundSpec nil '())                       :error
      (r/->AtomSpec)                (r/->CompoundSpec "foo" [(r/->FloatSpec)])     :error
      (r/->AtomSpec)                (r/->ListSpec (r/->FloatSpec))                 :error
      (r/->AtomSpec)                (r/->ListSpec (r/->FloatSpec))                 :error
      (r/->AtomSpec)                (r/->EmptyListSpec)                              :error
      (r/->AtomSpec)                (r/->TupleSpec [(r/->AtomSpec)])               :error
      (r/->AtomSpec)                (r/->GroundSpec)                                 (r/->AtomSpec)
      (r/->AtomSpec)                (r/->NonvarSpec)                                 (r/->AtomSpec)
      (r/->AtomSpec)                (r/->VarSpec)                                    :error
      (r/->AtomSpec)                (r/make-spec:user-defined "atomOrInt")           (r/->AtomSpec)
      (r/->AtomSpec)                (r/make-spec:user-defined "atomOrVar")           (r/->AtomSpec)
      (r/->AtomSpec)                (r/->AnySpec)                                    (r/->AtomSpec)

      (r/->StringSpec)                (r/->StringSpec)                                 (r/->StringSpec)
      (r/->StringSpec)                (r/->AtomicSpec)                                 (r/->StringSpec)
      (r/->StringSpec)                (r/->CompoundSpec nil '())                       :error
      (r/->StringSpec)                (r/->CompoundSpec "foo" [(r/->FloatSpec)])     :error
      (r/->StringSpec)                (r/->ListSpec (r/->FloatSpec))                 :error
      (r/->StringSpec)                (r/->ListSpec (r/->FloatSpec))                 :error
      (r/->StringSpec)                (r/->EmptyListSpec)                              :error
      (r/->StringSpec)                (r/->TupleSpec [(r/->AtomSpec)])               :error
      (r/->StringSpec)                (r/->GroundSpec)                                 (r/->StringSpec)
      (r/->StringSpec)                (r/->NonvarSpec)                                 (r/->StringSpec)
      (r/->StringSpec)                (r/->VarSpec)                                    :error
      (r/->StringSpec)                (r/make-spec:user-defined "atomOrInt")           :error
      (r/->StringSpec)                (r/make-spec:user-defined "atomOrVar")           :error
      (r/->StringSpec)                (r/->AnySpec)                                    (r/->StringSpec)


      (r/->AtomicSpec)                (r/->AtomicSpec)                                 (r/->AtomicSpec)
      (r/->AtomicSpec)                (r/->CompoundSpec nil '())                       :error
      (r/->AtomicSpec)                (r/->CompoundSpec "foo" [(r/->FloatSpec)])     :error
      (r/->AtomicSpec)                (r/->ListSpec (r/->FloatSpec))                 (r/->EmptyListSpec)
      (r/->AtomicSpec)                (r/->ListSpec (r/->FloatSpec))                 (r/->EmptyListSpec)
      (r/->AtomicSpec)                (r/->EmptyListSpec)                              (r/->EmptyListSpec)
      (r/->AtomicSpec)                (r/->TupleSpec [(r/->AtomSpec)])               :error
      (r/->AtomicSpec)                (r/->GroundSpec)                                 (r/->AtomicSpec)
      (r/->AtomicSpec)                (r/->NonvarSpec)                                 (r/->AtomicSpec)
      (r/->AtomicSpec)                (r/->VarSpec)                                    :error
      (r/->AtomicSpec)                (r/make-spec:user-defined "atomOrInt")           (r/->OneOfSpec #{(r/->IntegerSpec) (r/->AtomSpec)})
      (r/->AtomicSpec)                (r/make-spec:user-defined "atomOrVar")           (r/->AtomSpec)
      (r/->AtomicSpec)                (r/->AnySpec)                                    (r/->AtomicSpec)


      (r/->CompoundSpec nil '())                (r/->CompoundSpec nil '())                       (r/->CompoundSpec nil '())
      (r/->CompoundSpec nil '())                (r/->CompoundSpec "foo" [(r/->FloatSpec)])     (r/->CompoundSpec "foo" [(r/->FloatSpec)])
      (r/->CompoundSpec nil nil)                (r/->ListSpec (r/->FloatSpec))                 (r/->ListSpec (r/->FloatSpec))
      (r/->CompoundSpec nil nil)                (r/->EmptyListSpec)                              (r/->EmptyListSpec)
      (r/->CompoundSpec nil nil)                (r/->TupleSpec [(r/->AtomSpec)])               (r/->TupleSpec [(r/->AtomSpec)])
      (r/->CompoundSpec nil '())                (r/->GroundSpec)                                 (r/->CompoundSpec nil '())
      (r/->CompoundSpec nil '())                (r/->NonvarSpec)                                 (r/->CompoundSpec nil '())
      (r/->CompoundSpec nil '())                (r/->VarSpec)                                    :error
      (r/->CompoundSpec nil '())                (r/make-spec:user-defined "atomOrInt")           :error
      (r/->CompoundSpec nil '())                (r/make-spec:user-defined "atomOrVar")           :error
      (r/->CompoundSpec nil '())                (r/->AnySpec)                                    (r/->CompoundSpec nil '())

      (r/->CompoundSpec "foo" [(r/->FloatSpec)])                (r/->CompoundSpec "foo" [(r/->FloatSpec)])     (r/->CompoundSpec "foo" [(r/->FloatSpec)])
      (r/->CompoundSpec "foo" [(r/->FloatSpec)])                (r/->CompoundSpec "foo" [(r/->NumberSpec)])     (r/->CompoundSpec "foo" [(r/->FloatSpec)])
      (r/->CompoundSpec "foo" [(r/->FloatSpec)])                (r/->ListSpec (r/->FloatSpec))                 :error
      (r/->CompoundSpec "foo" [(r/->FloatSpec)])                (r/->ListSpec (r/->FloatSpec))                 :error
      (r/->CompoundSpec "foo" [(r/->FloatSpec)])                (r/->EmptyListSpec)                              :error
      (r/->CompoundSpec "foo" [(r/->FloatSpec)])                (r/->TupleSpec [(r/->AtomSpec)])               :error
      (r/->CompoundSpec "foo" [(r/->FloatSpec)])                (r/->GroundSpec)                                 (r/->CompoundSpec "foo" [(r/->FloatSpec)])
      (r/->CompoundSpec "foo" [(r/->VarSpec)])                  (r/->GroundSpec)                                 :error
      (r/->CompoundSpec "foo" [(r/->FloatSpec)])                (r/->NonvarSpec)                                 (r/->CompoundSpec "foo" [(r/->FloatSpec)])
      (r/->CompoundSpec "foo" [(r/->VarSpec)])                  (r/->NonvarSpec)                                 (r/->CompoundSpec "foo" [(r/->VarSpec)])
      (r/->CompoundSpec "foo" [(r/->FloatSpec)])                (r/->VarSpec)                                    :error
      (r/->CompoundSpec "foo" [(r/->FloatSpec)])                (r/make-spec:user-defined "atomOrInt")           :error
      (r/->CompoundSpec "foo" [(r/->FloatSpec)])                (r/make-spec:user-defined "atomOrVar")           :error
      (r/->CompoundSpec "foo" [(r/->FloatSpec)])                (r/->AnySpec)                                    (r/->CompoundSpec "foo" [(r/->FloatSpec)])


      (r/->ListSpec (r/->FloatSpec))                (r/->ListSpec (r/->FloatSpec))                 (r/->ListSpec (r/->FloatSpec))
      (r/->ListSpec (r/->FloatSpec))                (r/->ListSpec (r/->NumberSpec))                (r/->ListSpec (r/->FloatSpec))
      (r/->ListSpec (r/->FloatSpec))                (r/->ListSpec (r/->AtomSpec))                  :error
      (r/->ListSpec (r/->FloatSpec))                (r/->EmptyListSpec)                              (r/->EmptyListSpec)
      (r/->ListSpec (r/->FloatSpec))                (r/->TupleSpec [(r/->AtomSpec)])               :error
      (r/->ListSpec (r/->FloatSpec))                (r/->TupleSpec [(r/->NumberSpec)])             (r/->TupleSpec [(r/->FloatSpec)])
      (r/->ListSpec (r/->FloatSpec))                (r/->GroundSpec)                                 (r/->ListSpec (r/->FloatSpec))
      (r/->ListSpec (r/->VarSpec))                  (r/->GroundSpec)                                 :error
      (r/->ListSpec (r/->FloatSpec))                (r/->NonvarSpec)                                 (r/->ListSpec (r/->FloatSpec))
      (r/->ListSpec (r/->FloatSpec))                (r/->VarSpec)                                    :error
      (r/->ListSpec (r/->FloatSpec))                (r/make-spec:user-defined "atomOrInt")           :error
      (r/->ListSpec (r/->FloatSpec))                (r/make-spec:user-defined "atomOrVar")           :error
      (r/->ListSpec (r/->FloatSpec))                (r/->AnySpec)                                    (r/->ListSpec (r/->FloatSpec))

      (r/->EmptyListSpec)                (r/->EmptyListSpec)                              (r/->EmptyListSpec)
      (r/->EmptyListSpec)                (r/->TupleSpec [(r/->AtomSpec)])               :error
      (r/->EmptyListSpec)                (r/->GroundSpec)                                 (r/->EmptyListSpec)
      (r/->EmptyListSpec)                (r/->NonvarSpec)                                 (r/->EmptyListSpec)
      (r/->EmptyListSpec)                (r/->VarSpec)                                    :error
      (r/->EmptyListSpec)                (r/make-spec:user-defined "atomOrInt")           :error
      (r/->EmptyListSpec)                (r/make-spec:user-defined "atomOrVar")           :error
      (r/->EmptyListSpec)                (r/->AnySpec)                                    (r/->EmptyListSpec)


      (r/->TupleSpec [(r/->AtomSpec)])                (r/->TupleSpec [(r/->AtomSpec)])               (r/->TupleSpec [(r/->AtomSpec)])
      (r/->TupleSpec [(r/->AtomSpec)])                (r/->TupleSpec [(r/->FloatSpec)])              :error
      (r/->TupleSpec [(r/->AtomSpec)])                (r/->GroundSpec)                                 (r/->TupleSpec [(r/->AtomSpec)])
      (r/->TupleSpec [(r/->VarSpec)])                 (r/->GroundSpec)                                 :error
      (r/->TupleSpec [(r/->AtomSpec)])                (r/->NonvarSpec)                                 (r/->TupleSpec [(r/->AtomSpec)])
      (r/->TupleSpec [(r/->AtomSpec)])                (r/->VarSpec)                                    :error
      (r/->TupleSpec [(r/->AtomSpec)])                (r/make-spec:user-defined "atomOrInt")           :error
      (r/->TupleSpec [(r/->AtomSpec)])                (r/make-spec:user-defined "atomOrVar")           :error
      (r/->TupleSpec [(r/->AtomSpec)])                (r/->AnySpec)                                    (r/->TupleSpec [(r/->AtomSpec)])

      (r/->GroundSpec)                (r/->GroundSpec)                                 (r/->GroundSpec)
      (r/->GroundSpec)                (r/->NonvarSpec)                                 (r/->GroundSpec)
      (r/->GroundSpec)                (r/->VarSpec)                                    :error
      (r/->GroundSpec)                (r/make-spec:user-defined "atomOrInt")           (r/->OneOfSpec #{(r/->AtomSpec) (r/->IntegerSpec)})
      (r/->GroundSpec)                (r/make-spec:user-defined "atomOrVar")           (r/->AtomSpec)
      (r/->GroundSpec)                (r/->AnySpec)                                    (r/->GroundSpec)

      (r/->NonvarSpec)                (r/->NonvarSpec)                                 (r/->NonvarSpec)
      (r/->NonvarSpec)                (r/->VarSpec)                                    :error
      (r/->NonvarSpec)                (r/make-spec:user-defined "atomOrInt")           (r/->OneOfSpec #{(r/->IntegerSpec) (r/->AtomSpec)})
      (r/->NonvarSpec)                (r/make-spec:user-defined "atomOrVar")           (r/->AtomSpec)
      (r/->NonvarSpec)                (r/->AnySpec)                                    (r/->NonvarSpec)


      (r/->VarSpec)                (r/->VarSpec)                                    (r/->VarSpec)
      (r/->VarSpec)                (r/make-spec:user-defined "atomOrInt")           :error
      (r/->VarSpec)                (r/make-spec:user-defined "atomOrVar")           (r/->VarSpec)
      (r/->VarSpec)                (r/->AnySpec)                                    (r/->VarSpec)

      (r/make-spec:user-defined "atomOrInt")                (r/make-spec:user-defined "atomOrInt")           (r/make-spec:user-defined "atomOrInt")
      (r/make-spec:user-defined "atomOrInt")                (r/make-spec:user-defined "atomOrVar")           (r/->AtomSpec)
      (r/make-spec:user-defined "atomOrInt")                (r/->AnySpec)                                    (r/make-spec:user-defined "atomOrInt")

      (r/->AnySpec)                (r/->AnySpec)                                    (r/->AnySpec)


      ))

(sut/intersect (r/->IntegerSpec) (r/make-spec:user-defined "atomOrVar") test-defs)

(deftest intersect-pre-spec-and-or
  (are [a b c] (if (= c :error)
                 (and
                  (r/error-spec? (sut/intersect a b test-defs))
                  (r/error-spec? (sut/intersect b a test-defs)))
                 (and
                  (= c (sut/intersect a b test-defs))
                  (= c (sut/intersect b a test-defs))))
    (r/->AtomSpec) (r/->OneOfSpec #{(r/->AtomicSpec) (r/->IntegerSpec)}) (r/->AtomSpec)
    (r/->AtomSpec) (r/->OneOfSpec #{(r/->VarSpec) (r/->IntegerSpec)}) :error
    (r/->OneOfSpec #{(r/->AtomicSpec) (r/->VarSpec)}) (r/->OneOfSpec #{(r/->AnySpec) (r/->IntegerSpec)}) (r/->OneOfSpec #{(r/->AtomicSpec) (r/->VarSpec)})

    (r/->AndSpec #{(r/->AnySpec) (r/->IntegerSpec)}) (r/->AndSpec #{(r/->AtomicSpec) (r/->GroundSpec)}) (r/->IntegerSpec)
    (r/->AndSpec #{(r/->AnySpec) (r/->IntegerSpec)}) (r/->AndSpec #{(r/->FloatSpec) (r/->GroundSpec)}) :error
    (r/->AndSpec #{(r/->AnySpec) (r/->IntegerSpec)}) (r/->AnySpec) (r/->IntegerSpec)

    ))

(deftest to-or-spec-test
  (are [x y] (= x (apply sut/to-or-spec nil y))
    (r/->ErrorSpec "Cannot build empty one-of") []
    (r/->IntegerSpec) [(r/->IntegerSpec)]
    (r/->OneOfSpec #{(r/->IntegerSpec) (r/->AtomSpec)}) [(r/->IntegerSpec) (r/->AtomSpec)]))

(def user-def-tree-int (r/make-spec:user-defined "tree" [(r/->IntegerSpec)]))
(def user-def-atomOrInt (r/make-spec:user-defined "atomOrInt"))
(def user-def-blob (r/make-spec:user-defined "blob"))
(defn user-def-tree [value]
  (r/make-spec:user-defined "tree" [value]))


(deftest resolve-definition
  (are [in expected] (= expected (sut/resolve-definition-with-parameters in test-defs))
    user-def-tree-int (r/->OneOfSpec #{(r/->CompoundSpec "node" [user-def-tree-int (r/->IntegerSpec) user-def-tree-int]) (r/->ExactSpec "empty")})
    (user-def-tree (r/->SpecvarSpec "Y")) (r/->OneOfSpec #{(r/->CompoundSpec "node" [(user-def-tree (r/->SpecvarSpec "Y")) (r/->SpecvarSpec "Y") (user-def-tree (r/->SpecvarSpec "Y"))]) (r/->ExactSpec "empty")})
    (user-def-tree (r/->SpecvarSpec "A")) (r/->OneOfSpec #{(r/->CompoundSpec "node" [(user-def-tree (r/->SpecvarSpec "A")) (r/->SpecvarSpec "A") (user-def-tree (r/->SpecvarSpec "A"))]) (r/->ExactSpec "empty")})
    (user-def-tree (r/make-spec:user-defined "blob")) (r/->OneOfSpec #{(r/->CompoundSpec "node" [(user-def-tree (r/make-spec:user-defined "blob")) (r/make-spec:user-defined "blob") (user-def-tree (r/make-spec:user-defined "blob"))]) (r/->ExactSpec "empty")})

    (r/make-spec:user-defined "atomOrInt") (r/->OneOfSpec #{(r/->IntegerSpec) (r/->AtomSpec)})
    (r/make-spec:user-defined "blob") (r/->ExactSpec "blob")

    ))
