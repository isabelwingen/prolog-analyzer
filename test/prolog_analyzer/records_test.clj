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
    (sut/make-spec:tuple []) []
    (sut/make-spec:tuple [(sut/make-spec:integer)]) [(sut/make-spec:integer)]
    (sut/make-spec:tuple [(sut/make-spec:integer) (sut/make-spec:atom)]) [(sut/make-spec:integer) (sut/make-spec:atom)]))

(deftest to-or-spec-test
  (are [x y] (= x (apply sut/to-or-spec y))
    (sut/make-spec:error "Cannot build empty one-of") []
    (sut/make-spec:integer) [(sut/make-spec:integer)]
    (sut/make-spec:one-of [(sut/make-spec:integer) (sut/make-spec:atom)]) [(sut/make-spec:integer) (sut/make-spec:atom)]))

(deftest intersect
  (do-template [left right result] (is (= result (sut/intersect left right)) (str "Intersect of " (sut/spec-type left) " " (sut/spec-type right)))
               (sut/->IntegerSpec) (sut/->IntegerSpec) (sut/->IntegerSpec)
               (sut/->IntegerSpec) (sut/->FloatSpec) nil
               (sut/->IntegerSpec) (sut/->NumberSpec) (sut/->IntegerSpec)
               (sut/->IntegerSpec) (sut/->ExactSpec "cake") nil
               (sut/->IntegerSpec) (sut/->AtomSpec) nil
               (sut/->IntegerSpec) (sut/->AtomicSpec) (sut/->IntegerSpec)
               (sut/->IntegerSpec) (sut/->CompoundSpec "foo" [(sut/->AtomicSpec)]) nil
               (sut/->IntegerSpec) (sut/->ListSpec (sut/->AtomicSpec)) nil
               (sut/->IntegerSpec) (sut/->EmptyListSpec) nil
               (sut/->IntegerSpec) (sut/->TupleSpec [(sut/->AtomicSpec)]) nil
               (sut/->IntegerSpec) (sut/->GroundSpec) (sut/->IntegerSpec)
               (sut/->IntegerSpec) (sut/->NonvarSpec) (sut/->IntegerSpec)
               (sut/->IntegerSpec) (sut/->VarSpec) nil
               (sut/->IntegerSpec) (sut/->AnySpec) (sut/->IntegerSpec)

               (sut/->FloatSpec) (sut/->IntegerSpec) nil
               (sut/->FloatSpec) (sut/->FloatSpec) (sut/->FloatSpec)
               (sut/->FloatSpec) (sut/->NumberSpec) (sut/->FloatSpec)
               (sut/->FloatSpec) (sut/->ExactSpec "cake") nil
               (sut/->FloatSpec) (sut/->AtomSpec) nil
               (sut/->FloatSpec) (sut/->AtomicSpec) (sut/->FloatSpec)
               (sut/->FloatSpec) (sut/->CompoundSpec "foo" [(sut/->AtomicSpec)]) nil
               (sut/->FloatSpec) (sut/->ListSpec (sut/->AtomicSpec)) nil
               (sut/->FloatSpec) (sut/->EmptyListSpec) nil
               (sut/->FloatSpec) (sut/->TupleSpec [(sut/->AtomicSpec)]) nil
               (sut/->FloatSpec) (sut/->GroundSpec) (sut/->FloatSpec)
               (sut/->FloatSpec) (sut/->NonvarSpec) (sut/->FloatSpec)
               (sut/->FloatSpec) (sut/->VarSpec) nil
               (sut/->FloatSpec) (sut/->AnySpec) (sut/->FloatSpec)

               (sut/->NumberSpec) (sut/->IntegerSpec) (sut/->IntegerSpec)
               (sut/->NumberSpec) (sut/->FloatSpec) (sut/->FloatSpec)
               (sut/->NumberSpec) (sut/->NumberSpec) (sut/->NumberSpec)
               (sut/->NumberSpec) (sut/->ExactSpec "cake") nil
               (sut/->NumberSpec) (sut/->AtomSpec) nil
               (sut/->NumberSpec) (sut/->AtomicSpec) (sut/->NumberSpec)
               (sut/->NumberSpec) (sut/->CompoundSpec "foo" [(sut/->AtomicSpec)]) nil
               (sut/->NumberSpec) (sut/->ListSpec (sut/->AtomicSpec)) nil
               (sut/->NumberSpec) (sut/->EmptyListSpec) nil
               (sut/->NumberSpec) (sut/->TupleSpec [(sut/->AtomicSpec)]) nil
               (sut/->NumberSpec) (sut/->GroundSpec) (sut/->NumberSpec)
               (sut/->NumberSpec) (sut/->NonvarSpec) (sut/->NumberSpec)
               (sut/->NumberSpec) (sut/->VarSpec) nil
               (sut/->NumberSpec) (sut/->AnySpec) (sut/->NumberSpec)

               (sut/->ExactSpec "cake") (sut/->IntegerSpec) nil
               (sut/->ExactSpec "cake") (sut/->FloatSpec) nil
               (sut/->ExactSpec "cake") (sut/->NumberSpec) nil
               (sut/->ExactSpec "cake") (sut/->ExactSpec "cake") (sut/->ExactSpec "cake")
               (sut/->ExactSpec "cake") (sut/->ExactSpec "nocake") nil
               (sut/->ExactSpec "cake") (sut/->AtomSpec) (sut/->ExactSpec "cake")
               (sut/->ExactSpec "cake") (sut/->AtomicSpec) (sut/->ExactSpec "cake")
               (sut/->ExactSpec "cake") (sut/->CompoundSpec "foo" [(sut/->AtomicSpec)]) nil
               (sut/->ExactSpec "cake") (sut/->ListSpec (sut/->AtomicSpec)) nil
               (sut/->ExactSpec "cake") (sut/->EmptyListSpec) nil
               (sut/->ExactSpec "cake") (sut/->TupleSpec [(sut/->AtomicSpec)]) nil
               (sut/->ExactSpec "cake") (sut/->GroundSpec) (sut/->ExactSpec "cake")
               (sut/->ExactSpec "cake") (sut/->NonvarSpec) (sut/->ExactSpec "cake")
               (sut/->ExactSpec "cake") (sut/->VarSpec) nil
               (sut/->ExactSpec "cake") (sut/->AnySpec) (sut/->ExactSpec "cake")

               (sut/->AtomSpec) (sut/->IntegerSpec) nil
               (sut/->AtomSpec) (sut/->FloatSpec) nil
               (sut/->AtomSpec) (sut/->NumberSpec) nil
               (sut/->AtomSpec) (sut/->ExactSpec "cake") (sut/->ExactSpec "cake")
               (sut/->AtomSpec) (sut/->AtomSpec) (sut/->AtomSpec)
               (sut/->AtomSpec) (sut/->AtomicSpec) (sut/->AtomSpec)
               (sut/->AtomSpec) (sut/->CompoundSpec "foo" [(sut/->AtomicSpec)]) nil
               (sut/->AtomSpec) (sut/->ListSpec (sut/->AtomicSpec)) nil
               (sut/->AtomSpec) (sut/->EmptyListSpec) nil
               (sut/->AtomSpec) (sut/->TupleSpec [(sut/->AtomicSpec)]) nil
               (sut/->AtomSpec) (sut/->GroundSpec) (sut/->AtomSpec)
               (sut/->AtomSpec) (sut/->NonvarSpec) (sut/->AtomSpec)
               (sut/->AtomSpec) (sut/->VarSpec) nil
               (sut/->AtomSpec) (sut/->AnySpec) (sut/->AtomSpec)

               (sut/->AtomicSpec) (sut/->IntegerSpec) (sut/->IntegerSpec)
               (sut/->AtomicSpec) (sut/->FloatSpec) (sut/->FloatSpec)
               (sut/->AtomicSpec) (sut/->NumberSpec) (sut/->NumberSpec)
               (sut/->AtomicSpec) (sut/->ExactSpec "cake") (sut/->ExactSpec "cake")
               (sut/->AtomicSpec) (sut/->AtomSpec) (sut/->AtomSpec)
               (sut/->AtomicSpec) (sut/->AtomicSpec) (sut/->AtomicSpec)
               (sut/->AtomicSpec) (sut/->CompoundSpec "foo" [(sut/->AtomicSpec)]) nil
               (sut/->AtomicSpec) (sut/->ListSpec (sut/->AtomicSpec)) nil
               (sut/->AtomicSpec) (sut/->EmptyListSpec) (sut/->EmptyListSpec)
               (sut/->AtomicSpec) (sut/->TupleSpec [(sut/->AtomicSpec)]) nil
               (sut/->AtomicSpec) (sut/->GroundSpec) (sut/->AtomicSpec)
               (sut/->AtomicSpec) (sut/->NonvarSpec) (sut/->AtomicSpec)
               (sut/->AtomicSpec) (sut/->VarSpec) nil
               (sut/->AtomicSpec) (sut/->AnySpec) (sut/->AtomicSpec)


               (sut/->CompoundSpec "foo" [(sut/->IntegerSpec) (sut/->AtomSpec)]) (sut/->IntegerSpec) nil
               (sut/->CompoundSpec "foo" [(sut/->IntegerSpec) (sut/->AtomSpec)]) (sut/->FloatSpec) nil
               (sut/->CompoundSpec "foo" [(sut/->IntegerSpec) (sut/->AtomSpec)]) (sut/->NumberSpec) nil
               (sut/->CompoundSpec "foo" [(sut/->IntegerSpec) (sut/->AtomSpec)]) (sut/->ExactSpec "cake") nil
               (sut/->CompoundSpec "foo" [(sut/->IntegerSpec) (sut/->AtomSpec)]) (sut/->AtomSpec) nil
               (sut/->CompoundSpec "foo" [(sut/->IntegerSpec) (sut/->AtomSpec)]) (sut/->AtomicSpec) nil
               (sut/->CompoundSpec "foo" [(sut/->IntegerSpec) (sut/->AtomSpec)]) (sut/->ListSpec (sut/->AtomicSpec)) nil
               (sut/->CompoundSpec "foo" [(sut/->IntegerSpec) (sut/->AtomSpec)]) (sut/->EmptyListSpec) nil
               (sut/->CompoundSpec "foo" [(sut/->IntegerSpec) (sut/->AtomSpec)]) (sut/->TupleSpec [(sut/->AtomicSpec)]) nil
               (sut/->CompoundSpec "foo" [(sut/->IntegerSpec) (sut/->AtomSpec)]) (sut/->VarSpec) nil

               (sut/->CompoundSpec "foo" [(sut/->IntegerSpec) (sut/->VarSpec)]) (sut/->GroundSpec) nil
               (sut/->CompoundSpec "foo" [(sut/->IntegerSpec) (sut/->AtomSpec)]) (sut/->GroundSpec) (sut/->CompoundSpec "foo" [(sut/->IntegerSpec) (sut/->AtomSpec)])
               (sut/->CompoundSpec "foo" [(sut/->IntegerSpec) (sut/->VarSpec)]) (sut/->NonvarSpec) (sut/->CompoundSpec "foo" [(sut/->IntegerSpec) (sut/->VarSpec)])
               (sut/->CompoundSpec "foo" [(sut/->IntegerSpec)]) (sut/->CompoundSpec "foo" [(sut/->IntegerSpec)]) (sut/->CompoundSpec "foo" [(sut/->IntegerSpec)])
               (sut/->CompoundSpec "foo" [(sut/->IntegerSpec)]) (sut/->CompoundSpec "nofoo" [(sut/->IntegerSpec)]) nil
               (sut/->CompoundSpec "foo" [(sut/->IntegerSpec)]) (sut/->CompoundSpec "foo" [(sut/->AtomSpec)]) nil
               (sut/->CompoundSpec "foo" [(sut/->IntegerSpec)]) (sut/->CompoundSpec "foo" [(sut/->IntegerSpec) (sut/->AtomSpec)]) nil
               (sut/->CompoundSpec "foo" [(sut/->IntegerSpec) (sut/->AtomSpec)]) (sut/->AnySpec) (sut/->CompoundSpec "foo" [(sut/->IntegerSpec) (sut/->AtomSpec)])

               (sut/->ListSpec (sut/->IntegerSpec)) (sut/->IntegerSpec) nil
               (sut/->ListSpec (sut/->IntegerSpec)) (sut/->FloatSpec) nil
               (sut/->ListSpec (sut/->IntegerSpec)) (sut/->NumberSpec) nil
               (sut/->ListSpec (sut/->IntegerSpec)) (sut/->ExactSpec "cake") nil
               (sut/->ListSpec (sut/->IntegerSpec)) (sut/->AtomSpec) nil
               (sut/->ListSpec (sut/->IntegerSpec)) (sut/->AtomicSpec) (sut/->EmptyListSpec)
               (sut/->ListSpec (sut/->IntegerSpec)) (sut/->CompoundSpec "foo" [(sut/->AtomicSpec)]) nil
               (sut/->ListSpec (sut/->IntegerSpec)) (sut/->ListSpec (sut/->AtomicSpec)) (sut/->ListSpec (sut/->IntegerSpec))
               (sut/->ListSpec (sut/->IntegerSpec)) (sut/->ListSpec (sut/->AtomSpec)) nil
               (sut/->ListSpec (sut/->IntegerSpec)) (sut/->EmptyListSpec) (sut/->EmptyListSpec)
               (sut/->ListSpec (sut/->IntegerSpec)) (sut/->TupleSpec [(sut/->AtomicSpec)]) (sut/->TupleSpec [(sut/->IntegerSpec)])
               (sut/->ListSpec (sut/->IntegerSpec)) (sut/->TupleSpec [(sut/->AtomSpec)]) nil
               (sut/->ListSpec (sut/->IntegerSpec)) (sut/->GroundSpec) (sut/->ListSpec (sut/->IntegerSpec))
               (sut/->ListSpec (sut/->VarSpec)) (sut/->GroundSpec) nil
               (sut/->ListSpec (sut/->IntegerSpec)) (sut/->NonvarSpec) (sut/->ListSpec (sut/->IntegerSpec))
               (sut/->ListSpec (sut/->VarSpec)) (sut/->NonvarSpec) (sut/->ListSpec (sut/->VarSpec))
               (sut/->ListSpec (sut/->IntegerSpec)) (sut/->VarSpec) nil
               (sut/->ListSpec (sut/->IntegerSpec)) (sut/->AnySpec) (sut/->ListSpec (sut/->IntegerSpec))

               (sut/->EmptyListSpec) (sut/->IntegerSpec) nil
               (sut/->EmptyListSpec) (sut/->FloatSpec) nil
               (sut/->EmptyListSpec) (sut/->NumberSpec) nil
               (sut/->EmptyListSpec) (sut/->ExactSpec "cake") nil
               (sut/->EmptyListSpec) (sut/->AtomSpec) nil
               (sut/->EmptyListSpec) (sut/->AtomicSpec) (sut/->EmptyListSpec)
               (sut/->EmptyListSpec) (sut/->CompoundSpec "foo" [(sut/->AtomicSpec)]) nil
               (sut/->EmptyListSpec) (sut/->ListSpec (sut/->AtomicSpec)) (sut/->EmptyListSpec)
               (sut/->EmptyListSpec) (sut/->EmptyListSpec) (sut/->EmptyListSpec)
               (sut/->EmptyListSpec) (sut/->TupleSpec [(sut/->AtomicSpec)]) nil
               (sut/->EmptyListSpec) (sut/->TupleSpec []) (sut/->EmptyListSpec)
               (sut/->EmptyListSpec) (sut/->GroundSpec) (sut/->EmptyListSpec)
               (sut/->EmptyListSpec) (sut/->NonvarSpec) (sut/->EmptyListSpec)
               (sut/->EmptyListSpec) (sut/->VarSpec) nil
               (sut/->EmptyListSpec) (sut/->AnySpec) (sut/->EmptyListSpec)

               (sut/->TupleSpec [(sut/->IntegerSpec)]) (sut/->IntegerSpec) nil
               (sut/->TupleSpec [(sut/->IntegerSpec)]) (sut/->FloatSpec) nil
               (sut/->TupleSpec [(sut/->IntegerSpec)]) (sut/->NumberSpec) nil
               (sut/->TupleSpec [(sut/->IntegerSpec)]) (sut/->ExactSpec "cake") nil
               (sut/->TupleSpec [(sut/->IntegerSpec)]) (sut/->AtomSpec) nil
               (sut/->TupleSpec [(sut/->IntegerSpec)]) (sut/->AtomicSpec) nil
               (sut/->TupleSpec []) (sut/->AtomicSpec) (sut/->EmptyListSpec)
               (sut/->TupleSpec [(sut/->IntegerSpec)]) (sut/->CompoundSpec "foo" [(sut/->AtomicSpec)]) nil
               (sut/->TupleSpec [(sut/->IntegerSpec)]) (sut/->ListSpec (sut/->AtomicSpec)) (sut/->TupleSpec [(sut/->IntegerSpec)])
               (sut/->TupleSpec [(sut/->IntegerSpec)]) (sut/->EmptyListSpec) nil
               (sut/->TupleSpec []) (sut/->EmptyListSpec) (sut/->EmptyListSpec)
               (sut/->TupleSpec [(sut/->IntegerSpec)]) (sut/->TupleSpec []) nil
               (sut/->TupleSpec [(sut/->IntegerSpec)]) (sut/->TupleSpec [(sut/->AtomSpec)]) nil
               (sut/->TupleSpec [(sut/->IntegerSpec)]) (sut/->TupleSpec [(sut/->AtomicSpec)]) (sut/->TupleSpec [(sut/->IntegerSpec)])
               (sut/->TupleSpec [(sut/->IntegerSpec)]) (sut/->GroundSpec) (sut/->TupleSpec [(sut/->IntegerSpec)])
               (sut/->TupleSpec [(sut/->VarSpec)]) (sut/->GroundSpec) nil
               (sut/->TupleSpec [(sut/->IntegerSpec)]) (sut/->NonvarSpec) (sut/->TupleSpec [(sut/->IntegerSpec)])
               (sut/->TupleSpec [(sut/->VarSpec)]) (sut/->NonvarSpec) (sut/->TupleSpec [(sut/->VarSpec)])
               (sut/->TupleSpec [(sut/->IntegerSpec)]) (sut/->VarSpec) nil
               (sut/->TupleSpec [(sut/->IntegerSpec)]) (sut/->AnySpec) (sut/->TupleSpec [(sut/->IntegerSpec)])



               ))
