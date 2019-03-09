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


               (sut/->GroundSpec) (sut/->GroundSpec) (sut/->GroundSpec)
               (sut/->GroundSpec) (sut/->NonvarSpec) (sut/->GroundSpec)
               (sut/->GroundSpec) (sut/->AnySpec) (sut/->GroundSpec)
               (sut/->GroundSpec) (sut/->VarSpec) sut/DISJOINT

               (sut/->NonvarSpec) (sut/->GroundSpec) (sut/->GroundSpec)
               (sut/->NonvarSpec) (sut/->NonvarSpec) (sut/->NonvarSpec)
               (sut/->NonvarSpec) (sut/->AnySpec) (sut/->NonvarSpec)
               (sut/->NonvarSpec) (sut/->VarSpec) sut/DISJOINT

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


               ))
