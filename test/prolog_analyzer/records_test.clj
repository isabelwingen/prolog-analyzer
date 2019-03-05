(ns prolog-analyzer.records-test
  (:require [prolog-analyzer.records :as sut]
            [ubergraph.core :as uber]
            [loom.graph]
            [loom.attr]
            [ubergraph.protocols]
            [clojure.test.check :as tc]
            [clojure.test.check.generators :as gen]
            [clojure.test.check.properties :as prop]
            [clojure.test :refer [deftest are is]]))

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
