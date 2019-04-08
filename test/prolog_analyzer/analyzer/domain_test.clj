(ns prolog-analyzer.analyzer.domain-test
  (:require [prolog-analyzer.analyzer.domain :as sut]
            [prolog-analyzer.utils :as utils]
            [prolog-analyzer.analyzer.pretty-printer :as pp]
            [prolog-analyzer.records :as r]
            [clojure.test :refer [deftest is are]]
            [clojure.spec.alpha :as s]
            [clojure.spec.gen.alpha :as gen]
            [clojure.test.check :as tc]
            [clojure.test.check.properties :as prop]
            [ubergraph.core :as uber]
            [loom.graph]
            [loom.attr]
            [ubergraph.protocols]
            [clojure.spec.test.alpha :as stest]
            [clojure.template :refer [do-template]]
            ))


(defn valid-env?
  "Checks, if a env is valid and contains no errors."
  [env]
  (every? #(not= (r/spec-type %) :error) (map #(uber/attr env % :dom) (utils/get-terms env)))
  )





(def test-env (-> (uber/digraph) (uber/add-nodes-with-attrs
                                  [:ENVIRONMENT
                                   {:user-defined-specs
                                    {(r/make-spec:user-defined "tree" [(r/->SpecvarSpec "X")])
                                     (r/->OneOfSpec
                                      [(r/->CompoundSpec "node" [(r/make-spec:user-defined "tree" [(r/->SpecvarSpec "X")])
                                                                 (r/->SpecvarSpec "X")
                                                                 (r/make-spec:user-defined "tree" [(r/->SpecvarSpec "X")])])
                                       (r/->ExactSpec "empty")])

                                     (r/make-spec:user-defined "atomOrInt")
                                     (r/->OneOfSpec [(r/->IntegerSpec) (r/->AtomSpec)])

                                     (r/make-spec:user-defined "blob")
                                     (r/->ExactSpec "blob")

                                     (r/make-spec:user-defined "varOrInt")
                                     (r/->OneOfSpec [(r/->VarSpec) (r/->IntegerSpec)])
                                     }}])))


(def spec-simple (r/make-spec:user-defined "atomOrInt"))
(def spec-tree-int (r/make-spec:user-defined "tree" [(r/->IntegerSpec)]))
(def spec-tree-x (r/make-spec:user-defined"tree" [(r/->SpecvarSpec 0)]))

(def unfolded-tree-int
  (r/->OneOfSpec
   [(r/->CompoundSpec "node" [spec-tree-int (r/->IntegerSpec) spec-tree-int])
    (r/->ExactSpec "empty")]))

(deftest add-type-to-dom-test
  (is (= (r/->IntegerSpec)
         (-> test-env
             (uber/add-nodes-with-attrs [:x {:dom (r/->AtomicSpec)}])
             (sut/add-type-to-dom :x (r/->NumberSpec))
             (sut/add-type-to-dom :x (r/->IntegerSpec))
             (uber/attr :x :dom))))
  (is (r/error-spec?
         (-> test-env
             (sut/add-type-to-dom :x (r/->FloatSpec))
             (sut/add-type-to-dom :x (r/->IntegerSpec))
             (uber/attr :x :dom)))))

(defn calculate-and-get-dom
  ([initial? term spec]
   (utils/get-dom-of-term (sut/fill-env-for-term-with-spec test-env term spec {:initial initial?}) term))
  ([term spec]
   (calculate-and-get-dom false term spec)))

(deftest fill-env-test:any
  (do-template [term expected-dom] (is (= expected-dom (calculate-and-get-dom term (r/->AnySpec))) (str (r/to-string term)))
               (r/->AtomTerm "cake") (r/->AtomSpec)
               (r/->IntegerTerm 42) (r/->IntegerSpec)
               (r/->NumberTerm 23) (r/->NumberSpec)
               (r/->FloatTerm 3.1415) (r/->FloatSpec)
               (r/->ListTerm (r/->IntegerTerm 1) (r/->EmptyListTerm)) (r/->ListSpec (r/->IntegerSpec))
               (r/->CompoundTerm "wrap" [(r/->AtomTerm "salad") (r/->AtomTerm "tomatoes")]) (r/->CompoundSpec "wrap" [(r/->AtomSpec) (r/->AtomSpec)])
               (r/->VarTerm "X") (r/->AnySpec)
               (r/->AnonVarTerm "_1603") (r/->AnySpec)
               (r/->EmptyListTerm) (r/->EmptyListSpec)))

(deftest fill-env-test:ground
  (do-template [term expected-dom] (is (= expected-dom (calculate-and-get-dom term (r/->GroundSpec))) (str (r/to-string term)))
               (r/->AtomTerm "bunker") (r/->AtomSpec)
               (r/->IntegerTerm 42) (r/->IntegerSpec)
               (r/->NumberTerm 23) (r/->NumberSpec)
               (r/->FloatTerm 3.1415) (r/->FloatSpec)
               (r/->ListTerm (r/->IntegerTerm 1) (r/->EmptyListTerm)) (r/->ListSpec (r/->IntegerSpec))
               (r/->ListTerm (r/->VarTerm "X") (r/->EmptyListTerm)) (r/->ListSpec (r/->GroundSpec)) ;; TODO: is this okay?
               (r/->CompoundTerm "foo" [(r/->VarTerm "X")]) (r/->CompoundSpec "foo" [(r/->GroundSpec)])
               (r/->CompoundTerm "wrap" [(r/->AtomTerm "salad") (r/->AtomTerm "tomatoes")]) (r/->CompoundSpec "wrap" [(r/->AtomSpec) (r/->AtomSpec)]))
  (do-template [term] (is (r/error-spec? (calculate-and-get-dom term (r/->GroundSpec))) (str (r/to-string term)))
    (r/->VarTerm "X")
    (r/->AnonVarTerm "_1603")
    )

  )




(deftest fill-env-test:nonvar
  (do-template [term expected-dom] (is (= expected-dom (calculate-and-get-dom term (r/->NonvarSpec))) (str (r/to-string term)))
               (r/->AtomTerm "cake") (r/->AtomSpec)
               (r/->IntegerTerm 42) (r/->IntegerSpec)
               (r/->NumberTerm 23) (r/->NumberSpec)
               (r/->FloatTerm 3.1415) (r/->FloatSpec)
               (r/->ListTerm (r/->IntegerTerm 1) (r/->EmptyListTerm)) (r/->ListSpec (r/->IntegerSpec))
               (r/->ListTerm (r/->VarTerm "Y") (r/->EmptyListTerm)) (r/->ListSpec (r/->AnySpec))
               (r/->CompoundTerm "wrap" [(r/->AtomTerm "salad") (r/->AtomTerm "tomatoes")]) (r/->CompoundSpec "wrap" [(r/->AtomSpec) (r/->AtomSpec)]))
  (do-template [term] (is (r/error-spec? (calculate-and-get-dom term (r/->NonvarSpec))) (str (r/to-string term)))
    (r/->VarTerm "X")
    (r/->AnonVarTerm "_1603"))
  )
;; Important Example:
;; :- spec_pre(foo/1,[var]).
;; foo([1,2,3]).
;; --> Var is immediatly unified to [1,2,3] --> Change allowed
(deftest fill-env-test:var
  ;; NORMAL PRE_SPEC
  (is (r/error-spec? (calculate-and-get-dom (r/->AtomTerm "batman") (r/->VarSpec))))
  (is (r/error-spec? (calculate-and-get-dom (r/->IntegerTerm 1) (r/->VarSpec))))
  (is (every? (complement r/error-spec?) (calculate-and-get-dom (r/->VarTerm "X") (r/->VarSpec))))
  ;; UNIFICATION IN HEADER
  (do-template [term expected-dom] (is (= expected-dom (calculate-and-get-dom true term (r/->VarSpec))) (str (r/to-string term)))
               (r/->ListTerm (r/->IntegerTerm 1) (r/->EmptyListTerm)) (r/->ListSpec (r/->IntegerSpec))
               (r/->AtomTerm "a") (r/->AtomSpec)
    ))


(deftest fill-env-test:atomic
  (is (= (r/->AtomSpec) (calculate-and-get-dom (r/->AtomTerm "cake") (r/->AtomicSpec))))
  (is (= (r/->NumberSpec) (calculate-and-get-dom (r/->NumberTerm 2) (r/->AtomicSpec))))
  (is (= (r/->IntegerSpec) (calculate-and-get-dom (r/->IntegerTerm 2) (r/->AtomicSpec))))
  (is (= (r/->FloatSpec) (calculate-and-get-dom (r/->FloatTerm 2.0) (r/->AtomicSpec))))
  (are [term]
      (r/error-spec? (utils/get-dom-of-term (sut/fill-env-for-term-with-spec test-env term (r/->AtomSpec)) term))
    (r/->ListTerm (r/->IntegerTerm 1) (r/->EmptyListTerm))
    (r/->CompoundTerm "foo" [(r/->AtomTerm "foo")])
    )
  )



(deftest fill-env-test:atom
  (is (= (r/->AtomSpec) (calculate-and-get-dom (r/->AtomTerm "cake") (r/->AtomSpec))))
  (are [term] (r/error-spec? (calculate-and-get-dom term (r/->AtomSpec)))
    (r/->EmptyListTerm)
    (r/->ListTerm (r/->IntegerTerm 2) (r/->EmptyListTerm))
    (r/->CompoundTerm "foo" [(r/->AtomTerm "foo")])
    (r/->VarTerm "X")
    (r/->AnonVarTerm "_0410")
    )
  )

;;TODO: what is the initial spec of an atom? ExactSpec or AtomSpec? How handle intersectioning with exact?
;; Current Status: Value of atom is lost, intersect with exact always yields a result, check later, if it matches with the original value
(deftest fill-env-test:exact
  (are [term]
      (= (r/->ExactSpec "cake") (calculate-and-get-dom term (r/->ExactSpec "cake")))
    (r/->AtomTerm "cake")
    )
  (are [term]
      (r/error-spec? (utils/get-dom-of-term (sut/fill-env-for-term-with-spec test-env term (r/->ExactSpec "cake")) term))
    (r/->ListTerm (r/->IntegerTerm 2) (r/->EmptyListTerm))
    (r/->CompoundTerm "foo" [(r/->AtomTerm "foo")])
    (r/->AtomTerm "nocake")
    (r/->IntegerTerm 2)
    (r/->NumberTerm 2)
    (r/->FloatTerm 2.0)
    (r/->VarTerm "X")
    ))


(deftest fill-env-test:number
  (is (= (r/->IntegerSpec) (calculate-and-get-dom (r/->IntegerTerm 2) (r/->NumberSpec))))
  (is (= (r/->FloatSpec) (calculate-and-get-dom (r/->FloatTerm 2.5) (r/->NumberSpec))))
  (are [term]
      (r/error-spec? (calculate-and-get-dom term (r/->NumberSpec)))
    (r/->AtomTerm "no")
    (r/->VarTerm "X")
    (r/->ListTerm (r/->NumberTerm 2) (r/->EmptyListTerm))
    (r/->CompoundTerm "node" [(r/->AtomTerm "hello")])
    )
  )

(deftest fill-env-test:integer
  (are [term]
      (= (r/->IntegerSpec) (calculate-and-get-dom term (r/->IntegerSpec)))
    (r/->IntegerTerm 42))

  (do-template [term]
               (is (r/error-spec? (calculate-and-get-dom term (r/->IntegerSpec))) (str (r/to-string term)))
               (r/->NumberTerm 3.14)
               (r/->FloatTerm 3.14)
               (r/->VarTerm "X")
               (r/->AtomTerm "no")
               (r/->AnonVarTerm "_123")
               (r/->ListTerm (r/->IntegerTerm 0) (r/->EmptyListTerm))
               (r/->CompoundTerm "node" [(r/->AtomTerm "hello")]))
  )


(deftest fill-env-test:float
  (are [term]
      (= (r/->FloatSpec) (calculate-and-get-dom term (r/->FloatSpec)))
    (r/->FloatTerm 3.14))

  (are [term]
      (r/error-spec? (calculate-and-get-dom term (r/->FloatSpec)))
    (r/->NumberTerm 42)
    (r/->IntegerTerm 123)
    (r/->AtomTerm "no")
    (r/->ListTerm (r/->FloatTerm 0.0) (r/->EmptyListTerm))
    (r/->VarTerm "X")
    (r/->AnonVarTerm "_234")
    (r/->CompoundTerm "node" [(r/->AtomTerm "hello")])
    )
  )

  (deftest fill-env-test:list
    (are [term]
        (= (r/->ListSpec (r/->IntegerSpec)) (calculate-and-get-dom term (r/->ListSpec (r/->IntegerSpec))))
      (r/->ListTerm (r/->IntegerTerm 2) (r/->ListTerm (r/->IntegerTerm 3) (r/->EmptyListTerm)))
      )
    (are [term]
        (false? (valid-env? (sut/fill-env-for-term-with-spec test-env term (r/->ListSpec (r/->IntegerSpec)))))
      (r/->ListTerm (r/->FloatTerm 2.5) (r/->EmptyListTerm)) ;TODO: atm the error is only visible in the HEAD term. Should it be at top level?
      (r/->CompoundTerm "foo" [(r/->AtomTerm "foo")])
      (r/->AtomTerm "nocake")
      (r/->VarTerm "X")
      (r/->AnonVarTerm "_0410")
      (r/->IntegerTerm 2)
      (r/->NumberTerm 2)
      (r/->FloatTerm 2.0)))

(deftest fill-env-test:tuple
  (do-template [term spec expected-dom] (is (= expected-dom (calculate-and-get-dom term spec)) (str (r/to-string term)))
               (r/->ListTerm (r/->IntegerTerm 1) (r/->ListTerm (r/->IntegerTerm 3) (r/->EmptyListTerm)))
               (r/->TupleSpec [(r/->IntegerSpec) (r/->IntegerSpec)])
               (r/->TupleSpec [(r/->IntegerSpec) (r/->IntegerSpec)])


               (r/->ListTerm (r/->IntegerTerm 1) (r/->ListTerm (r/->AtomTerm "cake") (r/->EmptyListTerm)))
               (r/->TupleSpec [(r/->NumberSpec) (r/->AtomSpec)])
               (r/->TupleSpec [(r/->IntegerSpec) (r/->AtomSpec)])
               )
  (are [term]
      (r/error-spec? (utils/get-dom-of-term (sut/fill-env-for-term-with-spec test-env term (r/->TupleSpec [(r/->IntegerSpec) (r/->AtomSpec)])) term))
    (r/->EmptyListTerm)
    (r/->ListTerm (r/->FloatTerm 2.5) (r/->EmptyListTerm))
    (r/->CompoundTerm "foo" [(r/->AtomTerm "foo")])
    (r/->AtomTerm "nocake")
    (r/->IntegerTerm 2)
    (r/->VarTerm "X")
    (r/->AnonVarTerm "_0410")
    (r/->NumberTerm 2)
    (r/->FloatTerm 2.0)))

(deftest fill-env-test:compound
  (are [term]
      (= (r/->CompoundSpec "foo" [(r/->IntegerSpec) (r/->AtomSpec)]) (calculate-and-get-dom term (r/->CompoundSpec "foo" [(r/->IntegerSpec) (r/->AtomSpec)])))
    (r/->CompoundTerm "foo" [(r/->NumberTerm 2) (r/->AtomTerm "cake")])
    )
  (are [term]
      (false? (valid-env? (sut/fill-env-for-term-with-spec test-env term (r/->CompoundSpec "foo" [(r/->IntegerSpec) (r/->AtomSpec)]))))
    (r/->EmptyListTerm)
    (r/->ListTerm  (r/->FloatTerm 2.5) (r/->EmptyListTerm)) ;TODO: atm the error is only visible in the HEAD term. Should it be at top level?
    (r/->CompoundTerm "foo" [(r/->AtomTerm "foo")])
    (r/->AtomTerm "nocake")
    (r/->IntegerTerm 2)
    (r/->VarTerm "X")
    (r/->AnonVarTerm "_0410")
    (r/->NumberTerm 2)
    (r/->FloatTerm 2.0)))

(deftest fill-env-test:empty-list
  (are [term spec expected-dom] (= expected-dom (calculate-and-get-dom term spec))
    (r/->EmptyListTerm) (r/->TupleSpec []) (r/->EmptyListSpec)

    (r/->EmptyListTerm) (r/->ListSpec (r/->IntegerSpec)) (r/->EmptyListSpec)

    (r/->EmptyListTerm) (r/->EmptyListSpec) (r/->EmptyListSpec)

    (r/->EmptyListTerm) (r/->AndSpec [(r/->ListSpec (r/->IntegerSpec)) (r/->AtomicSpec)]) (r/->EmptyListSpec)


    ))


(deftest fill-env-test:var:initial-and-dom-empty

  ;; initial and dom empty
  (do-template [term spec expected-dom] (is (= expected-dom (calculate-and-get-dom true term spec)) (str "initial and empty: " (r/to-string term) " " (r/to-string spec)))
               (r/->VarTerm "X") (r/->VarSpec) (r/->VarSpec)
               (r/->VarTerm "X") (r/->IntegerSpec) (r/->IntegerSpec)
               (r/->VarTerm "X") (r/->AnySpec) (r/->AnySpec)
               (r/->VarTerm "X") (r/make-spec:user-defined "blob") (r/->ExactSpec "blob")
               (r/->VarTerm "X") (r/->SpecvarSpec "X") (r/->AnySpec)
    ))

(deftest fill-env-test:var:non-initial-and-dom-empty
  ;; non initial and dom empty
  (do-template [term spec expected-dom] (is (= expected-dom (calculate-and-get-dom false term spec)) (str "not initial and empty: " (r/to-string term) " " (r/to-string spec)))
               (r/->VarTerm "X") (r/->VarSpec) (r/->VarSpec)
               (r/->VarTerm "X") (r/->IntegerSpec) (sut/CANNOT-GROUND)
               (r/->VarTerm "X") (r/->AnySpec) (r/->AnySpec)
               (r/->VarTerm "X") (r/make-spec:user-defined "blob") (sut/CANNOT-GROUND)
               (r/->VarTerm "X") (r/->SpecvarSpec "X") (r/->AnySpec)
    ))

(deftest fill-env-test:var:non-initial-and-dom-already-nonvar

  ;; non initial and dom already nonvar
  (do-template [term spec expected-dom]
               (let [mod-env (sut/add-type-to-dom test-env (r/->VarTerm "X") (r/->GroundSpec))]
                 (is (= expected-dom
                        (utils/get-dom-of-term (sut/fill-env-for-term-with-spec mod-env term spec {:initial false}) term)) (str "not initial and already nonvar: " (r/to-string term) " " (r/to-string spec))))
               (r/->VarTerm "X") (r/->VarSpec) (sut/ALREADY-NONVAR)
               (r/->VarTerm "X") (r/->IntegerSpec) (r/->IntegerSpec)
               (r/->VarTerm "X") (r/->AnySpec) (r/->GroundSpec)
               (r/->VarTerm "X") (r/make-spec:user-defined "blob") (r/->ExactSpec "blob")
               (r/->VarTerm "X") (r/->SpecvarSpec "X") (r/->GroundSpec)
               ))
(deftest fill-env-test:var:non-initial-and-dom-is-var

  ;; non initial and dom is var
  (do-template [term spec expected-dom]
               (let [mod-env (sut/add-type-to-dom test-env (r/->VarTerm "X") (r/->VarSpec))]
                 (is (= expected-dom
                        (utils/get-dom-of-term (sut/fill-env-for-term-with-spec mod-env term spec {:initial false}) term)) (str "not initial and dom is var: " (r/to-string term) " " (r/to-string spec))))
               (r/->VarTerm "X") (r/->VarSpec) (r/->VarSpec)
               (r/->VarTerm "X") (r/->IntegerSpec) (sut/CANNOT-GROUND)
               (r/->VarTerm "X") (r/->AnySpec) (r/->VarSpec)
               (r/->VarTerm "X") (r/make-spec:user-defined "blob") (sut/CANNOT-GROUND)
               (r/->VarTerm "X") (r/->SpecvarSpec "X") (r/->VarSpec)
               )


  )

(deftest pre-filled-doms-when-initializing
  (do-template [term first-spec second-spec expected-dom]
               (is (= expected-dom
                      (dissoc
                       (-> test-env
                           (sut/fill-env-for-term-with-spec term first-spec {:initial true})
                           (sut/fill-env-for-term-with-spec term second-spec {:initial true})
                           (utils/get-dom-of-term term))
                       :origin))
                   (str (r/to-string term) " " (r/to-string first-spec) " " (r/to-string second-spec)))

               (r/->VarTerm "X") (r/->VarSpec) (r/->IntegerSpec) (r/->IntegerSpec)
               (r/->VarTerm "X") (r/->IntegerSpec) (r/->VarSpec) (r/->IntegerSpec)
               (r/->VarTerm "X") (r/->VarSpec) (r/->VarSpec) (r/->VarSpec)

               (r/->AtomTerm "a") (r/->VarSpec) (r/->AtomSpec) (r/->AtomSpec)
               (r/->AtomTerm "a") (r/->VarSpec) (r/->VarSpec) (r/->AtomSpec)
               ))

(deftest flags-empty-dom
  (do-template [term spec options expected-dom]
               (is (= expected-dom
                      (dissoc
                       (-> test-env
                           (sut/fill-env-for-term-with-spec term spec options)
                           (utils/get-dom-of-term term))
                       :origin))
                   (str (clojure.string/join " " [(r/to-string term) (r/to-string spec) (str options)])))

               (r/->VarTerm "Y") (r/->IntegerSpec) {:initial false :overwrite false} (sut/CANNOT-GROUND)
               (r/->VarTerm "Y") (r/->IntegerSpec) {:initial false :overwrite true} (r/->IntegerSpec)
               (r/->VarTerm "Y") (r/->IntegerSpec) {:initial true :overwrite false} (r/->IntegerSpec)
               (r/->VarTerm "Y") (r/->IntegerSpec) {:initial true :overwrite true} (r/->IntegerSpec)

               (r/->VarTerm "Y") (r/->OneOfSpec [(r/->VarSpec) (r/->IntegerSpec)]) {:initial false :overwrite false} (r/->VarSpec)
               (r/->VarTerm "Y") (r/->OneOfSpec [(r/->VarSpec) (r/->IntegerSpec)]) {:initial false :overwrite true} (r/->OneOfSpec [(r/->VarSpec) (r/->IntegerSpec)])
               (r/->VarTerm "Y") (r/->OneOfSpec [(r/->VarSpec) (r/->IntegerSpec)]) {:initial true :overwrite false} (r/->OneOfSpec [(r/->VarSpec) (r/->IntegerSpec)])
               (r/->VarTerm "Y") (r/->OneOfSpec [(r/->VarSpec) (r/->IntegerSpec)]) {:initial true :overwrite true} (r/->OneOfSpec [(r/->VarSpec) (r/->IntegerSpec)])


               (r/->IntegerTerm 1) (r/->VarSpec) {:initial false :overwrite false} (sut/ALREADY-NONVAR)
               (r/->IntegerTerm 1) (r/->VarSpec) {:initial false :overwrite true} (sut/ALREADY-NONVAR)
               (r/->IntegerTerm 1) (r/->VarSpec) {:initial true :overwrite false} (r/->IntegerSpec)
               (r/->IntegerTerm 1) (r/->VarSpec) {:initial true :overwrite true} (r/->IntegerSpec)

               (r/->IntegerTerm 1) (r/->OneOfSpec [(r/->VarSpec) (r/->IntegerSpec)]) {:initial false :overwrite false} (r/->IntegerSpec)
               (r/->IntegerTerm 1) (r/->OneOfSpec [(r/->VarSpec) (r/->IntegerSpec)]) {:initial false :overwrite true} (r/->IntegerSpec)
               (r/->IntegerTerm 1) (r/->OneOfSpec [(r/->VarSpec) (r/->IntegerSpec)]) {:initial true :overwrite false} (r/->IntegerSpec)
               (r/->IntegerTerm 1) (r/->OneOfSpec [(r/->VarSpec) (r/->IntegerSpec)]) {:initial true :overwrite true} (r/->IntegerSpec)


               ))

(deftest flags-non-empty-dom
  (do-template [term spec1 spec2 options expected-dom]
               (is (= expected-dom
                      (dissoc
                       (-> test-env
                           (sut/add-type-to-dom term spec1)
                           (sut/fill-env-for-term-with-spec term spec2 options)
                           (utils/get-dom-of-term term))
                       :origin))
                   (str (clojure.string/join " " [(r/to-string term) (r/to-string spec1) (r/to-string spec2) (str options)])))

               (r/->VarTerm "Y") (r/->GroundSpec) (r/->IntegerSpec) {:initial false :overwrite false} (r/->IntegerSpec)
               (r/->VarTerm "Y") (r/->GroundSpec) (r/->IntegerSpec) {:initial false :overwrite true} (r/->IntegerSpec)
               (r/->VarTerm "Y") (r/->GroundSpec) (r/->IntegerSpec) {:initial true :overwrite false} (r/->IntegerSpec)
               (r/->VarTerm "Y") (r/->GroundSpec) (r/->IntegerSpec) {:initial true :overwrite true} (r/->IntegerSpec)


               (r/->VarTerm "Y") (r/->VarSpec) (r/->IntegerSpec) {:initial false :overwrite false} (sut/CANNOT-GROUND)
               (r/->VarTerm "Y") (r/->VarSpec) (r/->IntegerSpec) {:initial false :overwrite true} (r/->IntegerSpec)
               (r/->VarTerm "Y") (r/->VarSpec) (r/->IntegerSpec) {:initial true :overwrite false} (r/->IntegerSpec)
               (r/->VarTerm "Y") (r/->VarSpec) (r/->IntegerSpec) {:initial true :overwrite true} (r/->IntegerSpec)

               (r/->VarTerm "Y") (r/->VarSpec) (r/->OneOfSpec [(r/->IntegerSpec) (r/->VarSpec)]) {:initial false :overwrite false} (r/->VarSpec)
               (r/->VarTerm "Y") (r/->VarSpec) (r/->OneOfSpec [(r/->IntegerSpec) (r/->VarSpec)]) {:initial false :overwrite true} (r/->OneOfSpec [(r/->IntegerSpec) (r/->VarSpec)])
               (r/->VarTerm "Y") (r/->VarSpec) (r/->OneOfSpec [(r/->IntegerSpec) (r/->VarSpec)]) {:initial true :overwrite false} (r/->OneOfSpec [(r/->IntegerSpec) (r/->VarSpec)])
               (r/->VarTerm "Y") (r/->VarSpec) (r/->OneOfSpec [(r/->IntegerSpec) (r/->VarSpec)]) {:initial true :overwrite true} (r/->OneOfSpec [(r/->IntegerSpec) (r/->VarSpec)])

               (r/->VarTerm "Y") (r/->GroundSpec) (r/->OneOfSpec [(r/->IntegerSpec) (r/->VarSpec)]) {:initial false :overwrite false} (r/->IntegerSpec)
               (r/->VarTerm "Y") (r/->GroundSpec) (r/->OneOfSpec [(r/->IntegerSpec) (r/->VarSpec)]) {:initial false :overwrite true} (r/->GroundSpec)
               (r/->VarTerm "Y") (r/->GroundSpec) (r/->OneOfSpec [(r/->IntegerSpec) (r/->VarSpec)]) {:initial true :overwrite false} (r/->IntegerSpec)
               (r/->VarTerm "Y") (r/->GroundSpec) (r/->OneOfSpec [(r/->IntegerSpec) (r/->VarSpec)]) {:initial true :overwrite true} (r/->GroundSpec)



               (r/->IntegerTerm "1") (r/->GroundSpec) (r/->VarSpec) {:initial false :overwrite false} (sut/ALREADY-NONVAR)
               (r/->IntegerTerm "1") (r/->GroundSpec) (r/->VarSpec) {:initial false :overwrite true} (sut/ALREADY-NONVAR)
               (r/->IntegerTerm "1") (r/->GroundSpec) (r/->VarSpec) {:initial true :overwrite false} (r/->IntegerSpec)
               (r/->IntegerTerm "1") (r/->GroundSpec) (r/->VarSpec) {:initial true :overwrite true} (r/->IntegerSpec)


               ))

(defn- get-artificial-term [env term]
  (let [artificial-term (some->> term
                                 (uber/out-edges env)
                                 (filter #(= :artificial (uber/attr env % :relation)))
                                 first
                                 uber/dest)]
    artificial-term))
