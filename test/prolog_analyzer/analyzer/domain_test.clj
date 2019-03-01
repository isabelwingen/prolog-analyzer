(ns prolog-analyzer.analyzer.domain-test
  (:require [prolog-analyzer.analyzer.domain :as sut]
            [prolog-analyzer.utils :as utils]
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
            ))

(defn valid-env?
  "Checks, if a env is valid and contains no errors."
  [env]
  (every? #(not= (r/spec-type %) :error) (mapcat #(uber/attr env % :dom) (utils/get-terms env))))



(deftest add-doms-to-node-test
  (is (= [(r/->IntegerSpec) (r/->AtomSpec) (r/->FloatSpec)]
         (-> (uber/digraph)
             (uber/add-nodes-with-attrs [:x {:dom [(r/->IntegerSpec)]}])
             (sut/add-doms-to-node :x (r/->AtomSpec) (r/->FloatSpec))
             (uber/attr :x :dom))))
  (is (= [(r/->FloatSpec) (r/->IntegerSpec)]
         (-> (uber/digraph)
             (sut/add-doms-to-node :x (r/->FloatSpec) (r/->IntegerSpec))
             (uber/attr :x :dom)))))


(def test-env (-> (uber/digraph) (uber/add-nodes-with-attrs
                                  [:ENVIRONMENT
                                   {:user-defined-specs
                                    {(r/make-spec:user-defined
                                      "tree"
                                      [(r/make-spec:specvar "X")])
                                     (r/make-spec:one-of
                                      [(r/make-spec:compound
                                        "node"
                                        [(r/make-spec:user-defined
                                          "tree"
                                          [(r/make-spec:specvar "X")])
                                         (r/make-spec:specvar "X")
                                         (r/make-spec:user-defined
                                          "tree"
                                          [(r/make-spec:specvar "X")])])
                                       (r/make-spec:exact "empty")])

                                     (r/make-spec:user-defined "atomOrInt")
                                     (r/make-spec:one-of [(r/make-spec:integer) (r/make-spec:atom)])}
                                    (r/make-spec:user-defined "blob")
                                    (r/make-spec:exact "blob")}])))


(def spec-simple (r/make-spec:user-defined "atomOrInt"))
(def spec-tree-int (r/make-spec:user-defined "tree" [(r/make-spec:integer)]))
(def spec-tree-x (r/make-spec:user-defined"tree" [(r/make-spec:specvar 0)]))

(def unfolded-tree-int
  (r/make-spec:one-of
   [(r/make-spec:compound "node" [spec-tree-int (r/make-spec:integer) spec-tree-int])
    (r/make-spec:exact "empty")]))

(deftest fill-env-for-term-with-spec-test-any
  (are [in out]
      (= [out] (utils/get-dom-of-term (sut/fill-env-for-term-with-spec test-env in (r/make-spec:any)) in))
    (r/->AtomTerm "cake") (r/make-spec:atom)
    (r/->IntegerTerm 42) (r/make-spec:integer)
    (r/->NumberTerm 23) (r/make-spec:number)
    (r/->FloatTerm 3.1415) (r/make-spec:float)
    (r/->ListTerm (r/->IntegerTerm 1) (r/->AtomicTerm "[]")) (r/make-spec:list (r/make-spec:any))
    (r/->CompoundTerm "wrap" [(r/->AtomTerm "salad") (r/->AtomTerm "tomatoes")]) (r/make-spec:compound "wrap" [(r/make-spec:any) (r/make-spec:any)])
    (r/->VarTerm "X") (r/make-spec:var)
    (r/->AnonVarTerm "_1603") (r/make-spec:var)
    (r/->EmptyListTerm) (r/->EmptyListSpec)))

(deftest fill-env-for-term-with-spec-test-ground
  (are [in out]
      (= [out] (utils/get-dom-of-term (sut/fill-env-for-term-with-spec test-env in (r/make-spec:ground)) in))
    (r/->AtomTerm "bunker") (r/make-spec:atom)
    (r/->IntegerTerm 42) (r/make-spec:integer)
    (r/->NumberTerm 23) (r/make-spec:number)
    (r/->FloatTerm 3.1415) (r/make-spec:float)
    (r/->ListTerm (r/->IntegerTerm 1) (r/->AtomicTerm "[]")) (r/make-spec:list (r/make-spec:ground))
    (r/->CompoundTerm "wrap" [(r/->AtomTerm "salad") (r/->AtomTerm "tomatoes")]) (r/make-spec:compound "wrap" [(r/make-spec:ground) (r/make-spec:ground)])
    (r/->VarTerm "X") (r/make-spec:ground)
    (r/->AnonVarTerm "_1603") (r/make-spec:ground)
    ))


(deftest fill-env-for-term-with-spec-test-nonvar
  (are [in out]
      (= [out] (utils/get-dom-of-term (sut/fill-env-for-term-with-spec test-env in (r/make-spec:nonvar)) in))
    (r/->AtomTerm "cake") (r/make-spec:atom)
    (r/->IntegerTerm 42) (r/make-spec:integer)
    (r/->NumberTerm 23) (r/make-spec:number)
    (r/->FloatTerm 3.1415) (r/make-spec:float)
    (r/->ListTerm (r/->IntegerTerm 1) (r/->AtomicTerm "[]")) (r/make-spec:list (r/make-spec:any))
    (r/->CompoundTerm "wrap" [(r/->AtomTerm "salad") (r/->AtomTerm "tomatoes")]) (r/make-spec:compound "wrap" [(r/make-spec:any) (r/make-spec:any)])
    (r/->VarTerm "X") (r/make-spec:nonvar)
    (r/->AnonVarTerm "_1603") (r/make-spec:nonvar)
    ))


(deftest fill-env-for-term-with-spec-test-var
  ;; WITH NON-EMPTY-DOM
  (are [in expected]
      (= expected (utils/get-dom-of-term (-> test-env
                                             (sut/fill-env-for-term-with-spec in (r/make-spec:any))
                                             (sut/fill-env-for-term-with-spec  in (r/make-spec:var))) in))
    (r/->AtomTerm "batman") [(r/make-spec:atom) (sut/ALREADY-NONVAR)]
    (r/->IntegerTerm 42) [(r/make-spec:integer) (sut/ALREADY-NONVAR)]
    (r/->NumberTerm 23) [(r/make-spec:number) (sut/ALREADY-NONVAR)]
    (r/->FloatTerm 3.1415) [(r/make-spec:float) (sut/ALREADY-NONVAR)]
    (r/->ListTerm (r/->IntegerTerm 1) (r/->AtomicTerm "[]")) [(r/make-spec:list (r/make-spec:any)) (sut/ALREADY-NONVAR)]
    (r/->CompoundTerm "wrap" [(r/->AtomTerm "salad") (r/->AtomTerm "tomatoes")]) [(r/make-spec:compound "wrap" [(r/make-spec:any) (r/make-spec:any)]) (sut/ALREADY-NONVAR)])
  
  ;; WITH EMPTY DOM
  (are [in expected]
      (= expected (utils/get-dom-of-term (sut/fill-env-for-term-with-spec test-env in (r/make-spec:var)) in))
    (r/->AtomTerm "batman") [(sut/ALREADY-NONVAR)]
    (r/->IntegerTerm 42) [(sut/ALREADY-NONVAR)]
    (r/->NumberTerm 23) [(sut/ALREADY-NONVAR)]
    (r/->FloatTerm 3.1415) [(sut/ALREADY-NONVAR)]
    (r/->ListTerm (r/->IntegerTerm 1) (r/->AtomicTerm "[]")) [(sut/ALREADY-NONVAR)]
    (r/->CompoundTerm "wrap" [(r/->AtomTerm "salad") (r/->AtomTerm "tomatoes")]) [(sut/ALREADY-NONVAR)])

  (is (valid-env? (sut/fill-env-for-term-with-spec test-env (r/->VarTerm "X") (r/make-spec:var))))
  (is (valid-env? (sut/fill-env-for-term-with-spec (sut/add-doms-to-node test-env (r/->VarTerm "X") (r/make-spec:any)) (r/->VarTerm "X") (r/make-spec:var))))
  (is (valid-env? (sut/fill-env-for-term-with-spec (sut/add-doms-to-node test-env (r/->VarTerm "X") (r/make-spec:var) (r/make-spec:var) (r/make-spec:any)) (r/->VarTerm "X") (r/make-spec:var))))
  (is ((complement valid-env?) (sut/fill-env-for-term-with-spec (sut/add-doms-to-node test-env (r/->VarTerm "X") (r/make-spec:ground)) (r/->VarTerm "X") (r/make-spec:var))))
  (is (valid-env? (sut/fill-env-for-term-with-spec test-env (r/->AnonVarTerm "X") (r/make-spec:var))))
  (is (valid-env? (sut/fill-env-for-term-with-spec (sut/add-doms-to-node test-env (r/->AnonVarTerm "X") (r/make-spec:any)) (r/->AnonVarTerm "X") (r/make-spec:var))))
  (is (valid-env? (sut/fill-env-for-term-with-spec (sut/add-doms-to-node test-env (r/->AnonVarTerm "X") (r/make-spec:var) (r/make-spec:var) (r/make-spec:any)) (r/->AnonVarTerm "X") (r/make-spec:var))))
  (is ((complement valid-env?) (sut/fill-env-for-term-with-spec (sut/add-doms-to-node test-env (r/->AnonVarTerm "X") (r/make-spec:ground)) (r/->AnonVarTerm "X") (r/make-spec:var)))))

(deftest fill-env-for-term-with-spec-test-atomic
  (are [term]
      (= [(r/make-spec:atomic)] (utils/get-dom-of-term (sut/fill-env-for-term-with-spec test-env term (r/make-spec:atomic)) term))
    (r/->VarTerm "X")
    (r/->AnonVarTerm "_0410"))
  (is (= [(r/make-spec:atom)] (utils/get-dom-of-term (sut/fill-env-for-term-with-spec test-env (r/->AtomTerm "cake") (r/make-spec:atomic)) (r/->AtomTerm "cake"))))
  (is (= [(r/make-spec:number)] (utils/get-dom-of-term (sut/fill-env-for-term-with-spec test-env (r/->NumberTerm 2) (r/make-spec:atomic)) (r/->NumberTerm 2))))
  (is (= [(r/make-spec:integer)] (utils/get-dom-of-term (sut/fill-env-for-term-with-spec test-env (r/->IntegerTerm 2) (r/make-spec:atomic)) (r/->IntegerTerm 2))))
  (is (= [(r/make-spec:float)] (utils/get-dom-of-term (sut/fill-env-for-term-with-spec test-env (r/->FloatTerm 2.0) (r/make-spec:atomic)) (r/->FloatTerm 2.0))))
  (are [term]
      (= [(sut/WRONG-TYPE term (r/make-spec:atom))] (utils/get-dom-of-term (sut/fill-env-for-term-with-spec test-env term (r/make-spec:atom)) term))
    (r/->ListTerm (r/->IntegerTerm 1) (r/->AtomicTerm "[]"))
    (r/->CompoundTerm "foo" [(r/->AtomTerm "foo")])
    )
  )

 
(deftest fill-env-for-term-with-spec-test-atom
  (are [term]
      (= [(r/make-spec:atom)] (utils/get-dom-of-term (sut/fill-env-for-term-with-spec test-env term (r/make-spec:atom)) term))
    (r/->AtomTerm "cake")
    (r/->VarTerm "X")
    (r/->AnonVarTerm "_0410")
    )
  (are [term]
      (= [(sut/WRONG-TYPE term (r/make-spec:atom))] (utils/get-dom-of-term (sut/fill-env-for-term-with-spec test-env term (r/make-spec:atom)) term))
    (r/->AtomicTerm "[]")
    (r/->ListTerm (r/->IntegerTerm 2) (r/->AtomicTerm "[]"))
    (r/->CompoundTerm "foo" [(r/->AtomTerm "foo")])
    )
  )

(deftest fill-env-for-term-with-spec-test-exact
  (are [term]
      (= [(r/make-spec:exact "cake")] (utils/get-dom-of-term (sut/fill-env-for-term-with-spec test-env term (r/make-spec:exact "cake")) term))
    (r/->AtomTerm "cake")
    (r/->VarTerm "X")
    (r/->AnonVarTerm "_0410")
    )
  (are [term]
        (= [(sut/WRONG-TYPE term (r/make-spec:exact "cake"))] (utils/get-dom-of-term (sut/fill-env-for-term-with-spec test-env term (r/make-spec:exact "cake")) term))
   (r/->ListTerm (r/->IntegerTerm 2) (r/->AtomicTerm "[]"))
   (r/->CompoundTerm "foo" [(r/->AtomTerm "foo")])
   (r/->AtomTerm "nocake")
   (r/->IntegerTerm 2)
   (r/->NumberTerm 2)
   (r/->FloatTerm 2.0)
   )
  )

(deftest fill-env-for-term-with-spec-test-number
  (are [term]
      (= [(r/make-spec:number)] (utils/get-dom-of-term (sut/fill-env-for-term-with-spec test-env term (r/make-spec:number)) term))
    (r/->VarTerm "X")
    (r/->AnonVarTerm "_234"))
  (is (= [(r/make-spec:integer)] (utils/get-dom-of-term (sut/fill-env-for-term-with-spec test-env (r/->IntegerTerm 2) (r/make-spec:number)) (r/->IntegerTerm 2))))
  (is (= [(r/make-spec:float)] (utils/get-dom-of-term (sut/fill-env-for-term-with-spec test-env (r/->FloatTerm 2.5) (r/make-spec:number)) (r/->FloatTerm 2.5))))
  (are [term]
      (= [(sut/WRONG-TYPE term (r/make-spec:number))] (utils/get-dom-of-term (sut/fill-env-for-term-with-spec test-env term (r/make-spec:number)) term))
    (r/->AtomTerm "no")
    (r/->ListTerm (r/->NumberTerm 2) (r/->AtomicTerm "[]"))
    (r/->CompoundTerm "node" [(r/->AtomTerm "hello")])
    )
  )


(deftest fill-env-for-term-with-spec-test-integer
  (are [term]
      (= [(r/make-spec:integer)] (utils/get-dom-of-term (sut/fill-env-for-term-with-spec test-env term (r/make-spec:integer)) term))
    (r/->IntegerTerm 42)
    (r/->NumberTerm 42)
    (r/->VarTerm "X")
    (r/->AnonVarTerm "_234"))

  (are [term]
      (= [(sut/WRONG-TYPE term (r/make-spec:integer))] (utils/get-dom-of-term (sut/fill-env-for-term-with-spec test-env term (r/make-spec:integer)) term))
    (r/->NumberTerm 3.14)
    (r/->FloatTerm 3.14)
    (r/->AtomTerm "no")
    (r/->ListTerm (r/->IntegerTerm 0) (r/->AtomicTerm "[]"))
    (r/->CompoundTerm "node" [(r/->AtomTerm "hello")]))
  )

(deftest fill-env-for-term-with-spec-test-float
  (are [term]
      (= [(r/make-spec:float)] (utils/get-dom-of-term (sut/fill-env-for-term-with-spec test-env term (r/make-spec:float)) term))
    (r/->FloatTerm 3.14)
    (r/->NumberTerm 1.0)
    (r/->VarTerm "X")
    (r/->AnonVarTerm "_234"))

  (are [term]
      (= [(sut/WRONG-TYPE term (r/make-spec:float))] (utils/get-dom-of-term (sut/fill-env-for-term-with-spec test-env term (r/make-spec:float)) term))
    (r/->NumberTerm 42)
    (r/->IntegerTerm 123)
    (r/->AtomTerm "no")
    (r/->ListTerm (r/->FloatTerm 0.0) (r/->AtomicTerm "[]"))
    (r/->CompoundTerm "node" [(r/->AtomTerm "hello")])
    )
  )

(deftest fill-env-for-term-with-spec-test-list
  (are [term]
      (= [(r/make-spec:list (r/make-spec:integer))] (utils/get-dom-of-term (sut/fill-env-for-term-with-spec test-env term (r/make-spec:list (r/make-spec:integer))) term))
    (r/->AtomicTerm "[]") ;; TODO: do we want `atomic` as an additional type?
    (r/->ListTerm (r/->IntegerTerm 2) (r/->ListTerm (r/->IntegerTerm 3) (r/->AtomicTerm "[]")))
    (r/->VarTerm "X")
    (r/->AnonVarTerm "_0410"))
  (are [term]
      (false? (valid-env? (sut/fill-env-for-term-with-spec test-env term (r/make-spec:list (r/make-spec:integer)))))
    (r/->ListTerm (r/->FloatTerm 2.5) (r/->AtomicTerm "[]")) ;TODO: atm the error is only visible in the HEAD term. Should it be at top level?
    (r/->CompoundTerm "foo" [(r/->AtomTerm "foo")])
    (r/->AtomTerm "nocake")
    (r/->AtomicTerm "cake")
    (r/->AtomicTerm 1)
    (r/->IntegerTerm 2)
    (r/->NumberTerm 2)
    (r/->FloatTerm 2.0)))

(deftest fill-env-for-term-with-spec-test-tuple
  (are [term]
      (= [(r/make-spec:tuple [(r/make-spec:integer) (r/make-spec:atom)])] (utils/get-dom-of-term (sut/fill-env-for-term-with-spec test-env term (r/make-spec:tuple [(r/make-spec:integer) (r/make-spec:atom)])) term))
    (r/->ListTerm  (r/->NumberTerm 2) (r/->ListTerm (r/->AtomTerm "cake") (r/->AtomicTerm "[]")))
    (r/->VarTerm "X")
    (r/->AnonVarTerm "_0410")
    )
  (are [term]
      (false? (valid-env? (sut/fill-env-for-term-with-spec test-env term (r/make-spec:tuple [(r/make-spec:integer) (r/make-spec:atom)]))))
    (r/->AtomicTerm "[]")
    (r/->ListTerm (r/->FloatTerm 2.5) (r/->AtomicTerm "[]")) ;TODO: atm the error is only visible in the HEAD term. Should it be at top level?
    (r/->CompoundTerm "foo" [(r/->AtomTerm "foo")])
    (r/->AtomTerm "nocake")
    (r/->IntegerTerm 2)
    (r/->NumberTerm 2)
    (r/->FloatTerm 2.0)))



(deftest fill-env-for-term-with-spec-test-compound
  (are [term]
      (= [(r/make-spec:compound "foo" [(r/make-spec:integer) (r/make-spec:atom)])] (utils/get-dom-of-term (sut/fill-env-for-term-with-spec test-env term (r/make-spec:compound "foo" [(r/make-spec:integer) (r/make-spec:atom)])) term))
    (r/->CompoundTerm "foo" [(r/->NumberTerm 2) (r/->AtomTerm "cake")])
    (r/->VarTerm "X")
    (r/->AnonVarTerm "_0410")
    )
  (are [term]
      (false? (valid-env? (sut/fill-env-for-term-with-spec test-env term (r/make-spec:compound "foo" [(r/make-spec:integer) (r/make-spec:atom)]))))
    (r/->AtomicTerm "[]")
    (r/->ListTerm  (r/->FloatTerm 2.5) (r/->AtomicTerm "[]")) ;TODO: atm the error is only visible in the HEAD term. Should it be at top level?
    (r/->CompoundTerm "foo" [(r/->AtomTerm "foo")])
    (r/->AtomTerm "nocake")
    (r/->IntegerTerm 2)
    (r/->NumberTerm 2)
    (r/->FloatTerm 2.0)))

(deftest fill-env-for-term-with-spec-test-specvar
  (let [term (r/->IntegerTerm 42)
        specvar (r/make-spec:specvar 0)
        expected-env (-> test-env
                         (sut/add-doms-to-node term specvar (r/make-spec:integer))
                         (sut/add-doms-to-node specvar (r/make-spec:integer)))]
    (is (= expected-env (sut/fill-env-for-term-with-spec test-env term specvar))))
  (let [term (r/->AtomTerm "nevegonnagiveyouup")
        specvar (r/make-spec:specvar 0)
        expected-env (-> test-env
                      (sut/add-doms-to-node term specvar (r/make-spec:atom))
                      (sut/add-doms-to-node specvar (r/make-spec:atom)))]
    (is (= expected-env (sut/fill-env-for-term-with-spec test-env term specvar))))
  (let [term (r/->ListTerm (r/->IntegerTerm 1) (r/->AtomicTerm "[]"))
        specvar (r/make-spec:specvar 0)
        spec-to-be-filled (r/make-spec:list specvar)
        expected-env (-> test-env
                         (sut/add-doms-to-node term spec-to-be-filled)
                         (sut/add-doms-to-node (r/->IntegerTerm 1) specvar (r/make-spec:integer))
                         (sut/add-doms-to-node specvar (r/make-spec:integer))
                         )
        actual-env (sut/fill-env-for-term-with-spec test-env term spec-to-be-filled)]
    (is (= expected-env actual-env))))

(deftest fill-env-for-term-with-spec-test-one-of
  (are [term spec expected-env] (= expected-env (sut/fill-env-for-term-with-spec test-env term spec))
    (r/->AtomTerm "hallohallo")
    (r/make-spec:one-of [(r/make-spec:integer) (r/make-spec:atomic)])
    (sut/add-doms-to-node test-env (r/->AtomTerm "hallohallo") (r/make-spec:atom))

    (r/->AtomTerm "hallohallo")
    (r/make-spec:one-of [(r/make-spec:atom) (r/make-spec:atomic)])
    (sut/add-doms-to-node test-env (r/->AtomTerm "hallohallo") (r/make-spec:atom))

    (r/->IntegerTerm 3)
    (r/make-spec:one-of [(r/make-spec:number) (r/make-spec:integer) (r/make-spec:atom)])
    (sut/add-doms-to-node test-env (r/->IntegerTerm 3) (r/make-spec:integer))

    (r/->IntegerTerm 3)
    (r/make-spec:one-of [(r/make-spec:integer) (r/make-spec:specvar 0)])
    (sut/add-doms-to-node test-env (r/->IntegerTerm 3) (r/make-spec:one-of [(r/make-spec:integer) (r/make-spec:and [(r/make-spec:specvar 0) (r/make-spec:integer)])]))

    (r/->ListTerm (r/->IntegerTerm 1) (r/->AtomicTerm "[]"))
    (r/make-spec:one-of [(r/make-spec:list (r/make-spec:number)) (r/make-spec:tuple [(r/make-spec:atomic)])])
    (sut/add-doms-to-node test-env
                          (r/->ListTerm (r/->IntegerTerm 1) (r/->AtomicTerm "[]"))
                          (r/make-spec:one-of [(r/make-spec:list (r/make-spec:number)) (r/make-spec:tuple [(r/make-spec:atomic)])]))
    ))

(deftest fill-env-for-term-with-spec-test-and
  (are [term spec expected-env]
      (= expected-env (sut/fill-env-for-term-with-spec test-env term spec))
    (r/->AtomTerm "hallohallo")
    (r/make-spec:and [(r/make-spec:atom) (r/make-spec:atomic)])
    (sut/add-doms-to-node test-env (r/->AtomTerm "hallohallo") (r/make-spec:atom))

    (r/->AtomTerm "hallohallo")
    (r/make-spec:and [(r/make-spec:any) (r/make-spec:atomic)])
    (sut/add-doms-to-node test-env (r/->AtomTerm "hallohallo") (r/make-spec:atom))

    (r/->IntegerTerm 3)
    (r/make-spec:and [(r/make-spec:ground) (r/make-spec:specvar 0)])
    (-> test-env
       (sut/add-doms-to-node (r/make-spec:specvar 0) (r/make-spec:integer))
       (sut/add-doms-to-node (r/->IntegerTerm 3) (r/make-spec:integer) (r/make-spec:specvar 0)))
    ))

(deftest fill-env-for-term-with-spec-test-user-defined
  (are [term spec expected-env] (= expected-env (sut/fill-env-for-term-with-spec test-env term spec))
    (r/->AtomTerm "empty")
    (r/make-spec:user-defined
     "tree" [(r/make-spec:integer)])
    (sut/add-doms-to-node
     test-env
     (r/->AtomTerm "empty")
     (r/make-spec:user-defined "tree" [(r/make-spec:integer)]) (r/make-spec:exact "empty"))

    (r/->AtomTerm "empty")
    (r/make-spec:user-defined "tree" [(r/make-spec:specvar 0)])
    (-> test-env
        (sut/add-doms-to-node
         (r/->AtomTerm "empty")
         (r/make-spec:user-defined "tree" [(r/make-spec:specvar 0)])
         (r/make-spec:exact "empty")))))


    
