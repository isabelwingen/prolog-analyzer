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
  (is (= [:a :b :c]
         (-> (uber/digraph)
             (uber/add-nodes-with-attrs [:x {:dom [:a]}])
             (sut/add-doms-to-node :x :b :c)
             (uber/attr :x :dom))))
  (is (= [:a :b]
         (-> (uber/digraph)
             (sut/add-doms-to-node :x :a :b)
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
    (r/make-term:any "anyanyany") (r/make-spec:any)
    (r/make-term:ground "anyanyany") (r/make-spec:ground)
    (r/make-term:nonvar "anyanyany") (r/make-spec:nonvar)
    (r/make-term:atom "cake") (r/make-spec:atom)
    (r/make-term:atomic "cake") (r/make-spec:atomic)
    (r/make-term:integer 42) (r/make-spec:integer)
    (r/make-term:number 23) (r/make-spec:number)
    (r/make-term:float 3.1415) (r/make-spec:float)
    (r/make-term:list (r/make-term:integer 1) (r/make-term:atomic "[]")) (r/make-spec:list (r/make-spec:any))
    (r/make-term:compound "wrap" [(r/make-term:atom "salad") (r/make-term:atom "tomatoes")]) (r/make-spec:compound "wrap" [(r/make-spec:any) (r/make-spec:any)])
    (r/make-term:var "X") (r/make-spec:var)
    (r/make-term:anon_var "_1603") (r/make-spec:var)))

(deftest fill-env-for-term-with-spec-test-ground
  (are [in out]
      (= [out] (utils/get-dom-of-term (sut/fill-env-for-term-with-spec test-env in (r/make-spec:ground)) in))
    (r/make-term:any "anyanyany") (r/make-spec:ground)
    (r/make-term:ground "anyanyany") (r/make-spec:ground)
    (r/make-term:nonvar "anyanyany") (r/make-spec:ground)
    (r/make-term:atom "bunker") (r/make-spec:atom)
    (r/make-term:atomic "cake") (r/make-spec:atomic)
    (r/make-term:integer 42) (r/make-spec:integer)
    (r/make-term:number 23) (r/make-spec:number)
    (r/make-term:float 3.1415) (r/make-spec:float)
    (r/make-term:list (r/make-term:integer 1) (r/make-term:atomic "[]")) (r/make-spec:list (r/make-spec:ground))
    (r/make-term:compound "wrap" [(r/make-term:atom "salad") (r/make-term:atom "tomatoes")]) (r/make-spec:compound "wrap" [(r/make-spec:ground) (r/make-spec:ground)])
    (r/make-term:var "X") (r/make-spec:ground)
    (r/make-term:anon_var "_1603") (r/make-spec:ground)
    ))


(deftest fill-env-for-term-with-spec-test-nonvar
  (are [in out]
      (= [out] (utils/get-dom-of-term (sut/fill-env-for-term-with-spec test-env in (r/make-spec:nonvar)) in))
    (r/make-term:any "anyanyany") (r/make-spec:nonvar)
    (r/make-term:ground "anyanyany") (r/make-spec:ground)
    (r/make-term:nonvar "anyanyany") (r/make-spec:nonvar)
    (r/make-term:atom "cake") (r/make-spec:atom)
    (r/make-term:atomic "cake") (r/make-spec:atomic)
    (r/make-term:integer 42) (r/make-spec:integer)
    (r/make-term:number 23) (r/make-spec:number)
    (r/make-term:float 3.1415) (r/make-spec:float)
    (r/make-term:list (r/make-term:integer 1) (r/make-term:atomic "[]")) (r/make-spec:list (r/make-spec:any))
    (r/make-term:compound "wrap" [(r/make-term:atom "salad") (r/make-term:atom "tomatoes")]) (r/make-spec:compound "wrap" [(r/make-spec:any) (r/make-spec:any)])
    (r/make-term:var "X") (r/make-spec:nonvar)
    (r/make-term:anon_var "_1603") (r/make-spec:nonvar)
    ))


(deftest fill-env-for-term-with-spec-test-var
  ;; WITH NON-EMPTY-DOM
  (are [in expected]
      (= expected (utils/get-dom-of-term (-> test-env
                                             (sut/fill-env-for-term-with-spec in (r/make-spec:any))
                                             (sut/fill-env-for-term-with-spec  in (r/make-spec:var))) in))
    (r/make-term:ground "anyanyany") [(r/make-spec:ground) (sut/ALREADY-NONVAR)]
    (r/make-term:nonvar "nonononvar") [(r/make-spec:nonvar) (sut/ALREADY-NONVAR)]
    (r/make-term:atom "batman") [(r/make-spec:atom) (sut/ALREADY-NONVAR)]
    (r/make-term:atomic "cake") [(r/make-spec:atomic) (sut/ALREADY-NONVAR)]
    (r/make-term:integer 42) [(r/make-spec:integer) (sut/ALREADY-NONVAR)]
    (r/make-term:number 23) [(r/make-spec:number) (sut/ALREADY-NONVAR)]
    (r/make-term:float 3.1415) [(r/make-spec:float) (sut/ALREADY-NONVAR)]
    (r/make-term:list (r/make-term:integer 1) (r/make-term:atomic "[]")) [(r/make-spec:list (r/make-spec:any)) (sut/ALREADY-NONVAR)]
    (r/make-term:compound "wrap" [(r/make-term:atom "salad") (r/make-term:atom "tomatoes")]) [(r/make-spec:compound "wrap" [(r/make-spec:any) (r/make-spec:any)]) (sut/ALREADY-NONVAR)])
  
  ;; WITH EMPTY DOM
  (are [in expected]
      (= expected (utils/get-dom-of-term (sut/fill-env-for-term-with-spec test-env in (r/make-spec:var)) in))
    (r/make-term:ground "anyanyany") [(sut/ALREADY-NONVAR)]
    (r/make-term:nonvar "nonononvar") [(sut/ALREADY-NONVAR)]
    (r/make-term:atom "batman") [(sut/ALREADY-NONVAR)]
    (r/make-term:atomic "cake") [(sut/ALREADY-NONVAR)]
    (r/make-term:integer 42) [(sut/ALREADY-NONVAR)]
    (r/make-term:number 23) [(sut/ALREADY-NONVAR)]
    (r/make-term:float 3.1415) [(sut/ALREADY-NONVAR)]
    (r/make-term:list (r/make-term:integer 1) (r/make-term:atomic "[]")) [(sut/ALREADY-NONVAR)]
    (r/make-term:compound "wrap" [(r/make-term:atom "salad") (r/make-term:atom "tomatoes")]) [(sut/ALREADY-NONVAR)])

  (is (valid-env? (sut/fill-env-for-term-with-spec test-env (r/make-term:var "X") (r/make-spec:var))))
  (is (valid-env? (sut/fill-env-for-term-with-spec (sut/add-doms-to-node test-env (r/make-term:var "X") (r/make-spec:any)) (r/make-term:var "X") (r/make-spec:var))))
  (is (valid-env? (sut/fill-env-for-term-with-spec (sut/add-doms-to-node test-env (r/make-term:var "X") (r/make-spec:var) (r/make-spec:var) (r/make-spec:any)) (r/make-term:var "X") (r/make-spec:var))))
  (is ((complement valid-env?) (sut/fill-env-for-term-with-spec (sut/add-doms-to-node test-env (r/make-term:var "X") (r/make-spec:ground)) (r/make-term:var "X") (r/make-spec:var))))
  (is (valid-env? (sut/fill-env-for-term-with-spec test-env (r/make-term:anon_var "X") (r/make-spec:var))))
  (is (valid-env? (sut/fill-env-for-term-with-spec (sut/add-doms-to-node test-env (r/make-term:anon_var "X") (r/make-spec:any)) (r/make-term:anon_var "X") (r/make-spec:var))))
  (is (valid-env? (sut/fill-env-for-term-with-spec (sut/add-doms-to-node test-env (r/make-term:anon_var "X") (r/make-spec:var) (r/make-spec:var) (r/make-spec:any)) (r/make-term:anon_var "X") (r/make-spec:var))))
  (is ((complement valid-env?) (sut/fill-env-for-term-with-spec (sut/add-doms-to-node test-env (r/make-term:anon_var "X") (r/make-spec:ground)) (r/make-term:anon_var "X") (r/make-spec:var)))))

(deftest fill-env-for-term-with-spec-test-atomic
  (are [term]
      (= [(r/make-spec:atomic)] (utils/get-dom-of-term (sut/fill-env-for-term-with-spec test-env term (r/make-spec:atomic)) term))
    (r/make-term:atomic "cake")
    (r/make-term:atomic "1603")
    (r/make-term:var "X")
    (r/make-term:anon_var "_0410"))
  (is (= [(r/make-spec:atom)] (utils/get-dom-of-term (sut/fill-env-for-term-with-spec test-env (r/make-term:atom "cake") (r/make-spec:atomic)) (r/make-term:atom "cake"))))
  (is (= [(r/make-spec:number)] (utils/get-dom-of-term (sut/fill-env-for-term-with-spec test-env (r/make-term:number 2) (r/make-spec:atomic)) (r/make-term:number 2))))
  (is (= [(r/make-spec:integer)] (utils/get-dom-of-term (sut/fill-env-for-term-with-spec test-env (r/make-term:integer 2) (r/make-spec:atomic)) (r/make-term:integer 2))))
  (is (= [(r/make-spec:float)] (utils/get-dom-of-term (sut/fill-env-for-term-with-spec test-env (r/make-term:float 2.0) (r/make-spec:atomic)) (r/make-term:float 2.0))))
  (are [term]
      (= [(sut/WRONG-TYPE term (r/make-spec:atom))] (utils/get-dom-of-term (sut/fill-env-for-term-with-spec test-env term (r/make-spec:atom)) term))
    (r/make-term:list (r/make-term:integer 1) (r/make-term:atomic "[]"))
    (r/make-term:compound "foo" [(r/make-term:atom "foo")])
    )
  )

 
(deftest fill-env-for-term-with-spec-test-atom
  (are [term]
      (= [(r/make-spec:atom)] (utils/get-dom-of-term (sut/fill-env-for-term-with-spec test-env term (r/make-spec:atom)) term))
    (r/make-term:atom "cake")
    (r/make-term:atomic "cake")
    (r/make-term:var "X")
    (r/make-term:anon_var "_0410")
    )
  (are [term]
      (= [(sut/WRONG-TYPE term (r/make-spec:atom))] (utils/get-dom-of-term (sut/fill-env-for-term-with-spec test-env term (r/make-spec:atom)) term))
    (r/make-term:atomic "[]")
    (r/make-term:atomic "1603")
    (r/make-term:list (r/make-term:integer 2) (r/make-term:atomic "[]"))
    (r/make-term:compound "foo" [(r/make-term:atom "foo")])
    )
  )

(deftest fill-env-for-term-with-spec-test-exact
  (are [term]
      (= [(r/make-spec:exact "cake")] (utils/get-dom-of-term (sut/fill-env-for-term-with-spec test-env term (r/make-spec:exact "cake")) term))
    (r/make-term:atomic "cake")
    (r/make-term:atom "cake")
    (r/make-term:var "X")
    (r/make-term:anon_var "_0410")
    )
  (are [term]
        (= [(sut/WRONG-TYPE term (r/make-spec:exact "cake"))] (utils/get-dom-of-term (sut/fill-env-for-term-with-spec test-env term (r/make-spec:exact "cake")) term))
   (r/make-term:list (r/make-term:integer 2) (r/make-term:atomic "[]"))
   (r/make-term:compound "foo" [(r/make-term:atom "foo")])
   (r/make-term:atom "nocake")
   (r/make-term:atomic 1)
   (r/make-term:integer 2)
   (r/make-term:number 2)
   (r/make-term:float 2.0)
   )
  )

(deftest fill-env-for-term-with-spec-test-number
  (are [term]
      (= [(r/make-spec:number)] (utils/get-dom-of-term (sut/fill-env-for-term-with-spec test-env term (r/make-spec:number)) term))
    (r/make-term:atomic "3.14")
    (r/make-term:atomic "100")
    (r/make-term:var "X")
    (r/make-term:anon_var "_234"))
  (is (= [(r/make-spec:integer)] (utils/get-dom-of-term (sut/fill-env-for-term-with-spec test-env (r/make-term:integer 2) (r/make-spec:number)) (r/make-term:integer 2))))
  (is (= [(r/make-spec:float)] (utils/get-dom-of-term (sut/fill-env-for-term-with-spec test-env (r/make-term:float 2.5) (r/make-spec:number)) (r/make-term:float 2.5))))
  (are [term]
      (= [(sut/WRONG-TYPE term (r/make-spec:number))] (utils/get-dom-of-term (sut/fill-env-for-term-with-spec test-env term (r/make-spec:number)) term))
    (r/make-term:atomic "no")
    (r/make-term:atom "no")
    (r/make-term:list (r/make-term:number 2) (r/make-term:atomic "[]"))
    (r/make-term:compound "node" [(r/make-term:atom "hello")])
    )
  )


(deftest fill-env-for-term-with-spec-test-integer
  (are [term]
      (= [(r/make-spec:integer)] (utils/get-dom-of-term (sut/fill-env-for-term-with-spec test-env term (r/make-spec:integer)) term))
    (r/make-term:integer 42)
    (r/make-term:number 42)
    (r/make-term:atomic "42")
    (r/make-term:var "X")
    (r/make-term:anon_var "_234"))

  (are [term]
      (= [(sut/WRONG-TYPE term (r/make-spec:integer))] (utils/get-dom-of-term (sut/fill-env-for-term-with-spec test-env term (r/make-spec:integer)) term))
    (r/make-term:number 3.14)
    (r/make-term:float 3.14)
    (r/make-term:atomic "3.14")
    (r/make-term:atomic "no")
    (r/make-term:atom "no")
    (r/make-term:list (r/make-term:integer 0) (r/make-term:atomic "[]"))
    (r/make-term:compound "node" [(r/make-term:atom "hello")]))
  )

(deftest fill-env-for-term-with-spec-test-float
  (are [term]
      (= [(r/make-spec:float)] (utils/get-dom-of-term (sut/fill-env-for-term-with-spec test-env term (r/make-spec:float)) term))
    (r/make-term:float 3.14)
    (r/make-term:number 1.0)
    (r/make-term:atomic "3.14")
    (r/make-term:var "X")
    (r/make-term:anon_var "_234"))

  (are [term]
      (= [(sut/WRONG-TYPE term (r/make-spec:float))] (utils/get-dom-of-term (sut/fill-env-for-term-with-spec test-env term (r/make-spec:float)) term))
    (r/make-term:number 42)
    (r/make-term:integer 123)
    (r/make-term:atomic "1")
    (r/make-term:atomic "no")
    (r/make-term:atom "no")
    (r/make-term:list (r/make-term:float 0.0) (r/make-term:atomic "[]"))
    (r/make-term:compound "node" [(r/make-term:atom "hello")])
    )
  )

(deftest fill-env-for-term-with-spec-test-list
  (are [term]
      (= [(r/make-spec:list (r/make-spec:integer))] (utils/get-dom-of-term (sut/fill-env-for-term-with-spec test-env term (r/make-spec:list (r/make-spec:integer))) term))
    (r/make-term:atomic "[]") ;; TODO: do we want `atomic` as an additional type?
    (r/make-term:list (r/make-term:integer 2) (r/make-term:list (r/make-term:integer 3) (r/make-term:atomic "[]")))
    (r/make-term:var "X")
    (r/make-term:anon_var "_0410"))
  (are [term]
      (false? (valid-env? (sut/fill-env-for-term-with-spec test-env term (r/make-spec:list (r/make-spec:integer)))))
    (r/make-term:list (r/make-term:float 2.5) (r/make-term:atomic "[]")) ;TODO: atm the error is only visible in the HEAD term. Should it be at top level?
    (r/make-term:compound "foo" [(r/make-term:atom "foo")])
    (r/make-term:atom "nocake")
    (r/make-term:atomic "cake")
    (r/make-term:atomic 1)
    (r/make-term:integer 2)
    (r/make-term:number 2)
    (r/make-term:float 2.0)))

(deftest fill-env-for-term-with-spec-test-tuple
  (are [term]
      (= [(r/make-spec:tuple [(r/make-spec:integer) (r/make-spec:atom)])] (utils/get-dom-of-term (sut/fill-env-for-term-with-spec test-env term (r/make-spec:tuple [(r/make-spec:integer) (r/make-spec:atom)])) term))
    (r/make-term:list (r/make-term:integer 2) (r/make-term:list (r/make-term:atomic "cake") (r/make-term:atomic "[]")))
    (r/make-term:list  (r/make-term:number 2) (r/make-term:list (r/make-term:atom "cake") (r/make-term:atomic "[]")))
    (r/make-term:var "X")
    (r/make-term:anon_var "_0410")
    )
  (are [term]
      (false? (valid-env? (sut/fill-env-for-term-with-spec test-env term (r/make-spec:tuple [(r/make-spec:integer) (r/make-spec:atom)]))))
    (r/make-term:atomic "[]")
    (r/make-term:list (r/make-term:float 2.5) (r/make-term:atomic "[]")) ;TODO: atm the error is only visible in the HEAD term. Should it be at top level?
    (r/make-term:compound "foo" [(r/make-term:atom "foo")])
    (r/make-term:atom "nocake")
    (r/make-term:atomic "cake")
    (r/make-term:atomic 1)
    (r/make-term:integer 2)
    (r/make-term:number 2)
    (r/make-term:float 2.0)))



(deftest fill-env-for-term-with-spec-test-compound
  (are [term]
      (= [(r/make-spec:compound "foo" [(r/make-spec:integer) (r/make-spec:atom)])] (utils/get-dom-of-term (sut/fill-env-for-term-with-spec test-env term (r/make-spec:compound "foo" [(r/make-spec:integer) (r/make-spec:atom)])) term))
    (r/make-term:compound "foo" [(r/make-term:integer 2) (r/make-term:atomic "cake")])
    (r/make-term:compound "foo" [(r/make-term:number 2) (r/make-term:atom "cake")])
    (r/make-term:var "X")
    (r/make-term:anon_var "_0410")
    )
  (are [term]
      (false? (valid-env? (sut/fill-env-for-term-with-spec test-env term (r/make-spec:compound "foo" [(r/make-spec:integer) (r/make-spec:atom)]))))
    (r/make-term:atomic "[]")
    (r/make-term:list  (r/make-term:float 2.5) (r/make-term:atomic "[]")) ;TODO: atm the error is only visible in the HEAD term. Should it be at top level?
    (r/make-term:compound "foo" [(r/make-term:atom "foo")])
    (r/make-term:compound "not-foo" [(r/make-term:integer 2) (r/make-term:atomic "cake")])
    (r/make-term:compound "foo" [(r/make-term:float 2.5) (r/make-term:atomic "cake")])
    (r/make-term:atom "nocake")
    (r/make-term:atomic "cake")
    (r/make-term:atomic 1)
    (r/make-term:integer 2)
    (r/make-term:number 2)
    (r/make-term:float 2.0)))

(deftest fill-env-for-term-with-spec-test-specvar
  (let [term (r/make-term:integer 42)
        specvar (r/make-spec:specvar 0)
        expected-env (-> test-env
                         (sut/add-doms-to-node term specvar (r/make-spec:integer))
                         (sut/add-doms-to-node specvar (r/make-spec:integer)))]
    (is (= expected-env (sut/fill-env-for-term-with-spec test-env term specvar))))
  (let [term (r/make-term:atom "nevegonnagiveyouup")
        specvar (r/make-spec:specvar 0)
        expected-env (-> test-env
                      (sut/add-doms-to-node term specvar (r/make-spec:atom))
                      (sut/add-doms-to-node specvar (r/make-spec:atom)))]
    (is (= expected-env (sut/fill-env-for-term-with-spec test-env term specvar))))
  (let [term (r/make-term:list (r/make-term:integer 1) (r/make-term:atomic "[]"))
        specvar (r/make-spec:specvar 0)
        spec-to-be-filled (r/make-spec:list specvar)
        expected-env (-> test-env
                         (sut/add-doms-to-node term spec-to-be-filled)
                         (sut/add-doms-to-node (r/make-term:integer 1) specvar (r/make-spec:integer))
                         (sut/add-doms-to-node specvar (r/make-spec:integer))
                         )
        actual-env (sut/fill-env-for-term-with-spec test-env term spec-to-be-filled)]
    (is (= expected-env actual-env))))

(deftest fill-env-for-term-with-spec-test-one-of
  (are [term spec expected-env] (= expected-env (sut/fill-env-for-term-with-spec test-env term spec))
    (r/make-term:atom "hallohallo")
    (r/make-spec:one-of [(r/make-spec:integer) (r/make-spec:atomic)])
    (sut/add-doms-to-node test-env (r/make-term:atom "hallohallo") (r/make-spec:atom))

    (r/make-term:atom "hallohallo")
    (r/make-spec:one-of [(r/make-spec:atom) (r/make-spec:atomic)])
    (sut/add-doms-to-node test-env (r/make-term:atom "hallohallo") (r/make-spec:atom))

    (r/make-term:integer 3)
    (r/make-spec:one-of [(r/make-spec:number) (r/make-spec:integer) (r/make-spec:atom)])
    (sut/add-doms-to-node test-env (r/make-term:integer 3) (r/make-spec:integer))

    (r/make-term:integer 3)
    (r/make-spec:one-of [(r/make-spec:integer) (r/make-spec:specvar 0)])
    (sut/add-doms-to-node test-env (r/make-term:integer 3) (r/make-spec:one-of [(r/make-spec:integer) (r/make-spec:and [(r/make-spec:specvar 0) (r/make-spec:integer)])]))

    (r/make-term:list (r/make-term:integer 1) (r/make-term:atomic "[]"))
    (r/make-spec:one-of [(r/make-spec:list (r/make-spec:number)) (r/make-spec:tuple [(r/make-spec:atomic)])])
    (sut/add-doms-to-node test-env
                          (r/make-term:list (r/make-term:integer 1) (r/make-term:atomic "[]"))
                          (r/make-spec:one-of [(r/make-spec:list (r/make-spec:number)) (r/make-spec:tuple [(r/make-spec:atomic)])]))
    ))

(deftest fill-env-for-term-with-spec-test-and
  (are [term spec expected-env]
      (= expected-env (sut/fill-env-for-term-with-spec test-env term spec))
    (r/make-term:atom "hallohallo")
    (r/make-spec:and [(r/make-spec:atom) (r/make-spec:atomic)])
    (sut/add-doms-to-node test-env (r/make-term:atom "hallohallo") (r/make-spec:atom))

    (r/make-term:atom "hallohallo")
    (r/make-spec:and [(r/make-spec:any) (r/make-spec:atomic)])
    (sut/add-doms-to-node test-env (r/make-term:atom "hallohallo") (r/make-spec:atom))

    (r/make-term:integer 3)
    (r/make-spec:and [(r/make-spec:ground) (r/make-spec:specvar 0)])
    (-> test-env
        (sut/add-doms-to-node (r/make-spec:specvar 0) (r/make-spec:integer))
        (sut/add-doms-to-node (r/make-term:integer 3) (r/make-spec:integer) (r/make-spec:specvar 0)))))

(deftest fill-env-for-term-with-spec-test-user-defined
  (are [term spec expected-env] (= expected-env (sut/fill-env-for-term-with-spec test-env term spec))
    (r/make-term:atom "empty")
    (r/make-spec:user-defined
     "tree" [(r/make-spec:integer)])
    (sut/add-doms-to-node
     test-env
     (r/make-term:atom "empty")
     (r/make-spec:user-defined "tree" [(r/make-spec:integer)]) (r/make-spec:exact "empty"))

    (r/make-term:atom "empty")
    (r/make-spec:user-defined "tree" [(r/make-spec:specvar 0)])
    (-> test-env
        (sut/add-doms-to-node
         (r/make-term:atom "empty")
         (r/make-spec:user-defined "tree" [(r/make-spec:specvar 0)])
         (r/make-spec:exact "empty")))))


    
