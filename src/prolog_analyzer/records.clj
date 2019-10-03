(ns prolog-analyzer.records
  (:require [clojure.tools.logging :as log]
            [clojure.tools.namespace.repl :refer [refresh]]
            [instaparse.core :as insta]
            [clojure.string]))


(def INTEGER :integer)
(def FLOAT :float)
(def NUMBER :number)
(def EXACT :exact)
(def ATOM :atom)
(def STRING :string)
(def ATOMIC :atomic)
(def COMPOUND :compound)
(def LIST :list)
(def EMPTYLIST :empty-list)
(def TUPLE :tuple)
(def GROUND :ground)
(def NONVAR :nonvar)
(def VAR :var)

(def USERDEFINED :user-defined)
(def SPECVAR :specvar)
(def ERROR :error)

(def AND :and)
(def OR :one-of)

(def ANY :any)
(def PLACEHOLDER :placeholder)

(declare to-arglist)
(declare empty-list?)
(declare spec-type)
(declare term-type)
(declare ->AndSpec)
(declare has-specvars)
(declare var-spec?)
(declare create-incomplete-list-spec)

(defprotocol printable
  (to-string [x]))

(defprotocol spec
  (spec-type [spec])
  (length [x]))

(defprotocol term
  (term-type [term])
  (initial-spec [term]))

(defn is-term? [x]
  (satisfies? term x))

(defn is-spec? [x]
  (satisfies? spec x))


(defrecord AnySpec []
  spec
  (spec-type [spec] ANY)
  (length [x] 1)
  printable
  (to-string [x] "Any"))


(defrecord ErrorSpec [reason]
  spec
  (spec-type [spec] ERROR)
  (length [x] (count reason))
  printable
  (to-string [x] (str "ERROR: " reason)))

(defn DISJOINT
  ([msg] (->ErrorSpec msg))
  ([a b]
   (let [strings (->> [a b]
                      (map to-string)
                      sort
                      (clojure.string/join " and "))]
     (->ErrorSpec (str "No valid intersection of " strings)))))

(defrecord VarSpec []
  spec
  (spec-type [spec] VAR)
  (length [x] 1)
  printable
  (to-string [x] "Var"))

(defrecord EmptyListSpec []
  spec
  (spec-type [spec] EMPTYLIST)
  (length [x] 1)
  printable
  (to-string [x] "EmptyList"))

(defrecord StringSpec []
  spec
  (spec-type [spec] STRING)
  (length [x] 1)
  printable
  (to-string [x] "String"))

(defrecord AtomSpec []
  spec
  (spec-type [spec] ATOM)
  (length [x] 1)
  printable
  (to-string [x] "Atom"))

(defrecord IntegerSpec []
  spec
  (spec-type [spec] INTEGER)
  (length [x] 1)
  printable
  (to-string [x] "Integer"))

(defrecord FloatSpec []
  spec
  (spec-type [spec] FLOAT)
  (length [x] 1)
  printable
  (to-string [x] "Float"))

(defrecord NumberSpec []
  spec
  (spec-type [spec] NUMBER)
  (length [x] 1)
  printable
  (to-string [x] "Number"))

(defrecord AtomicSpec []
  spec
  (spec-type [spec] ATOMIC)
  (length [x] 1)
  printable
  (to-string [x] "Atomic"))

(defrecord ExactSpec [value]
  spec
  (spec-type [spec] EXACT)
  (length [x] 1)
  printable
  (to-string [x] (str "Exact(" value ")")))

(defn get-head-and-tail [term]
  {:head (first (.arglist term)) :tail (second (.arglist term))})

(defrecord ListSpec [type]
  spec
  (spec-type [spec] LIST)
  (length [x] 2)
  printable
  (to-string [x] (str "List(" (to-string type) ")")))

(defrecord TupleSpec [arglist]
  spec
  (spec-type [spec] TUPLE)
  (length [x] (apply + 1 (map length arglist)))
  printable
  (to-string [x] (str "Tuple(" (to-arglist arglist) ")")))


(defrecord CompoundSpec [functor arglist]
  spec
  (spec-type [spec] COMPOUND)
  (length [x] (apply + 1 (map length arglist)))
  printable
  (to-string [x] (if (nil? functor) "Compound" (str "Compound(" functor "(" (to-arglist arglist) "))"))))


(defrecord GroundSpec []
  spec
  (spec-type [spec] GROUND)
  (length [x] 1)
  printable
  (to-string [x] "Ground"))

(defrecord AndSpec [arglist]
  spec
  (spec-type [spec] AND)
  (length [x] (apply + 1 (map length arglist)))
  printable
  (to-string [x] (str "And(" (to-arglist arglist) ")")))

(defrecord OneOfSpec [arglist]
  spec
  (spec-type [spec] OR)
  (length [x] (apply + 1 (map length arglist)))
  printable
  (to-string [x] (str "OneOf(" (to-arglist arglist) ")")))

(defrecord UserDefinedSpec [name]
  spec
  (spec-type [spec] USERDEFINED)
  (length [x] (if (contains? x :arglist)
                (apply + 1 (map length (:arglist x)))
                2))
  printable
  (to-string [x] (if (contains? x :arglist)
                   (str name "(" (to-arglist (:arglist x)) ")")
                   (str name))))

(defrecord NonvarSpec []
  spec
  (spec-type [spec] NONVAR)
  (length [x] 1)
  printable
  (to-string [x] "Nonvar"))

(defrecord PlaceholderSpec [name]
  spec
  (spec-type [spec] PLACEHOLDER)
  (length [x] 2)
  printable
  (to-string [x] (if (contains? x :alias)
                   (str "Placeholder(" name "):" (to-string (:alias x)))
                   (str "Placeholder(" name ")"))))


(defrecord SpecvarSpec [name]
  spec
  (spec-type [spec] SPECVAR)
  (length [x] 2)
  printable
  (to-string [x] (str "Specvar(" name ")")))

(defrecord VarTerm [name]
  term
  (term-type [term] VAR)
  (initial-spec [term] (->AnySpec))
  printable
  (to-string [x] (str name)))

(defrecord AtomTerm [term]
  term
  (term-type [term] ATOM)
  (initial-spec [term] (->AtomSpec))
  printable
  (to-string [x] (if (nil? term) "<atom>" (str term))))

(defrecord StringTerm [term]
  term
  (term-type [term] STRING)
  (initial-spec [term] (->StringSpec))
  printable
  (to-string [x] (str "\"" term "\"")))


(defrecord EmptyListTerm []
  term
  (term-type [term] EMPTYLIST)
  (initial-spec [term] (->EmptyListSpec))
  printable
  (to-string [x] "[]"))


(defrecord IntegerTerm [value]
  term
  (term-type [term] INTEGER)
  (initial-spec [term] (->IntegerSpec))
  printable
  (to-string [x] (str value)))

(defrecord FloatTerm [value]
  term
  (term-type [term] FLOAT)
  (initial-spec [term] (->FloatSpec))
  printable
  (to-string [x] (str value)))

(defrecord NumberTerm [value]
  term
  (term-type [term] NUMBER)
  (initial-spec [term] (->NumberSpec))
  printable
  (to-string [x] (str value)))

(defn get-elements-of-list [{head :head tail :tail}]
  (if (nil? tail)
    (list)
    (conj (get-elements-of-list tail) head)))

(defn- unpack [spec]
  (loop [arglist (:arglist spec)
         res #{}]
    (if-let [f (first arglist)]
      (if (= OR (spec-type f))
        (recur (concat (rest arglist) (:arglist f)) res)
        (recur (rest arglist) (conj res f)))
      (->OneOfSpec (set res)))))

(defrecord ListTerm [head tail]
  term
  (term-type [term] LIST)
  (initial-spec [term]
    (let [head-dom (initial-spec head)
          tail-dom (initial-spec tail)]
      (case (spec-type tail-dom)
        :list (->ListSpec (->OneOfSpec (hash-set head-dom (:type tail-dom))))
        :tuple (->TupleSpec (vec (cons head-dom (:arglist tail-dom))))
        :empty-list (->TupleSpec [head-dom])
        (->CompoundSpec "." [head-dom tail-dom]))))
  printable
  (to-string [x]
    (case (term-type tail)
           :atomic (str "[" (to-string head) "]")
           :empty-list (str "[" (to-string head) "]")
           :var (str "[" (to-string head) "|" (to-string tail) "]")
           :list (str "[" (to-arglist (get-elements-of-list x)) "]")
           (str "[" (to-string head) "|" (to-string tail) "]"))))


(defrecord CompoundTerm [functor arglist]
  term
  (term-type [term] COMPOUND)
  (initial-spec [term] (->CompoundSpec functor (map initial-spec arglist)))
  printable
  (to-string [x] (str "Compound(" functor "(" (to-arglist arglist) "))")))

(defrecord ShouldNotHappenTerm [term]
  term
  (term-type [term] ERROR)
  (initial-spec [term] (->ErrorSpec (str "This term should not exists: " term)))
  printable
  (to-string [x] (str "ERROR: " term)))



(defn- singleton? [singletons term]
  (contains? (set singletons) term))


(defn map-to-term
  ([input-m] (map-to-term :nothing input-m))
  ([singletons input-m]
   (if (not (map? input-m))
     (log/error (str input-m) " is " (type input-m))
     (let [m (dissoc input-m :type)]
       (case (:type input-m)
         :var (map->VarTerm m)
         :atom (map->AtomTerm m)
         :number (map->NumberTerm m)
         :integer (map->IntegerTerm m)
         :float (map->FloatTerm m)
         :list (map->ListTerm (-> m
                                  (update :head (partial map-to-term singletons))
                                  (update :tail (partial map-to-term singletons))))
         :compound (map->CompoundTerm (update m :arglist #(map (partial map-to-term singletons) %)))
         :empty-list (->EmptyListTerm)
         :string (map->StringTerm m)
         :should-not-happen (map->ShouldNotHappenTerm m)
         (do
           (log/error "No case for" input-m "in map-to-term")
           (->AtomTerm "ERROR")))))))

(defn map-to-spec [m]
  (case (:spec m)
    :var (map->VarSpec m)
    :any (map->AnySpec m)
    :ground (map->GroundSpec m)
    :nonvar (map->NonvarSpec m)
    :atom (map->AtomSpec m)
    :exact (map->ExactSpec m)
    :atomic (map->AtomicSpec m)
    :number (map->NumberSpec m)
    :integer (map->IntegerSpec m)
    :float (map->FloatSpec m)
    :string (->StringSpec)
    :list (map->ListSpec (update m :type map-to-spec))
    :tuple (map->TupleSpec (update m :arglist (partial map map-to-spec)))
    :compound (map->CompoundSpec (update m :arglist (partial map map-to-spec)))
    :and (map->AndSpec (-> m
                           (update :arglist (partial map map-to-spec))
                           (update :arglist set)))
    :one-of (map->OneOfSpec (-> m
                                (update :arglist (partial map map-to-spec))
                                (update :arglist set)))
    :user-defined (map->UserDefinedSpec (update m :arglist (partial map map-to-spec)))
    :error-spec (map->ErrorSpec m)
    :emptylist (->EmptyListSpec)
    (do
      (log/error "No case for" m "in map-to-spec")
      (->AnySpec))))


(defn make-spec:user-defined
  ([name] (->UserDefinedSpec name))
  ([name arglist] (-> (->UserDefinedSpec name)
                      (assoc :arglist arglist))))


(defn to-arglist [list]
  (clojure.string/join ", " (map to-string list)))

(defn incomplete-list-spec? [spec]
  (and
   (= COMPOUND (spec-type spec))
   (= "." (:functor spec))
   (= 2 (count (:arglist spec)))))


(defn to-spec [string]
  (case string
    "Any" (->AnySpec)
    "Ground" (->GroundSpec)
    "Integer" (->IntegerSpec)
    "Float" (->FloatSpec)
    "Number" (->NumberSpec)
    "Atomic" (->AtomicSpec)
    "Atom" (->AtomSpec)
    "Var" (->VarSpec)
    "Nonvar" (->NonvarSpec)
    "EmptyList" (->EmptyListSpec)))

(def p
  (insta/parser
   "<Spec> = Simple | Complex
    <Complex> = Or | And | List | Tuple | Exact | Compound | Userdef
    <Simple> = Any | Integer | Float | Atom | Atomic | Number | EmptyList | Var | Nonvar | Ground
    <Arglist> = <''> | Spec (<','> <' '>* Spec)*
    Any = <'Any'>
    Integer = <'Integer'>
    Float = <'Float'>
    Number = <'Number'>
    Atom = <'Atom'>
    Atomic = <'Atomic'>
    Ground = <'Ground'>
    Var = <'Var'>
    Nonvar = <'Nonvar'>
    EmptyList = <'EmptyList'>
    <Letters> = #'[a-z_\\.]*'
    Exact = <'Exact('> Letters <')'>
    List = <'List('> Spec <')'>
    Tuple = <'Tuple('> Arglist <')'>
    Functor = Letters
    Compound = <'Compound('> Functor <'('> Arglist <'))'>
    Or = <'OneOf('> Arglist <')'>
    And = <'And('> Arglist <')'>
    Userdef = Functor | Functor <'('> Arglist <')'>"
   :output-format :enlive))

(defmulti to-spec (fn [{tag :tag}] tag))

(defmethod to-spec :Integer [_]
  (->IntegerSpec))
(defmethod to-spec :Float [_]
  (->FloatSpec))
(defmethod to-spec :Number [_]
  (->NumberSpec))
(defmethod to-spec :Atom [_]
  (->AtomSpec))
(defmethod to-spec :Atomic [_]
  (->AtomicSpec))
(defmethod to-spec :Ground [_]
  (->GroundSpec))
(defmethod to-spec :Nonvar [_]
  (->NonvarSpec))
(defmethod to-spec :Var [_]
  (->VarSpec))
(defmethod to-spec :Any [_]
  (->AnySpec))
(defmethod to-spec :Exact [{[x] :content}]
  (->ExactSpec x))
(defmethod to-spec :List [{[x] :content}]
(->ListSpec (to-spec x)))
(defmethod to-spec :Tuple [{content :content}]
  (->TupleSpec (vec (map to-spec content))))
(defmethod to-spec :Functor [{[content] :content}]
  content)
(defmethod to-spec :Compound [{[functor & args] :content}]
  (->CompoundSpec (to-spec functor) (vec (map to-spec args))))
(defmethod to-spec :Or [{content :content}]
  (->OneOfSpec (set (map to-spec content))))
(defmethod to-spec :And [{content :content}]
  (->OneOfSpec (set (map to-spec content))))
(defmethod to-spec :Userdef [{[functor & args] :content}]
  (if (empty? args)
    (->UserDefinedSpec (to-spec functor))
    (make-spec:user-defined (to-spec functor) (vec (map to-spec args)))))
(defmethod to-spec :EmptyList [_]
  (->EmptyListSpec))
(defmethod to-spec :default [spec]
  spec)


(defn back-to-spec [str]
  (first (map to-spec (insta/parse p str))))
