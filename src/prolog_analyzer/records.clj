(ns prolog-analyzer.records
  (:require [prolog-analyzer.utils :refer [case+]]
            [clojure.tools.logging :as log]
            [clojure.string]))

(def INTEGER :integer)
(def FLOAT :float)
(def NUMBER :number)
(def ATOM :atom)
(def ATOMIC :atomic)
(def GROUND :ground)
(def ANY :any)
(def NONVAR :nonvar)
(def VAR :var)
(def LIST :list)
(def COMPOUND :compound)
(def TUPLE :tuple)
(def AND :and)
(def OR :one-of)
(def USERDEFINED :user-defined)
(def SPECVAR :specvar)
(def EXACT :exact)
(def ERROR :error)

(declare get-elements-of-list)
(declare to-arglist)
(declare empty-list?)


(defprotocol printable
  (to-string [x]))

(defprotocol spec
  (spec-type [spec])
  (suitable-spec [spec term]))

(defprotocol term
  (term-type [term]))


(defrecord AnyTerm [type term]
  term
  (term-type [term] ANY)
  printable
  (to-string [x] (str term)))

(defrecord GroundTerm [type term]
  term
  (term-type [term] GROUND)
  printable
  (to-string [x] (str term)))

(defrecord NonvarTerm [type term]
  term
  (term-type [term] NONVAR)
  printable
  (to-string [x] (str term)))

(defrecord VarTerm [type name]
  term
  (term-type [term] VAR)
  printable
  (to-string [x] (str name)))

(defrecord AnonVarTerm [type name]
  term
  (term-type [term] VAR)
  printable
  (to-string [x] (str name)))

(defrecord AtomTerm [type term]
  term
  (term-type [term] ATOM)
  printable
  (to-string [x] (str term)))

(defrecord AtomicTerm [type term]
  term
  (term-type [term] ATOMIC)
  printable
  (to-string [x] (str term)))

(defrecord IntegerTerm [type value]
  term
  (term-type [term] INTEGER)
  printable
  (to-string [x] (str value)))

(defrecord FloatTerm [type value]
  term
  (term-type [term] FLOAT)
  printable
  (to-string [x] (str value)))

(defrecord NumberTerm [type value]
  term
  (term-type [term] NUMBER)
  printable
  (to-string [x] (str value)))

(defrecord ListTerm [type head tail]
  term
  (term-type [term] LIST)
  printable
  (to-string [x] (cond
                   (= "[]" (:term tail)) (str "[" (to-string head) "]")
                   (instance? VarTerm tail) (str "[" (to-string head) "|" (to-string tail) "]")
                   (instance? AnonVarTerm tail) (str "[" (to-string head) "|" (to-string tail) "]")
                   (instance? ListTerm tail) (str "[" (to-arglist (get-elements-of-list x)) "]"))))

(defrecord CompoundTerm [type functor arglist]
  term
  (term-type [term] COMPOUND)
  printable
  (to-string [x] (str functor "(" (to-arglist arglist) ")")))


(defrecord AnySpec [spec]
  spec
  (spec-type [spec] ANY)
  (suitable-spec [spec term] spec)
  printable
  (to-string [x] "Any"))

(defrecord VarSpec [spec]
  spec
  (spec-type [spec] VAR)
  (suitable-spec [spec term] spec)
  printable
  (to-string [x] "Var"))

(defrecord AtomSpec [spec]
  spec
  (spec-type [spec] ATOM)
  (suitable-spec [spec term]
    (case+ (term-type term)
           (ATOM, VAR) spec
           ATOMIC (if (and (not= "[]" (:term term)) ((complement number?) (read-string (:term term)))) spec nil)
           nil))
  printable
  (to-string [x] "Atom"))

(defrecord IntegerSpec [spec]
  spec
  (spec-type [spec] INTEGER)
  (suitable-spec [spec term]
    (case+ (term-type term)
           (INTEGER, VAR) spec
           NUMBER (if (int? (:value term)) spec nil)
           ATOMIC (if (int? (read-string (:term term))) spec nil)
           nil))
  printable
  (to-string [x] "Integer"))

(defrecord FloatSpec [spec]
  spec
  (spec-type [spec] FLOAT)
  (suitable-spec [spec term]
    (case+ (term-type term)
           (FLOAT, VAR) spec
           NUMBER (if (float? (:value term)) spec nil)
           ATOMIC (if (float? (read-string (:term term))) spec nil)
           nil))
  printable
  (to-string [x] "Float"))

(defrecord NumberSpec [spec]
  spec
  (spec-type [spec] NUMBER)
  (suitable-spec [spec term]
    (case+ (term-type term)
           (NUMBER, VAR) spec
           INTEGER (IntegerSpec. :integer)
           FLOAT (FloatSpec. :float)
           ATOMIC (if (number? (read-string (:term term))) spec nil)
           nil))
  printable
  (to-string [x] "Number"))



(defrecord AtomicSpec [spec]
  spec
  (spec-type [spec] ATOMIC)
  (suitable-spec [spec term]
    (println (term-type term))
    (case+ (term-type term)
           (ATOMIC, VAR) spec
           ATOM (AtomSpec. :atom)
           NUMBER (NumberSpec. :number)
           INTEGER (IntegerSpec. :integer)
           FLOAT (FloatSpec. :float)
           nil))
  printable
  (to-string [x] "Atomic"))

(defrecord GroundSpec [spec]
  spec
  (spec-type [spec] GROUND)
  (suitable-spec [spec term] spec)
  printable
  (to-string [x] "Ground"))

(defrecord NonvarSpec [spec]
  spec
  (spec-type [spec] NONVAR)
  (suitable-spec [spec term] spec)
  printable
  (to-string [x] "Nonvar"))

(defrecord ListSpec [spec type]
  spec
  (spec-type [spec] LIST)
  (suitable-spec [spec term]
    (case+ (term-type term)
           VAR spec
           LIST (if (suitable-spec type (:head term)) spec nil)
           ATOMIC (if (= "[]" (:term term)) spec nil)
           nil))
  printable
  (to-string [x] (str "List(" (to-string type) ")")))


(defrecord TupleSpec [spec arglist]
  spec
  (spec-type [spec] TUPLE)
  (suitable-spec [spec term]
    (case+ (term-type term)
           VAR spec
           ATOMIC (if (and (empty-list? term) (empty? arglist)) spec nil)
           LIST (if (and (suitable-spec (first arglist) (:head term))
                         (suitable-spec (TupleSpec. :tuple (rest arglist)) (:tail term)))
                  spec
                  nil)
           nil))
  printable
  (to-string [x] (str "Tuple(" (to-arglist arglist) ")")))

(defrecord ExactSpec [spec value]
  spec
  (spec-type [spec] EXACT)
  (suitable-spec [spec term]
    (case+ (term-type term)
           VAR spec
           (ATOMIC, ATOM) (do (println "yo") (if (= value (:term term)) spec nil))
           nil))
  printable
  (to-string [x] (str "Exact(" value ")")))


(defrecord SpecvarSpec [spec name]
  spec
  (spec-type [spec] SPECVAR)
  (suitable-spec [spec term] spec)
  printable
  (to-string [x] (str "Specvar(" name ")")))

(defrecord CompoundSpec [spec functor arglist]
  spec
  (spec-type [spec] COMPOUND)
  (suitable-spec [spec term]
    (case+ (term-type term)
           VAR spec
           COMPOUND (if (and (= functor (:functor term)) (= (count arglist) (count (:arglist term)))) spec nil)
           nil))
  printable
  (to-string [x] (str functor "(" (to-arglist arglist) ")")))

(defrecord AndSpec [spec arglist]
  spec
  (spec-type [spec] AND)
  (suitable-spec [spec term] spec)
  printable
  (to-string [x] (str "And(" (to-arglist arglist) ")")))

(defrecord OneOfSpec [spec arglist]
  spec
  (spec-type [spec] OR)
  (suitable-spec [spec term] spec)
  printable
  (to-string [x] (str "OneOf(" (to-arglist arglist) ")")))

(defrecord UserDefinedSpec [spec name]
  spec
  (spec-type [spec] USERDEFINED)
  (suitable-spec [spec term] spec)
  printable
  (to-string [x] (if (contains? x :arglist)
                   (str name "(" (to-arglist (:arglist x)) ")")
                   (str name))))

(defrecord ErrorSpec [spec reason]
  spec
  (spec-type [spec] ERROR)
  (suitable-spec [spec term] spec)
  printable
  (to-string [x] (str "ERROR: " reason)))




(defn map-to-term [m]
  (case (:type m)
    :anon_var (map->AnonVarTerm m)
    :var (map->VarTerm m)
    :any (map->AnyTerm m)
    :ground (map->GroundTerm m)
    :nonvar (map->NonvarTerm m)
    :atom (map->AtomTerm m)
    :atomic (map->AtomicTerm m)
    :number (map->NumberTerm m)
    :integer (map->IntegerTerm m)
    :float (map->FloatTerm m)
    :list (map->ListTerm (-> m
                         (update :head map-to-term)
                         (update :tail map-to-term)))
    :compound (map->CompoundTerm (update m :arglist #(map map-to-term %)))
    (log/error "No case for" m "in map-to-term")))

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
    :list (map->ListSpec (update m :type map-to-spec))
    :tuple (map->TupleSpec (update m :arglist (partial map map-to-spec)))
    :compound (map->CompoundSpec (update m :arglist (partial map map-to-spec)))
    :and (map->AndSpec (update m :arglist (partial map map-to-spec)))
    :one-of (map->OneOfSpec (update m :arglist (partial map map-to-spec)))
    :user-defined (map->UserDefinedSpec (update m :arglist (partial map map-to-spec)))
    :error-spec (map->ErrorSpec m)
    (log/error "No case for" m "in map-to-term")))


(defn make-term:var [name]
  (VarTerm. :var name))

(defn make-term:anon_var [name]
  (AnonVarTerm. :anon_var name))

(defn make-term:any [name]
  (AnyTerm. :any name))

(defn make-term:ground [name]
  (GroundTerm. :ground name))

(defn make-term:nonvar [name]
  (NonvarTerm. :nonvar name))

(defn make-term:atom [term]
  (AtomTerm. :atom term))

(defn make-term:atomic [term]
  (AtomicTerm. :atomic term))

(defn make-term:number [value]
  (NumberTerm. :number value))

(defn make-term:integer [value]
  (IntegerTerm. :integer value))

(defn make-term:float [value]
  (FloatTerm. :float value))

(defn make-term:list [head tail]
  (ListTerm. :list head tail))

(defn make-term:compound [functor arglist]
  (CompoundTerm. :compound functor arglist))


(defn make-spec:var []
  (VarSpec. :var))

(defn make-spec:atom []
  (AtomSpec. :atom))

(defn make-spec:atomic []
  (AtomicSpec. :atomic))

(defn make-spec:integer []
  (IntegerSpec. :integer))

(defn make-spec:float []
  (FloatSpec. :float))

(defn make-spec:number []
  (NumberSpec. :number))

(defn make-spec:ground []
  (GroundSpec. :ground))

(defn make-spec:nonvar []
  (NonvarSpec. :nonvar))

(defn make-spec:any []
  (AnySpec. :any))

(defn make-spec:list [type]
  (ListSpec. :list type))

(defn make-spec:tuple [arglist]
  (TupleSpec. :tuple arglist))

(defn make-spec:exact [value]
  (ExactSpec. :exact value))

(defn make-spec:specvar [name]
  (SpecvarSpec. :specvar name))

(defn make-spec:compound [functor arglist]
  (CompoundSpec. :compound functor arglist))

(defn make-spec:one-of [arglist]
  (OneOfSpec. :one-of  arglist))

(defn make-spec:and [arglist]
  (AndSpec. :and arglist))

(defn make-spec:user-defined
  ([name] (UserDefinedSpec. :user-defined name))
  ([name arglist] (-> (UserDefinedSpec. :user-defined name)
                      (assoc :arglist arglist))))

(defn make-spec:error [reason]
  (ErrorSpec. :error reason))


(defn empty-list?
  "Checks if the input is the empty (prolog) list."
  [{term :term type :type}]
  (and (= type :atomic) (= term "[]")))

(defn to-head-tail-list
  "Transforms a bunch of `terms` to a proper prolog list."
  [& terms]
  (if (empty? terms)
    (make-term:atomic "[]")
    (make-term:list (first terms) (apply to-head-tail-list (rest terms)))))

(defn to-tuple-spec
  "Transforms a bunch of `specs` to a tuple spec."
  [& specs]
  (if (empty? specs)
    (make-spec:error "Cannot build a tuple with zero arguments")
    (make-spec:tuple specs)))

(defn to-or-spec
  "Transforms a bunch of `specs` to a one-of spec."
  [& specs]
  (case (count specs)
    0 (make-spec:error "Cannot build empty one-of")
    1 (first specs)
    (make-spec:one-of specs)))

(defn get-elements-of-list [{head :head tail :tail}]
  (if (= "[]" (:term tail))
    (list head)
    (conj (get-elements-of-list tail) head)))

(defn to-arglist [list]
  (clojure.string/join ", " (map to-string list)))

