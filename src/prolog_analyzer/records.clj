(ns prolog-analyzer.records
  (:require [clojure.tools.logging :as log]
            [clojure.string]))

(declare to-string)
(declare get-elements-of-list)

(defn to-arglist [list]
  (clojure.string/join ", " (map to-string list)))

(defprotocol printable
  (to-string [x]))

(defprotocol spec
  (spec-type [spec])
  (suitable-spec [spec term]))

(defprotocol term
  (term-type [term]))


(defrecord AnyTerm [type term]
  printable
  (to-string [x] (str term)))

(defrecord GroundTerm [type term]
  printable
  (to-string [x] (str term)))

(defrecord NonvarTerm [type term]
  printable
  (to-string [x] (str term)))

(defrecord VarTerm [type name]
  printable
  (to-string [x] (str name)))

(defrecord AnonVarTerm [type name]
  printable
  (to-string [x] (str name)))

(defrecord AtomTerm [type term]
  printable
  (to-string [x] (str term)))

(defrecord AtomicTerm [type term]
  printable
  (to-string [x] (str term)))

(defrecord IntegerTerm [type value]
  printable
  (to-string [x] (str value)))

(defrecord FloatTerm [type value]
  printable
  (to-string [x] (str value)))

(defrecord NumberTerm [type value]
  printable
  (to-string [x] (str value)))

(defrecord ListTerm [type head tail]
  printable
  (to-string [x] (cond
                   (= "[]" (:term tail)) (str "[" (to-string head) "]")
                   (instance? VarTerm tail) (str "[" (to-string head) "|" (to-string tail) "]")
                   (instance? AnonVarTerm tail) (str "[" (to-string head) "|" (to-string tail) "]")
                   (instance? ListTerm tail) (str "[" (to-arglist (get-elements-of-list x)) "]"))))

(defrecord CompoundTerm [type functor arglist]
  printable
  (to-string [x] (str functor "(" (to-arglist arglist) ")")))


(defrecord AnySpec [spec]
  printable
  (to-string [x] "Any"))

(defrecord VarSpec [spec]
  printable
  (to-string [x] "Var"))

(defrecord AtomSpec [spec]
  printable
  (to-string [x] "Atom"))

(defrecord AtomicSpec [spec]
  printable
  (to-string [x] "Atomic"))

(defrecord GroundSpec [spec]
  printable
  (to-string [x] "Ground"))

(defrecord NonvarSpec [spec]
  printable
  (to-string [x] "Nonvar"))

(defrecord NumberSpec [spec]
  printable
  (to-string [x] "Number"))

(defrecord IntegerSpec [spec]
  printable
  (to-string [x] "Integer"))

(defrecord FloatSpec [spec]
  printable
  (to-string [x] "Float"))

(defrecord ListSpec [spec type]
  printable
  (to-string [x] (str "List(" (to-string type) ")")))

(defrecord TupleSpec [spec arglist]
  printable
  (to-string [x] (str "Tuple(" (to-arglist arglist) ")")))

(defrecord ExactSpec [spec value]
  printable
  (to-string [x] (str "Exact(" value ")")))

(defrecord SpecvarSpec [spec name]
  printable
  (to-string [x] (str "Specvar(" name ")")))

(defrecord CompoundSpec [spec functor arglist]
  printable
  (to-string [x] (str functor "(" (to-arglist arglist) ")")))

(defrecord AndSpec [spec arglist]
  printable
  (to-string [x] (str "And(" (to-arglist arglist) ")")))

(defrecord OneOfSpec [spec arglist]
  printable
  (to-string [x] (str "OneOf(" (to-arglist arglist) ")")))

(defrecord UserDefinedSpec [spec name]
  printable
  (to-string [x] (if (contains? x :arglist)
                   (str name "(" (to-arglist (:arglist x)) ")")
                   (str name))))

(defrecord ErrorSpec [spec reason]
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
