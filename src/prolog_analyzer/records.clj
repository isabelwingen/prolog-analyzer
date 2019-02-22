(ns prolog-analyzer.records
  (:require [clojure.tools.logging :as log]
            [clojure.string]))

(declare to-string)

(defn to-arglist [list]
  (clojure.string/join ", " (map to-string list)))

(defprotocol printable
  (to-string [x]))

(defn get-elements-of-list [{head :head tail :tail}]
  (if (= "[]" (:term tail))
    (list head)
    (conj (get-elements-of-list tail) head)))


(defrecord AnyTerm [type term]
  printable
  (to-string [_] (str term)))

(defrecord GroundTerm [type term]
  printable
  (to-string [_] (str term)))

(defrecord NonvarTerm [type term]
  printable
  (to-string [_] (str term)))

(defrecord VarTerm [type name]
  printable
  (to-string [_] (str name)))

(defrecord AnonVarTerm [type name]
  printable
  (to-string [_] (str name)))

(defrecord AtomTerm [type term]
  printable
  (to-string [_] (str term)))

(defrecord AtomicTerm [type term]
  printable
  (to-string [_] (str term)))

(defrecord IntegerTerm [type value]
  printable
  (to-string [_] (str value)))

(defrecord FloatTerm [type value]
  printable
  (to-string [_] (str value)))

(defrecord NumberTerm [type value]
  printable
  (to-string [_] (str value)))

(defrecord ListTerm [type head tail]
  printable
  (to-string [x] (cond
                   (= "[]" (:term tail)) (str "[" (to-string head) "]")
                   (instance? VarTerm tail) (str "[" (to-string head) "|" (to-string tail) "]")
                   (instance? AnonVarTerm tail) (str "[" (to-string head) "|" (to-string tail) "]")
                   (instance? ListTerm tail) (str "[" (to-arglist (get-elements-of-list x)) "]"))))

(defrecord CompoundTerm [type functor arglist]
  printable
  (to-string [_] (str functor "(" (to-arglist arglist) ")")))


(defrecord AnySpec [spec]
  printable
  (to-string [_] "Any"))

(defrecord VarSpec [spec]
  printable
  (to-string [_] "Var"))

(defrecord AtomSpec [spec]
  printable
  (to-string [_] "Atom"))

(defrecord AtomicSpec [spec]
  printable
  (to-string [_] "Atomic"))

(defrecord GroundSpec [spec]
  printable
  (to-string [_] "Ground"))

(defrecord NonvarSpec [spec]
  printable
  (to-string [_] "Nonvar"))

(defrecord NumberSpec [spec]
  printable
  (to-string [_] "Number"))

(defrecord IntegerSpec [spec]
  printable
  (to-string [_] "Integer"))

(defrecord FloatSpec [spec]
  printable
  (to-string [_] "Float"))

(defrecord ListSpec [spec type]
  printable
  (to-string [_] (str "List(" (to-string type) ")")))

(defrecord TupleSpec [spec arglist]
  printable
  (to-string [_] (str "Tuple(" (to-arglist arglist) ")")))

(defrecord ExactSpec [spec value]
  printable
  (to-string [_] (str "Exact(" value ")")))

(defrecord SpecvarSpec [spec name]
  printable
  (to-string [_] (str "Specvar(" name ")")))

(defrecord CompoundSpec [spec functor arglist]
  printable
  (to-string [_] (str functor "(" (to-arglist arglist) ")")))

(defrecord AndSpec [spec arglist]
  printable
  (to-string [_] (str "And(" (to-arglist arglist) ")")))

(defrecord OneOfSpec [spec arglist]
  printable
  (to-string [_] (str "OneOf(" (to-arglist arglist) ")")))

(defrecord UserDefinedSpec [spec name]
  printable
  (to-string [x] (if (contains? x :arglist)
                   (str name "(" (to-arglist (:arglist x)) ")")
                   (str name))))

(defrecord ErrorSpec [spec reason]
  printable
  (to-string [_] (str "ERROR: " reason)))




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


