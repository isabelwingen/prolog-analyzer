(ns prolog-analyzer.records
  (:require [prolog-analyzer.utils :refer [case+]]
            [clojure.tools.logging :as log]
            [clojure.tools.namespace.repl :refer [refresh]]
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
(declare suitable-spec)


(defprotocol printable
  (to-string [x]))

(defprotocol spec
  (spec-type [spec])
  (suitable-spec [spec term]))

(defprotocol term
  (term-type [term])
  (initial-spec [term]))

(defrecord VarSpec []
  spec
  (spec-type [spec] VAR)
  (suitable-spec [spec term]
    (case+ (term-type term)
           (VAR, ANY) spec
           nil))
  printable
  (to-string [x] "Var"))

(defrecord AtomSpec []
  spec
  (spec-type [spec] ATOM)
  (suitable-spec [spec term]
    (case+ (term-type term)
           (ATOM, VAR) spec
           ATOMIC (if (and (not= "[]" (:term term)) ((complement number?) (read-string (:term term)))) spec nil)
           nil))
  printable
  (to-string [x] "Atom"))

(defrecord IntegerSpec []
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

(defrecord FloatSpec []
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

(defrecord NumberSpec []
  spec
  (spec-type [spec] NUMBER)
  (suitable-spec [spec term]
    (case+ (term-type term)
           (NUMBER, VAR) spec
           INTEGER (->IntegerSpec)
           FLOAT (->FloatSpec)
           ATOMIC (if (number? (read-string (:term term))) spec nil)
           nil))
  printable
  (to-string [x] "Number"))

(defrecord AtomicSpec []
  spec
  (spec-type [spec] ATOMIC)
  (suitable-spec [spec term]
    (case+ (term-type term)
           (ATOMIC, VAR) spec
           ATOM (->AtomSpec)
           NUMBER (->NumberSpec)
           INTEGER (->IntegerSpec)
           FLOAT (->FloatSpec)
           nil))
  printable
  (to-string [x] "Atomic"))

(defrecord ListSpec [type]
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

(defrecord TupleSpec [arglist]
  spec
  (spec-type [spec] TUPLE)
  (suitable-spec [spec term]
    (case+ (term-type term)
           VAR spec
           ATOMIC (if (and (empty-list? term) (empty? arglist)) spec nil)
           LIST (if (and (suitable-spec (first arglist) (:head term))
                         (suitable-spec (->TupleSpec (rest arglist)) (:tail term)))
                  spec
                  nil)
           nil))
  printable
  (to-string [x] (str "Tuple(" (to-arglist arglist) ")")))

(defrecord ExactSpec [value]
  spec
  (spec-type [spec] EXACT)
  (suitable-spec [spec term]
    (case+ (term-type term)
           VAR spec
           (ATOMIC, ATOM) (if (= value (:term term)) spec nil)
           nil))
  printable
  (to-string [x] (str "Exact(" value ")")))


(defrecord CompoundSpec [functor arglist]
  spec
  (spec-type [spec] COMPOUND)
  (suitable-spec [spec term]
    (case+ (term-type term)
           VAR spec
           COMPOUND (if (and (= functor (:functor term)) (= (count arglist) (count (:arglist term)))) spec nil)
           nil))
  printable
  (to-string [x] (str functor "(" (to-arglist arglist) ")")))

(defrecord GroundSpec []
  spec
  (spec-type [spec] GROUND)
  (suitable-spec [spec term]
    (case+ (term-type term)
           (GROUND, NONVAR, ANY, VAR) spec
           ATOMIC (->AtomicSpec)
           ATOM (->AtomSpec)
           NUMBER (->NumberSpec)
           INTEGER (->IntegerSpec)
           FLOAT (->FloatSpec)
           LIST (->ListSpec (->GroundSpec))
           COMPOUND (->CompoundSpec (:functor term) (repeat (count (:arglist term)) (->GroundSpec)))
           nil))
  printable
  (to-string [x] "Ground"))

(defrecord AndSpec [arglist]
  spec
  (spec-type [spec] AND)
  (suitable-spec [spec term]
    (if (every? (complement nil?) (map #(suitable-spec % term) arglist))
      spec
      nil))
  printable
  (to-string [x] (str "And(" (to-arglist arglist) ")")))

(defrecord OneOfSpec [arglist]
  spec
  (spec-type [spec] OR)
  (suitable-spec [spec term] spec)
  printable
  (to-string [x] (str "OneOf(" (to-arglist arglist) ")")))

(defrecord UserDefinedSpec [name]
  spec
  (spec-type [spec] USERDEFINED)
  (suitable-spec [spec term] spec)
  printable
  (to-string [x] (if (contains? x :arglist)
                   (str name "(" (to-arglist (:arglist x)) ")")
                   (str name))))

(defrecord ErrorSpec [reason]
  spec
  (spec-type [spec] ERROR)
  (suitable-spec [spec term] spec)
  printable
  (to-string [x] (str "ERROR: " reason)))

(declare ->NonvarSpec)

(defrecord AnySpec []
  spec
  (spec-type [spec] ANY)
  (suitable-spec [spec term]
    (case+ (term-type term)
           INTEGER (->IntegerSpec)
           FLOAT (->FloatSpec)
           NUMBER (->NumberSpec)
           ATOM (->AtomSpec)
           ATOMIC (->AtomicSpec)
           ANY (->AnySpec)
           GROUND (->GroundSpec)
           NONVAR (->NonvarSpec)
           VAR (->VarSpec)
           LIST (->ListSpec (->AnySpec))
           COMPOUND (->CompoundSpec (:functor term) (repeat (count (:arglist term)) (->AnySpec)))
           nil))
  printable
  (to-string [x] "Any"))

(defrecord NonvarSpec []
  spec
  (spec-type [spec] NONVAR)
  (suitable-spec [spec term]
    (case+ (term-type term)
           INTEGER (->IntegerSpec)
           FLOAT (->FloatSpec)
           NUMBER (->NumberSpec)
           ATOM (->AtomSpec)
           ATOMIC (->AtomicSpec)
           ANY (->NonvarSpec)
           GROUND (->GroundSpec)
           NONVAR (->NonvarSpec)
           VAR (->NonvarSpec)
           LIST (->ListSpec (->AnySpec))
           COMPOUND (->CompoundSpec (:functor term) (repeat (count (:arglist term)) (->AnySpec)))
           nil))
  printable
  (to-string [x] "Nonvar"))

(defrecord SpecvarSpec [name]
  spec
  (spec-type [spec] SPECVAR)
  (suitable-spec [spec term]
    (let [p (case+ (term-type term)
                   (GROUND, NONVAR, ATOM, ATOMIC, INTEGER, FLOAT, NUMBER) (initial-spec term)
                   VAR (->VarSpec)
                   ANY (->AnySpec)
                   LIST (->ListSpec (->AnySpec))
                   COMPOUND (->CompoundSpec (:functor term) (repeat (count (:arglist term)) (->AnySpec)))
                   nil
                   )]
      (->AndSpec [spec p])))
  printable
  (to-string [x] (str "Specvar(" name ")")))


(defrecord AnyTerm [type term]
  term
  (term-type [term] ANY)
  (initial-spec [term] (->AnySpec))
  printable
  (to-string [x] (str term)))

(defrecord GroundTerm [type term]
  term
  (term-type [term] GROUND)
  (initial-spec [term] (->GroundSpec))
  printable
  (to-string [x] (str term)))

(defrecord NonvarTerm [type term]
  term
  (term-type [term] NONVAR)
  (initial-spec [term] (->NonvarSpec))
  printable
  (to-string [x] (str term)))

(defrecord VarTerm [type name]
  term
  (term-type [term] VAR)
  (initial-spec [term] (->VarSpec))
  printable
  (to-string [x] (str name)))

(defrecord AnonVarTerm [type name]
  term
  (term-type [term] VAR)
  (initial-spec [term] (->VarSpec))
  printable
  (to-string [x] (str name)))

(defrecord AtomTerm [type term]
  term
  (term-type [term] ATOM)
  (initial-spec [term] (->AtomSpec))
  printable
  (to-string [x] (str term)))

(defrecord AtomicTerm [type term]
  term
  (term-type [term] ATOMIC)
  (initial-spec [term] (->AtomicSpec))
  printable
  (to-string [x] (str term)))

(defrecord IntegerTerm [type value]
  term
  (term-type [term] INTEGER)
  (initial-spec [term] (->IntegerSpec))
  printable
  (to-string [x] (str value)))

(defrecord FloatTerm [type value]
  term
  (term-type [term] FLOAT)
  (initial-spec [term] (->FloatSpec))
  printable
  (to-string [x] (str value)))

(defrecord NumberTerm [type value]
  term
  (term-type [term] NUMBER)
  (initial-spec [term] (->NumberSpec))
  printable
  (to-string [x] (str value)))

(defrecord ListTerm [type head tail]
  term
  (term-type [term] LIST)
  (initial-spec [term] (->ListSpec (->AnySpec)))
  printable
  (to-string [x]
    (case+ (term-type tail)
           ATOMIC (str "[" (to-string head) "]")
           VAR (str "[" (to-string head) "|" (to-string tail) "]")
           LIST (str "[" (to-arglist (get-elements-of-list x)) "]"))))

(defrecord CompoundTerm [type functor arglist]
  term
  (term-type [term] COMPOUND)
  (initial-spec [term] (->CompoundSpec functor (repeat (count arglist) (->AnySpec))))
  printable
  (to-string [x] (str functor "(" (to-arglist arglist) ")")))

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
  (VarSpec.))

(defn make-spec:atom []
  (AtomSpec.))

(defn make-spec:atomic []
  (AtomicSpec.))

(defn make-spec:integer []
  (IntegerSpec.))

(defn make-spec:float []
  (FloatSpec.))

(defn make-spec:number []
  (NumberSpec.))

(defn make-spec:ground []
  (GroundSpec.))

(defn make-spec:nonvar []
  (NonvarSpec.))

(defn make-spec:any []
  (AnySpec.))

(defn make-spec:list [type]
  (ListSpec. type))

(defn make-spec:tuple [arglist]
  (TupleSpec. arglist))

(defn make-spec:exact [value]
  (ExactSpec. value))

(defn make-spec:specvar [name]
  (SpecvarSpec. name))

(defn make-spec:compound [functor arglist]
  (CompoundSpec. functor arglist))

(defn make-spec:one-of [arglist]
  (OneOfSpec. arglist))

(defn make-spec:and [arglist]
  (AndSpec. arglist))

(defn make-spec:user-defined
  ([name] (UserDefinedSpec. name))
  ([name arglist] (-> (UserDefinedSpec. name)
                      (assoc :arglist arglist))))

(defn make-spec:error [reason]
  (ErrorSpec. reason))


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

(defn replace-specvar-name-with-value [spec specvar-name replace-value]
  (case (spec-type spec)
    SPECVAR
    (if (= specvar-name (:name spec)) (assoc spec :name replace-value) spec)

    (USERDEFINED, OR, AND, COMPOUND, TUPLE)
    (update spec :arglist (fn [s] (seq (map #(replace-specvar-name-with-value % specvar-name replace-value) s))))

    LIST
    (update spec :type #(replace-specvar-name-with-value % specvar-name replace-value))

    spec
    ))

(defn replace-specvars-with-spec [spec specvar-name replace-spec]
  (case (spec-type spec)
    SPECVAR
    (if (= specvar-name (:name spec)) replace-spec spec)

    (USERDEFINED, OR, AND, COMPOUND, TUPLE)
    (update spec :arglist (fn [s] (seq (map #(replace-specvars-with-spec % specvar-name replace-spec) s))))

    LIST
    (update spec :type #(replace-specvars-with-spec % specvar-name replace-spec))

    spec
    ))

(defn find-specvars [spec]
  (case (spec-type spec)
    SPECVAR [spec]
    (USERDEFINED, OR, AND, COMPOUND, TUPLE) (distinct (reduce concat (map find-specvars (:arglist spec))))
    LIST (find-specvars (spec-type spec))
    []))

