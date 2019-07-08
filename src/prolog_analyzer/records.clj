(ns prolog-analyzer.records
  (:require [prolog-analyzer.utils :refer [case+ get-elements-of-list recursive-check-condition] :as utils]
            [clojure.tools.logging :as log]
            [clojure.tools.namespace.repl :refer [refresh]]

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
(def UNION :union)
(def COMPATIBLE :compatible)
(def ERROR :error)

(def AND :and)
(def OR :one-of)

(def ANY :any)
(def PLACEHOLDER :placeholder)

(declare to-arglist)
(declare empty-list?)
(declare next-steps)
(declare spec-type)
(declare term-type)
(declare supertype?)
(declare ->AndSpec)
(declare has-specvars)
(declare var-spec?)

(defprotocol printable
  (to-string [x]))

(defprotocol spec
  (spec-type [spec])
  (next-steps
    [spec term defs]
    [spec term defs overwrite?]))

(defprotocol term
  (term-type [term])
  (initial-spec [term]))

(defrecord AnySpec []
  spec
  (spec-type [spec] ANY)
  printable
  (to-string [x] "Any"))


(defrecord ErrorSpec [reason]
  spec
  (spec-type [spec] ERROR)
  printable
  (to-string [x] (str "ERROR: " reason)))

(defn DISJOINT
  ([] (->ErrorSpec (str "No valid intersection")))
  ([a] (->ErrorSpec (str "No valid intersection of " (to-string a))))
  ([a b]
   (->ErrorSpec (str "No valid intersection of " (to-string a) " and " (to-string b)))))


(defn error-spec? [spec]
  (or (nil? spec)
      (= ERROR (spec-type spec))
      (if (contains? spec :type) (error-spec? (.type spec)) false)
      (if (contains? spec :arglist) (some error-spec? (:arglist spec)) false)
      ))

(defrecord VarSpec []
  spec
  (spec-type [spec] VAR)
  printable
  (to-string [x] "Var"))

(defrecord EmptyListSpec []
  spec
  (spec-type [spec] EMPTYLIST)
  printable
  (to-string [x] "EmptyList"))

(defrecord StringSpec []
  spec
  (spec-type [spec] STRING)
  printable
  (to-string [x] "String"))

(defrecord AtomSpec []
  spec
  (spec-type [spec] ATOM)
  printable
  (to-string [x] "Atom"))

(defrecord IntegerSpec []
  spec
  (spec-type [spec] INTEGER)
  printable
  (to-string [x] "Integer"))

(defrecord FloatSpec []
  spec
  (spec-type [spec] FLOAT)
  printable
  (to-string [x] "Float"))

(defrecord NumberSpec []
  spec
  (spec-type [spec] NUMBER)
  printable
  (to-string [x] "Number"))

(defrecord AtomicSpec []
  spec
  (spec-type [spec] ATOMIC)
  printable
  (to-string [x] "Atomic"))

(defrecord ExactSpec [value]
  spec
  (spec-type [spec] EXACT)
  printable
  (to-string [x] (str "Exact(" value ")")))

(defn list-term? [term]
  (and (= COMPOUND (term-type term))
       (= "." (.functor term))
       (= 2 (count (.arglist term)))))

(defn get-head-and-tail [term]
  {:head (first (.arglist term)) :tail (second (.arglist term))})

(defrecord ListSpec [type]
  spec
  (spec-type [spec] LIST)
  printable
  (to-string [x] (str "List(" (to-string type) ")")))

(defrecord TupleSpec [arglist]
  spec
  (spec-type [spec] TUPLE)
  printable
  (to-string [x] (str "Tuple(" (to-arglist arglist) ")")))


(defrecord CompoundSpec [functor arglist]
  spec
  (spec-type [spec] COMPOUND)
  (next-steps [spec term defs] (next-steps spec term defs false))
  printable
  (to-string [x] (if (nil? functor) "Compound" (str "Compound(" functor "(" (to-arglist arglist) "))"))))


(defrecord GroundSpec []
  spec
  (spec-type [spec] GROUND)
  printable
  (to-string [x] "Ground"))

(defrecord AndSpec [arglist]
  spec
  (spec-type [spec] AND)
  printable
  (to-string [x] (str "And(" (to-arglist arglist) ")")))

(defrecord OneOfSpec [arglist]
  spec
  (spec-type [spec] OR)
  printable
  (to-string [x] (str "OneOf(" (to-arglist arglist) ")")))

(defrecord UserDefinedSpec [name]
  spec
  (spec-type [spec] USERDEFINED)
  printable
  (to-string [x] (if (contains? x :arglist)
                   (str name "(" (to-arglist (:arglist x)) ")")
                   (str name))))

(defrecord NonvarSpec []
  spec
  (spec-type [spec] NONVAR)
  printable
  (to-string [x] "Nonvar"))

(defrecord UnionSpec [name]
  spec
  (spec-type [spec] UNION)
  printable
  (to-string [x] (str "Union(" (if (.startsWith (str name) "G__") (apply str (drop 3 (str name))) (str name)) ")")))

(defrecord CompatibleSpec [name]
  spec
  (spec-type [spec] COMPATIBLE)
  printable
  (to-string [x] (str "Compatible(" (if (.startsWith (str name) "G__") (apply str (drop 3 (str name))) (str name)) ")")))

(defrecord PlaceholderSpec [inner-spec]
  spec
  (spec-type [spec] PLACEHOLDER)
  printable
  (to-string [x] (str "Placeholder(" (to-string inner-spec) ")")))


(defrecord SpecvarSpec [name]
  spec
  (spec-type [spec] SPECVAR)
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

(defrecord ListTerm [head tail]
  term
  (term-type [term] LIST)
  (initial-spec [term] (if (empty-list? tail)
                         (->ListSpec (initial-spec head))
                         (if (contains? tail :head)
                           (->ListSpec (->OneOfSpec [(initial-spec head) (:type (initial-spec tail))]))
                           (->ListSpec (->AnySpec)))))
  printable
  (to-string [x]
    (case+ (term-type tail)
           ATOMIC (str "[" (to-string head) "]")
           EMPTYLIST (str "[" (to-string head) "]")
           VAR (str "[" (to-string head) "|" (to-string tail) "]")
           LIST (str "[" (to-arglist (get-elements-of-list x)) "]")
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
         :var (let [var (map->VarTerm m)]
                (if (= singletons :nothing)
                  var
                  (assoc var :singleton? (singleton? singletons var))))
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


(defn empty-list?
  "Checks if the input is the empty (prolog) list."
  [term]
  (or (and (= ATOMIC (term-type term)) (= "[]" (:term term)))
      (= EMPTYLIST (term-type term))))

(defn to-head-tail-list
  "Transforms a bunch of `terms` to a proper prolog list."
  [& terms]
  (if (empty? terms)
    (->EmptyListTerm)
    (->ListTerm (first terms) (apply to-head-tail-list (rest terms)))))

(defn to-tuple-spec
  "Transforms a bunch of `specs` to a tuple spec."
  [& specs]
  (if (empty? specs)
    (->TupleSpec [])
    (->TupleSpec specs)))

(defn to-arglist [list]
  (clojure.string/join ", " (map to-string list)))

(defn replace-specvar-name-with-value [spec specvar-name replace-value]
  (case+ (spec-type spec)
         (SPECVAR,UNION,COMPATIBLE)
         (if (= specvar-name (:name spec)) (assoc spec :name replace-value) spec)

         (OR,AND) (-> spec
                      (update :arglist (partial map #(replace-specvar-name-with-value % specvar-name replace-value)))
                      (update :arglist set))

         (USERDEFINED, COMPOUND, TUPLE)
         (-> spec
             (update :arglist (partial map #(replace-specvar-name-with-value % specvar-name replace-value)))
             (update :arglist (partial apply vector)))
         LIST
         (update spec :type #(replace-specvar-name-with-value % specvar-name replace-value))

         spec
         ))


(defn replace-specvars-with-spec [type specvar-name replace-spec]
  (case+ (spec-type type)
         (SPECVAR,UNION,COMPATIBLE) (if (= specvar-name (:name type)) replace-spec type)

         (OR,AND) (-> type
                      (update :arglist (partial map #(replace-specvars-with-spec % specvar-name replace-spec)))
                      (update :arglist set))

         (USERDEFINED, COMPOUND, TUPLE) (-> type
                                            (update :arglist (partial map #(replace-specvars-with-spec % specvar-name replace-spec)))
                                            (update :arglist (partial apply vector)))
         LIST (update type :type #(replace-specvars-with-spec % specvar-name replace-spec))
         type
         ))

(defn replace-union-and-comp-with-placeholder [type]
  (case+ (spec-type type)
         (UNION,COMPATIBLE) (->PlaceholderSpec type)

         (OR,AND) (-> type
                      (update :arglist (partial map replace-union-and-comp-with-placeholder))
                      (update :arglist set))

         (USERDEFINED, COMPOUND, TUPLE) (-> type
                                            (update :arglist (partial map replace-union-and-comp-with-placeholder))
                                            (update :arglist (partial apply vector)))

         LIST (update type :type replace-union-and-comp-with-placeholder)

         type))


(defn find-specvars [spec]
  (case+ (spec-type spec)
         (SPECVAR,UNION,COMPATIBLE) [spec]
         (OR, AND, COMPOUND, TUPLE) (set (mapcat find-specvars (.arglist spec)))
         USERDEFINED (if (contains? spec :arglist) (set (mapcat find-specvars (:arglist spec))) [])
         LIST (find-specvars (.type spec))
         #{}))



(defn has-specvars [spec]
  (or
   (= SPECVAR (spec-type spec))
   (= UNION (spec-type spec))
   (= COMPATIBLE (spec-type spec))
   (some has-specvars (:arglist spec))
   (some->> (:type spec)
            has-specvars)))


(defn replace-specvars-with-any [spec]
  (let [used-specvars (find-specvars spec)
        replace-map (reduce #(assoc %1 (:name %2) (->AnySpec)) {} used-specvars)]
    (reduce-kv replace-specvars-with-spec spec replace-map)))


(defn length-of-list-term [{head :head tail :tail :as list}]
  (cond
    (nil? head) 0
    (nil? tail) 1
    (= VAR (term-type tail)) :inf
    :default
    (let [tail-length (length-of-list-term tail)]
      (if (= :inf tail-length)
        :inf
        (inc tail-length)))))


(defn var-spec? [spec]
  (if (nil? spec)
    false
    (= VAR (spec-type spec))))

(defn any-spec? [spec]
  (if (nil? spec)
    true
    (= ANY (spec-type spec))))


(defn record-type [type]
  (cond
    (nil? type) nil
    (satisfies? spec type) (spec-type spec)
    :else (term-type spec)))

(defn maybe-spec [spec]
  (cond
    (:arglist spec) (->OneOfSpec #{(->VarSpec) (update spec :arglist (partial map (fn [x] (->AnySpec))))})
    (:type spec) (->OneOfSpec #{(->VarSpec) (assoc spec :type (->AnySpec))})
    :else (if (any-spec? spec) spec (->OneOfSpec #{(->VarSpec) spec}))))
