(ns prolog-analyzer.records
  (:require [prolog-analyzer.utils :refer [case+ get-elements-of-list] :as utils]
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
(def ERROR :error)

(def AND :and)
(def OR :one-of)

(def ANY :any)

(declare to-arglist)
(declare empty-list?)
(declare next-steps)
(declare spec-type)
(declare term-type)
(declare simplify-or)
(declare resolve-definition-with-parameters)
(declare supertype?)
(declare ->AndSpec)
(declare has-specvars)


(defprotocol printable
  (to-string [x]))

(defprotocol spec
  (spec-type [spec])
  (next-steps
    [spec term defs]
    [spec term defs overwrite?])
  (intersect
    [spec other-spec defs]
    [spec other-spec defs overwrite?]))

(defprotocol term
  (term-type [term])
  (initial-spec [term]))

(defrecord AnySpec []
  spec
  (spec-type [spec] ANY)
  (next-steps [spec term defs overwrite?]
    (if (contains? #{LIST, COMPOUND} (term-type term))
      [term (intersect spec (initial-spec term) defs)]
      []))
  (next-steps [spec term defs] (next-steps spec term defs false))
  (intersect [spec other-spec defs] other-spec)
  (intersect [spec other-spec defs overwrite?] other-spec)
  printable
  (to-string [x] "Any"))


(defrecord ErrorSpec [reason]
  spec
  (spec-type [spec] ERROR)
  (next-steps [spec term defs] [])
  (next-steps [spec term defs overwrite?] [])
  (intersect [spec other-spec defs] (intersect spec other-spec defs false))
  (intersect [spec other-spec defs _]
    (if (= ERROR (spec-type other-spec))
      other-spec
      spec))
  printable
  (to-string [x] (str "ERROR: " reason)))

(def DISJOINT (->ErrorSpec "No Valid Intersection"))

(defn error-spec? [spec]
  (or (nil? spec)
      (= ERROR (spec-type spec))
      (if (contains? spec :type) (error-spec? (.type spec)))
      (if (contains? spec :arglist) (some error-spec? (:arglist spec)))
      ))

(defn replace-error-spec-with-intersect-error [spec]
  (if (error-spec? spec)
    DISJOINT
    spec))

(defrecord VarSpec []
  spec
  (spec-type [spec] VAR)
  (next-steps [spec term defs] [])
  (next-steps [spec term defs overwrite?] [])
  (intersect [spec other-spec defs]
    (case+ (spec-type other-spec)
           (VAR, ANY) spec
           (AND, OR) (intersect other-spec spec defs)
           USERDEFINED (intersect (resolve-definition-with-parameters other-spec defs) spec defs)
           ERROR other-spec
           SPECVAR spec
           DISJOINT))
  (intersect [spec other-spec defs overwrite?]
    (if overwrite?
      (intersect (->AnySpec) other-spec defs overwrite?)
      (intersect spec other-spec defs)))
  printable
  (to-string [x] "Var"))

(defrecord EmptyListSpec []
  spec
  (spec-type [spec] EMPTYLIST)
  (next-steps [spec term defs] [])
  (next-steps [spec term defs overwrite?] [])
  (intersect [spec other-spec defs overwrite?]
    (case+ (spec-type other-spec)
           (EMPTYLIST, LIST, GROUND, NONVAR, ANY, ATOMIC) spec
           TUPLE (if (empty? (.arglist other-spec)) spec DISJOINT)
           (AND, OR) (intersect other-spec spec defs overwrite?)
           USERDEFINED (intersect (resolve-definition-with-parameters other-spec defs) spec defs overwrite?)
           ERROR other-spec
           VAR (if overwrite? spec DISJOINT)
           SPECVAR spec
           DISJOINT))
  (intersect [spec other-spec defs] (intersect spec other-spec defs false))
  printable
  (to-string [x] "EmptyList"))

(defrecord StringSpec []
  spec
  (spec-type [spec] STRING)
  (next-steps [spec term defs] [])
  (next-steps [spec term defs overwrite?] [])
  (intersect [spec other-spec defs overwrite?]
    (case+ (spec-type other-spec)
           (STRING, ATOMIC, GROUND, NONVAR, ANY) spec
           (AND, OR) (intersect other-spec spec defs overwrite?)
           USERDEFINED (intersect (resolve-definition-with-parameters other-spec defs) spec defs overwrite?)
           ERROR other-spec
           VAR (if overwrite? spec DISJOINT)
           SPECVAR spec
           DISJOINT))
  (intersect [spec other-spec defs] (intersect spec other-spec defs false))
  printable
  (to-string [x] "String"))

(defrecord AtomSpec []
  spec
  (spec-type [spec] ATOM)
  (next-steps [spec term defs] [])
  (next-steps [spec term defs overwrite?] [])
  (intersect [spec other-spec defs overwrite?]
    (case+ (spec-type other-spec)
           (ATOM, ATOMIC, GROUND, NONVAR, ANY) spec
           EXACT other-spec
           (AND, OR) (intersect other-spec spec defs overwrite?)
           USERDEFINED (intersect (resolve-definition-with-parameters other-spec defs) spec defs overwrite?)
           SPECVAR spec
           ERROR other-spec
           VAR (if overwrite? spec DISJOINT)
           DISJOINT))
  (intersect [spec other-spec defs] (intersect spec other-spec defs false))
  printable
  (to-string [x] "Atom"))

(defrecord IntegerSpec []
  spec
  (spec-type [spec] INTEGER)
  (next-steps [spec term defs] [])
  (next-steps [spec term defs overwrite?] [])
  (intersect [spec other-spec defs overwrite?]
    (case+ (spec-type other-spec)
           (ATOMIC, INTEGER, NUMBER, GROUND, NONVAR, ANY) spec
           (AND, OR) (intersect other-spec spec defs overwrite?)
           USERDEFINED (intersect (resolve-definition-with-parameters other-spec defs) spec defs overwrite?)
           SPECVAR spec
           ERROR other-spec
           VAR (if overwrite? spec DISJOINT)
           DISJOINT
           ))
  (intersect [spec other-spec defs] (intersect spec other-spec defs false))
  printable
  (to-string [x] "Integer"))

(defrecord FloatSpec []
  spec
  (spec-type [spec] FLOAT)
  (next-steps [spec term defs] [])
  (next-steps [spec term defs overwrite?] [])
  (intersect [spec other-spec defs overwrite?]
    (case+ (spec-type other-spec)
           (ATOMIC, FLOAT, NUMBER, GROUND, NONVAR, ANY) spec
           (AND, OR) (intersect other-spec spec defs overwrite?)
           USERDEFINED (intersect (resolve-definition-with-parameters other-spec defs) spec defs overwrite?)
           SPECVAR spec
           ERROR other-spec
           VAR (if overwrite? spec DISJOINT)
           DISJOINT
           ))
  (intersect [spec other-spec defs] (intersect spec other-spec defs false))
  printable
  (to-string [x] "Float"))

(defrecord NumberSpec []
  spec
  (spec-type [spec] NUMBER)
  (next-steps [spec term defs] [])
  (next-steps [spec term defs overwrite?] [])
  (intersect [spec other-spec defs overwrite?]
    (case+ (spec-type other-spec)
           (ATOMIC, NUMBER, GROUND, NONVAR, ANY) spec
           (INTEGER, FLOAT) other-spec
           (AND, OR) (intersect other-spec spec defs overwrite?)
           USERDEFINED (intersect (resolve-definition-with-parameters other-spec defs) spec defs overwrite?)
           SPECVAR spec
           ERROR other-spec
           VAR (if overwrite? spec DISJOINT)
           DISJOINT))
  (intersect [spec other-spec defs] (intersect spec other-spec defs false))
  printable
  (to-string [x] "Number"))

(defrecord AtomicSpec []
  spec
  (spec-type [spec] ATOMIC)
  (next-steps [spec term defs] [])
  (next-steps [spec term defs overwrite?] [])
  (intersect [spec other-spec defs overwrite?]
    (case+ (spec-type other-spec)
           (ATOMIC, GROUND, NONVAR, ANY) spec
           (INTEGER, FLOAT, ATOM, NUMBER, EMPTYLIST, EXACT) other-spec
           LIST (->EmptyListSpec)
           (AND, OR) (intersect other-spec spec defs overwrite?)
           USERDEFINED (intersect (resolve-definition-with-parameters other-spec defs) spec defs overwrite?)
           SPECVAR spec
           VAR (if overwrite? spec DISJOINT)
           ERROR other-spec
           DISJOINT))
  (intersect [spec other-spec defs] (intersect spec other-spec defs false))
  printable
  (to-string [x] "Atomic"))


(defrecord ExactSpec [value]
  spec
  (spec-type [spec] EXACT)
  (next-steps [spec term defs] [])
  (next-steps [spec term defs overwrite?] [])
  (intersect [spec other-spec defs overwrite?]
    (case+ (spec-type other-spec)
           (GROUND, NONVAR, ANY, ATOM, ATOMIC) spec
           EXACT (if (= value (.value other-spec)) spec DISJOINT)
           (AND, OR) (intersect other-spec spec defs overwrite?)
           USERDEFINED (intersect (resolve-definition-with-parameters other-spec defs) spec defs overwrite?)
           SPECVAR spec
           VAR (if overwrite? spec DISJOINT)
           ERROR other-spec
           DISJOINT))
  (intersect [spec other-spec defs] (intersect spec other-spec defs false))
  printable
  (to-string [x] (str "Exact(" value ")")))


(defrecord ListSpec [type]
  spec
  (spec-type [spec] LIST)
  (next-steps [spec term defs _]
    (if (= LIST (term-type term))
      (if (empty-list? (:tail term))
        [(.head term) type]
        [(.head term) type
         (.tail term) spec])
      []))
  (next-steps [spec term defs] (next-steps spec term defs false))
  (intersect [spec other-spec defs overwrite?]
    (case+ (spec-type other-spec)
           LIST (-> other-spec
                    (update :type #(intersect type % defs overwrite?))
                    replace-error-spec-with-intersect-error)
           TUPLE (-> other-spec
                     (update :arglist (partial map #(intersect type % defs overwrite?)))
                     replace-error-spec-with-intersect-error)
           EMPTYLIST other-spec
           ATOMIC (->EmptyListSpec)
           (ANY, NONVAR) spec
           GROUND (replace-error-spec-with-intersect-error (update spec :type #(intersect other-spec % defs overwrite?)))
           (AND, OR) (intersect other-spec spec defs overwrite?)
           USERDEFINED (intersect (resolve-definition-with-parameters other-spec defs) spec defs overwrite?)
           SPECVAR spec
           VAR (if overwrite? spec DISJOINT)
           ERROR other-spec
           DISJOINT))
  (intersect [spec other-spec defs] (intersect spec other-spec defs false))
  printable
  (to-string [x] (str "List(" (to-string type) ")")))

(defrecord TupleSpec [arglist]
  spec
  (spec-type [spec] TUPLE)
  (next-steps [spec term defs _]
    (if (= LIST (term-type term))
      (if (empty-list? (:tail term))
        [(.head term) (first (.arglist spec))]
        [(.head term) (first (.arglist spec))
         (.tail term) (update spec :arglist rest)])
      []))
  (next-steps [spec term defs] (next-steps spec term defs false))
  (intersect [spec other-spec defs overwrite?]
    (case+ (spec-type other-spec)
           EMPTYLIST (if (empty? arglist) other-spec DISJOINT)
           LIST (-> spec
                    (update :arglist (partial map #(intersect (.type other-spec) % defs overwrite?)))
                    replace-error-spec-with-intersect-error)
           TUPLE (if (= (count arglist) (count (.arglist other-spec)))
                   (-> other-spec
                       (update :arglist (partial map #(intersect %1 %2 defs overwrite?) arglist))
                       replace-error-spec-with-intersect-error)
                   DISJOINT)
           ATOMIC (if (empty? arglist) (->EmptyListSpec) DISJOINT)
           (ANY, NONVAR) spec
           GROUND (replace-error-spec-with-intersect-error (update spec :arglist (partial map #(intersect other-spec % defs overwrite?))))
           (AND, OR) (intersect other-spec spec defs overwrite?)
           USERDEFINED (intersect (resolve-definition-with-parameters other-spec defs) spec defs overwrite?)
           SPECVAR spec
           VAR (if overwrite? spec DISJOINT)
           ERROR other-spec
           DISJOINT))
  (intersect [spec other-spec defs] (intersect spec other-spec defs false))
  printable
  (to-string [x] (str "Tuple(" (to-arglist arglist) ")")))


(defrecord CompoundSpec [functor arglist]
  spec
  (spec-type [spec] COMPOUND)
  (next-steps [spec term defs _]
    (if (= COMPOUND (term-type term))
      (interleave (.arglist term) arglist)
      []))
  (next-steps [spec term defs] (next-steps spec term defs false))
  (intersect [spec other-spec defs overwrite?]
    (case+ (spec-type other-spec)
           COMPOUND (cond
                      (nil? functor) other-spec
                      (nil? (.functor other-spec)) spec
                      :else (if (and (= functor (.functor other-spec))
                                     (= (count arglist) (count (.arglist other-spec))))
                              (-> other-spec
                                  (update :arglist (partial map #(intersect %1 %2 defs overwrite?) arglist))
                                  replace-error-spec-with-intersect-error)
                              DISJOINT))
           (ANY, NONVAR) spec
           GROUND (replace-error-spec-with-intersect-error (update spec :arglist (partial map #(intersect other-spec % defs overwrite?))))
           (AND, OR) (intersect other-spec spec defs overwrite?)
           USERDEFINED (intersect (resolve-definition-with-parameters other-spec defs) spec defs overwrite?)
           SPECVAR spec
           VAR (if overwrite? spec DISJOINT)
           ERROR other-spec
           DISJOINT))
  (intersect [spec other-spec defs] (intersect spec other-spec defs false))
  printable
  (to-string [x] (if (nil? functor) "Compound" (str "Compound(" functor "(" (to-arglist arglist) "))"))))

(declare ->GroundSpec)


(defn contains-vars?
  [spec defs already-resolved]
  (let [new-resolved (conj already-resolved spec)]
    (if (= USERDEFINED (spec-type spec))
      (if (contains? already-resolved spec)
        false
        (->> (resolve-definition-with-parameters spec defs)
             :arglist
             (remove #(contains? already-resolved %))
             (remove #(= spec %))
             (some #(contains-vars? % defs new-resolved))))
      (or (= VAR (spec-type spec))
          (some #(contains-vars? % defs new-resolved) (:arglist spec))))))

(defn helper:userdef->ground [spec defs overwrite? alread-done]
  (let [resolved (if (= USERDEFINED (spec-type spec))
                   (resolve-definition-with-parameters spec defs)
                   spec)]
    (if (contains-vars? spec defs #{})
      (if (nil? (:arglist resolved))
        (intersect (->GroundSpec) resolved defs overwrite?)
        (update spec :arglist (partial reduce (fn [done new] (conj done (helper:userdef->ground new defs overwrite? (set (conj done new))))) [])))
      spec)))



(defn userdef->ground [spec defs overwrite?]
  (helper:userdef->ground spec defs overwrite? #{}))

(defrecord GroundSpec []
  spec
  (spec-type [spec] GROUND)
  (next-steps [spec term defs overwrite?]
    (if (contains? #{LIST, COMPOUND} (term-type term))
      [term (intersect spec (initial-spec term) defs overwrite?)]
      []))
  (next-steps [spec term defs] (next-steps spec term defs false))
  (intersect [spec other-spec defs overwrite?]
    (case+ (spec-type other-spec)
     (ANY, NONVAR, GROUND) spec
     USERDEFINED (userdef->ground other-spec defs overwrite?)
     SPECVAR spec
     VAR (if overwrite? spec DISJOINT)
     ERROR other-spec
     (intersect other-spec spec defs overwrite?)))
  (intersect [spec other-spec defs] (intersect spec other-spec defs false))
  printable
  (to-string [x] "Ground"))


(defn simplify-and [{arglist :arglist} defs overwrite?]
  (let [intersect (some->> arglist
                           distinct
                           (reduce #(intersect %1 %2 defs overwrite?)))]
    (if (error-spec? intersect)
      DISJOINT
      intersect)))


(defn- add-to-one-of [defs so-far e]
  (let [one-direction (apply vector (distinct (remove (partial supertype? defs e) so-far)))]
    (if (every? #(not (supertype? defs % e)) one-direction)
      (apply vector (distinct (conj one-direction e)))
      one-direction)))

(defn simplify-or [spec defs]
  (let [simplified-or (-> spec
                          (update :arglist distinct)
                          (update :arglist (partial mapcat #(if (= OR (spec-type %)) (:arglist %) [%])))
                          (update :arglist #(reduce (partial add-to-one-of defs) [(first %)] (rest %)))
                          (update :arglist (partial apply vector)))]
    (case (count (:arglist simplified-or))
      0 DISJOINT
      1 (first (:arglist simplified-or))
      (if (some #{ANY} (map spec-type (:arglist simplified-or)))
        (->AnySpec)
        simplified-or))))

(defrecord AndSpec [arglist]
  spec
  (spec-type [spec] AND)
  (next-steps [spec term defs overwrite?]
    (if-let [suitable-spec (intersect spec (initial-spec term) defs overwrite?)]
      (if (= AND (spec-type suitable-spec))
        (interleave (repeat (count (.arglist suitable-spec)) term) (.arglist suitable-spec))
        [term suitable-spec])
      []))
  (next-steps [spec term defs] (next-steps spec term defs false))
  (intersect [spec other-spec defs overwrite?]
    (case+ (spec-type other-spec)
           AND (-> other-spec
                   (update :arglist (partial concat arglist))
                   (simplify-and defs overwrite?)
                   replace-error-spec-with-intersect-error)
           OR (-> other-spec
                  (update :arglist (partial map #(->AndSpec (conj arglist %))))
                  (update :arglist (partial map #(simplify-and % defs overwrite?)))
                  (update :arglist (partial remove error-spec?))
                  (simplify-or defs)
                  replace-error-spec-with-intersect-error)
           ERROR other-spec
           (-> spec
               (update :arglist #(conj % other-spec))
               (simplify-and defs overwrite?)
               replace-error-spec-with-intersect-error)))
  (intersect [spec other-spec defs] (intersect spec other-spec defs false))
  printable
  (to-string [x] (str "And(" (to-arglist arglist) ")")))

(defrecord OneOfSpec [arglist]
  spec
  (spec-type [spec] OR)
  (next-steps [spec term defs overwrite?]
    (if-let [{arglist :arglist :as suitable-spec} (intersect spec (initial-spec term) defs overwrite?)]
      (if (= OR (spec-type suitable-spec))
        (->> arglist
             (map #(next-steps % term defs overwrite?))
             (map (partial apply hash-map))
             (map (partial reduce-kv (fn [m k v] (update m k #(set (conj % v)))) {}))
             (apply merge-with into)
             (reduce-kv (fn [m k v] (conj m k (simplify-or (->OneOfSpec (apply vector v)) defs))) []))
        [term suitable-spec])
      []))
  (next-steps [spec term defs] (next-steps spec term defs false))
  (intersect [spec other-spec defs overwrite?]
    (case+ (spec-type other-spec)
           OR (->> (for [x arglist
                         y (.arglist other-spec)]
                     [x y])
                   (map ->AndSpec)
                   (map #(simplify-and % defs overwrite?))
                   (remove error-spec?)
                   (apply vector)
                   ->OneOfSpec
                   (#(simplify-or % defs))
                   replace-error-spec-with-intersect-error)
           AND (-> spec
                   (update :arglist (partial map #(->AndSpec (conj (.arglist other-spec) %))))
                   (update :arglist (partial map #(simplify-and % defs overwrite?)))
                   (update :arglist (partial remove error-spec?))
                   (simplify-or defs)
                   replace-error-spec-with-intersect-error)
           ERROR other-spec
           (-> spec
               (update :arglist (partial map #(intersect other-spec % defs overwrite?)))
               (update :arglist (partial remove error-spec?))
               (simplify-or defs)
               replace-error-spec-with-intersect-error
               )))
  (intersect [spec other-spec defs] (intersect spec other-spec defs false))
  printable
  (to-string [x] (str "OneOf(" (to-arglist arglist) ")")))


(defn- intersect-userdef-with-userdef [userdef1 userdef2 defs overwrite?]
  (if (and (= (:name userdef1) (:name userdef2))
           (= (count (:arglist userdef1)) (count (:arglist userdef2))))
    (if (nil? (:arglist userdef1))
      userdef1
      (update userdef1 :arglist (partial map #(intersect %1 %2 defs overwrite?) (:arglist userdef2))))
    (let [def1 (resolve-definition-with-parameters userdef1 defs)
          def2 (resolve-definition-with-parameters userdef2 defs)]
      (intersect def1 def2 defs overwrite?))))

(defrecord UserDefinedSpec [name]
  spec
  (spec-type [spec] USERDEFINED)
  (next-steps [spec term defs] [])
  (next-steps [spec term defs overwrite?] [])
  (intersect [spec other-spec defs overwrite?]
    (case+ (spec-type other-spec)
           ANY spec
           USERDEFINED (intersect-userdef-with-userdef spec other-spec defs overwrite?)
           SPECVAR spec
           GROUND (intersect other-spec spec defs overwrite?)
           (AND, OR) (intersect other-spec (resolve-definition-with-parameters spec defs) defs overwrite?)
           VAR (if overwrite? spec DISJOINT)
           ERROR other-spec
           (intersect (resolve-definition-with-parameters spec defs) other-spec defs overwrite?)))
  (intersect [spec other-spec defs] (intersect spec other-spec defs false))
  printable
  (to-string [x] (if (contains? x :arglist)
                   (str name "(" (to-arglist (:arglist x)) ")")
                   (str name))))

(defrecord NonvarSpec []
  spec
  (spec-type [spec] NONVAR)
  (next-steps [spec term defs overwrite?]
    (if (contains? #{LIST, COMPOUND} (term-type term))
      [term (intersect spec (initial-spec term) defs overwrite?)]
      []))
  (next-steps [spec term defs] (next-steps spec term defs false))
  (intersect [spec other-spec defs overwrite?]
    (case+ (spec-type other-spec)
           (NONVAR, ANY) spec
           GROUND other-spec
           USERDEFINED (intersect (resolve-definition-with-parameters other-spec defs) spec defs overwrite?)
           SPECVAR spec
           VAR (if overwrite? spec DISJOINT)
           ERROR other-spec
           (intersect other-spec spec defs overwrite?)))
  (intersect [spec other-spec defs] (intersect spec other-spec defs false))
  printable
  (to-string [x] "Nonvar"))

(defrecord SpecvarSpec [name]
  spec
  (spec-type [spec] SPECVAR)
  (next-steps [spec term defs] [])
  (next-steps [spec term defs overwrite?] [])
  (intersect [spec other-spec defs overwrite?] other-spec)
  (intersect [spec other-spec defs] other-spec)
  printable
  (to-string [x] (str "Specvar(" (if (.startsWith (str name) "G__") (apply str (drop 3 (str name))) (str name)) ")")))


(defrecord VarTerm [name]
  term
  (term-type [term] VAR)
  (initial-spec [term] (->AnySpec))
  printable
  (to-string [x] (str name)))

(defrecord AnonVarTerm [name]
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
                           (->ListSpec (simplify-or (->OneOfSpec [(initial-spec head) (:type (initial-spec tail))]) nil))
                           (->ListSpec (->AnySpec)))))
  printable
  (to-string [x]
    (case+ (term-type tail)
           ATOMIC (str "[" (to-string head) "]")
           EMPTYLIST (str "[" (to-string head) "]")
           VAR (str "[" (to-string head) "|" (to-string tail) "]")
           LIST (str "[" (to-arglist (get-elements-of-list x)) "]")
           ATOM (if (= tail "[]") (str "[" (to-string head) "]") "error:list-with-atom-tail"))))


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

(defn map-to-term [input-m]
  (if (not (map? input-m))
    (log/error (str input-m) " is " (type input-m))
    (let [m (dissoc input-m :type)]
      (case (:type input-m)
        :anon_var (map->AnonVarTerm m)
        :var (map->VarTerm m)
        :atom (map->AtomTerm m)
        :number (map->NumberTerm m)
        :integer (map->IntegerTerm m)
        :float (map->FloatTerm m)
        :list (map->ListTerm (-> m
                                 (update :head map-to-term)
                                 (update :tail map-to-term)))
        :compound (map->CompoundTerm (update m :arglist #(map map-to-term %)))
        :empty-list (->EmptyListTerm)
        :string (map->StringTerm m)
        :should-not-happen (map->ShouldNotHappenTerm m)
        (do
          (log/error "No case for" input-m "in map-to-term")
          (->AtomTerm "ERROR"))))))

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
    :and (map->AndSpec (update m :arglist (partial map map-to-spec)))
    :one-of (map->OneOfSpec (update m :arglist (partial map map-to-spec)))
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

(defn to-or-spec
  "Transforms a bunch of `specs` to a one-of spec."
  [defs & specs]
  (case (count specs)
    0 (->ErrorSpec "Cannot build empty one-of")
    1 (first specs)
    (simplify-or (->OneOfSpec specs) defs)))

(defn to-arglist [list]
  (clojure.string/join ", " (map to-string list)))

(defn replace-specvar-name-with-value [spec specvar-name replace-value]
  (case+ (spec-type spec)
    SPECVAR
    (if (= specvar-name (:name spec)) (assoc spec :name replace-value) spec)

    (USERDEFINED, OR, AND, COMPOUND, TUPLE)
    (update spec :arglist (fn [s] (seq (map #(replace-specvar-name-with-value % specvar-name replace-value) s))))

    LIST
    (update spec :type #(replace-specvar-name-with-value % specvar-name replace-value))

    spec
    ))

(defn replace-specvars-with-spec [spec specvar-name replace-spec]
  (case+ (spec-type spec)
    SPECVAR
    (if (= specvar-name (:name spec)) replace-spec spec)

    (USERDEFINED, OR, AND, COMPOUND, TUPLE)
    (update spec :arglist (fn [s] (seq (map #(replace-specvars-with-spec % specvar-name replace-spec) s))))

    LIST
    (update spec :type #(replace-specvars-with-spec % specvar-name replace-spec))

    spec
    ))

(defn find-specvars [spec]
  (case+ (spec-type spec)
    SPECVAR [spec]
    (OR, AND, COMPOUND, TUPLE) (distinct (reduce concat (map find-specvars (.arglist spec))))
    USERDEFINED (if (contains? spec :arglist) (distinct (reduce concat (map find-specvars (:arglist spec)))) [])
    LIST (find-specvars (.type spec))
    []))

(defn get-definition-of-alias [defs user-defined-alias]
  (get defs (dissoc user-defined-alias :origin)))

(defn resolve-definition-with-parameters
  "User-defined specs can have parameters and when in use in spec annotations,
  there are values assigned to these parameters. To get the correct definition,
  we have to replace the parameter with their value."
  [{n :name arglist :arglist :as user-def-spec} defs]
  (if (nil? arglist)
    (get-definition-of-alias defs user-def-spec)
    (let [alias (->> defs
                     keys
                     (filter #(= (:name %) n))
                     (filter #(= (count (:arglist %)) (count arglist)))
                     first)
          definition (get-definition-of-alias defs alias)
          replace-map (apply hash-map (interleave (map :name (:arglist alias)) arglist))]
      (reduce-kv replace-specvars-with-spec definition replace-map))))


(defn supertype? [defs parent child]
  (cond
      (= SPECVAR (spec-type parent)) false
      (= OR (spec-type parent)) (some #(= child (intersect % child defs)) (:arglist parent))
      :default (= child (intersect parent child defs))))

(defn has-specvars [spec]
  (or (= SPECVAR (spec-type spec))
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



                                        ;---------------------------------------------------------------------------

(defmulti is-child-of (fn [child parent] [(spec-type child) (spec-type parent)]))

(def hierarchy
  (-> (make-hierarchy)
      (derive INTEGER NUMBER)
      (derive FLOAT NUMBER)
      (derive ATOM ATOMIC)
      (derive NUMBER ATOMIC)
      (derive STRING ATOMIC)
      (derive EXACT ATOM)
      (derive ATOMIC GROUND)
      (derive GROUND NONVAR)
      (derive NONVAR ANY)
      (derive VAR ANY)
      (derive LIST :compounds)
      (derive TUPLE :compounds)
      (derive COMPOUND :compounds)
      ))

(defmulti intersect-pre-spec (fn [userdefs spec other-spec] [(spec-type spec) (spec-type other-spec)]) :hierarchy #'hierarchy)

(defmethod intersect-pre-spec :default [userdef left right]
  (if (isa? hierarchy (spec-type left) (spec-type right))
    left
    (if (isa? hierarchy (spec-type right) (spec-type left))
      right
      DISJOINT)))


(defmethod intersect-pre-spec [EXACT EXACT] [_ e1 e2]
  (if (= (.value e1) (.value e2))
    e1
    DISJOINT))
(defmethod intersect-pre-spec [EXACT STRING] [_ _ _] DISJOINT)
(defmethod intersect-pre-spec [EXACT NUMBER] [_ _ _] DISJOINT)
(defmethod intersect-pre-spec [STRING EXACT] [_ _ _] DISJOINT)
(defmethod intersect-pre-spec [NUMBER EXACT] [_ _ _] DISJOINT)



(defmethod intersect-pre-spec [NONVAR VAR] [_ l any] DISJOINT)
(defmethod intersect-pre-spec [VAR NONVAR] [_ l any] DISJOINT)

(defmethod intersect-pre-spec [SPECVAR ANY] [_ _ l] l)
(defmethod intersect-pre-spec [ANY SPECVAR] [_ l _] l)

(defmethod intersect-pre-spec [EMPTYLIST LIST] [_ l _] l)
(defmethod intersect-pre-spec [EMPTYLIST TUPLE] [_ el tuple]
  (if (empty? (.arglist tuple))
    el
    DISJOINT))
(defmethod intersect-pre-spec [EMPTYLIST NUMBER] [_ _ _] DISJOINT)
(defmethod intersect-pre-spec [EMPTYLIST ATOM] [_ _ _] DISJOINT)
(defmethod intersect-pre-spec [EMPTYLIST STRING] [_ _ _] DISJOINT)
(defmethod intersect-pre-spec [EMPTYLIST VAR] [_ _ _] DISJOINT)
(defmethod intersect-pre-spec [EMPTYLIST ATOMIC] [_ _ _] (->EmptyListSpec))
(defmethod intersect-pre-spec [EMPTYLIST SPECVAR] [_ _ _] (->EmptyListSpec))
(defmethod intersect-pre-spec [EMPTYLIST ANY] [_ _ _] (->EmptyListSpec))
(defmethod intersect-pre-spec [EMPTYLIST LIST] [_ _ _] (->EmptyListSpec))

(defmethod intersect-pre-spec [LIST EMPTYLIST] [_ _ _] (->EmptyListSpec))
(defmethod intersect-pre-spec [ATOMIC EMPTYLIST] [_ _ _] (->EmptyListSpec))
(defmethod intersect-pre-spec [ANY EMPTYLIST] [_ _ _] (->EmptyListSpec))
(defmethod intersect-pre-spec [SPECVAR EMPTYLIST] [_ _ _] (->EmptyListSpec))
(defmethod intersect-pre-spec [ATOM EMPTYLIST] [_ _ _] DISJOINT)
(defmethod intersect-pre-spec [STRING EMPTYLIST] [_ _ _] DISJOINT)
(defmethod intersect-pre-spec [NUMBER EMPTYLIST] [_ _ _] DISJOINT)
(defmethod intersect-pre-spec [VAR EMPTYLIST] [_ _ _] DISJOINT)

(defmethod intersect-pre-spec [EMPTYLIST EMPTYLIST] [_ _ _] (->EmptyListSpec))

(defmethod intersect-pre-spec [LIST ATOMIC] [_ _ _] (->EmptyListSpec))
(defmethod intersect-pre-spec [LIST ATOM] [_ _ _] DISJOINT)
(defmethod intersect-pre-spec [LIST NUMBER] [_ _ _] DISJOINT)
(defmethod intersect-pre-spec [LIST STRING] [_ _ _] DISJOINT)
(defmethod intersect-pre-spec [ATOMIC LIST] [_ _ _] (->EmptyListSpec))
(defmethod intersect-pre-spec [ATOM LIST] [_ _ _] DISJOINT)
(defmethod intersect-pre-spec [NUMBER LIST] [_ _ _] DISJOINT)
(defmethod intersect-pre-spec [STRING LIST] [_ _ _] DISJOINT)

(defmethod intersect-pre-spec [SPECVAR SPECVAR] [_ _ _] (->AnySpec))

(defmethod intersect-pre-spec [:compounds VAR] [_ _ _] DISJOINT)
(defmethod intersect-pre-spec [VAR :compounds] [_ _ _] DISJOINT)

(defmethod intersect-pre-spec [:compounds ANY] [_ l _] l)
(defmethod intersect-pre-spec [ANY :compounds] [_ _ l] l)
(defmethod intersect-pre-spec [:compounds SPECVAR] [_ l _] l)
(defmethod intersect-pre-spec [SPECVAR :compounds] [_ _ l] l)

(defmethod intersect-pre-spec [:compounds ATOMIC] [_ _ _] DISJOINT)
(defmethod intersect-pre-spec [ATOMIC :compounds] [_ _ _] DISJOINT)
(defmethod intersect-pre-spec [GROUND :compounds] [u g c] (intersect-pre-spec u c g))
(defmethod intersect-pre-spec [:compounds GROUND] [u compound ground]
  (if (:type compound)
    (replace-error-spec-with-intersect-error (update compound :type (partial intersect-pre-spec u (->GroundSpec))))
    (replace-error-spec-with-intersect-error (update compound :arglist (partial map (partial intersect-pre-spec u (->GroundSpec)))))))


(intersect-pre-spec {} (->StringSpec) (->ExactSpec "cake"))

(defmethod intersect-pre-spec [LIST LIST] [userdefs l r]
  (replace-error-spec-with-intersect-error (update l :type (partial intersect-pre-spec userdefs (.type r)))))
(defmethod intersect-pre-spec [LIST TUPLE] [userdefs list tuple]
  (replace-error-spec-with-intersect-error (update tuple :arglist (partial map (partial intersect-pre-spec userdefs (.type list))))))

(defmethod intersect-pre-spec [TUPLE LIST] [userdefs tuple list]
  (replace-error-spec-with-intersect-error (update tuple :arglist (partial map (partial intersect-pre-spec userdefs (.type list))))))
(defmethod intersect-pre-spec [TUPLE TUPLE] [userdefs tuple1 tuple2]
  (if (= (count (.arglist tuple1)) (count (.arglist tuple2)))
    (replace-error-spec-with-intersect-error (update tuple1 :arglist (partial map (partial intersect-pre-spec userdefs) (.arglist tuple2))))
    DISJOINT))
(defmethod intersect-pre-spec [TUPLE EMPTYLIST] [_ tuple el]
  (if (empty? (.arglist tuple))
    el
    DISJOINT))

(defmethod intersect-pre-spec [COMPOUND COMPOUND] [userdefs comp1 comp2]
  (cond
    (nil? (.functor comp1)) comp2
    (nil? (.functor comp2)) comp1
    (and (= (.functor comp1) (.functor comp2))
         (= (count (.arglist comp1)) (count (.arglist comp2)))) (replace-error-spec-with-intersect-error (update comp1 :arglist (partial map (partial intersect-pre-spec userdefs) (.arglist comp2))))
    :else DISJOINT))


(defmethod intersect-pre-spec [EXACT EXACT] [_ e1 e2]
  (if (= (.value e1) (.value e2))
    e1
    DISJOINT))

(defmethod intersect-pre-spec [EXACT ATOMIC] [_ e a] e)
(defmethod intersect-pre-spec [ATOMIC EXACT] [_ a e] e)

(intersect-pre-spec {}  (->AtomSpec) (->ListSpec (->FloatSpec)))

(comment

    (defmethod intersect-pre-spec [USERDEFINED ANY] [defs alias other]
    (replace-error-spec-with-intersect-error (intersect-pre-spec defs (resolve-definition-with-parameters alias defs) other)))
  (defmethod intersect-pre-spec [ANY USERDEFINED] [defs other alias]
    (replace-error-spec-with-intersect-error (intersect-pre-spec defs (resolve-definition-with-parameters alias defs) other)))
  (defmethod intersect-pre-spec [USERDEFINED USERDEFINED] [defs ali baba]
    (replace-error-spec-with-intersect-error (intersect-pre-spec defs (resolve-definition-with-parameters ali defs) (resolve-definition-with-parameters baba defs))))

  (defmethod intersect-pre-spec [OR ANY] [userdefs or any]
    (intersect-pre-spec userdefs any or))
  (defmethod intersect-pre-spec [ANY OR] [userdefs any or]
    (-> or
        (update :arglist (partial map (partial intersect-pre-spec userdefs any)))
        (update :arglist (partial remove error-spec?))
        (simplify-or userdefs)
        replace-error-spec-with-intersect-error
        )))


(defmulti intersect-post-spec (fn [spec other-spec user-definitions] [(spec-type spec) (spec-type other-spec)]))
(defmulti intersect-post-spec-for-var-dom (fn [other-spec user-definitions] [(spec-type spec) (spec-type other-spec)]))
