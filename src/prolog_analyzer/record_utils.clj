(ns prolog-analyzer.record-utils
  (:require [prolog-analyzer.records :as r]
            [prolog-analyzer.state :refer [user-typedefs]]
            [clojure.tools.logging :as log]
            [prolog-analyzer.utils :as utils :refer [case+ duocase]]))

(declare intersect)
(declare simplify)
(declare find-placeholders)
(declare contains-placeholder?)
(declare replace-placeholder-with-alias)

(defn var-spec? [spec]
  (if (nil? spec)
    false
    (= r/VAR (r/safe-spec-type spec "var-spec"))))

(defn any-spec? [spec]
  (if (nil? spec)
    true
    (= r/ANY (r/safe-spec-type spec "any-spec"))))

(defn or-spec? [spec]
  (= r/OR (r/spec-type spec)))

(defn and-spec? [spec]
  (= r/AND (r/safe-spec-type spec "and-spec?")))

(defn user-defined-spec? [spec]
  (= r/USERDEFINED (r/safe-spec-type spec "user-defined-spec?")))

(defn placeholder-spec? [spec]
  (= r/PLACEHOLDER (r/safe-spec-type spec "placeholder-spec?")))

(defn error-spec? [spec]
  (or (nil? spec)
      (= r/ERROR (r/safe-spec-type spec "error"))
      (if (contains? spec :type) (error-spec? (.type spec)) false)
      (if (contains? spec :arglist) (some error-spec? (:arglist spec)) false)
      ))

(defn empty-list-term? [term]
  (= r/EMPTYLIST (r/term-type term)))

(defn var-term? [term]
  (= r/VAR (r/term-type term)))

(defn nonvar-term? [term]
  ((complement var-term?) term))

(defn tuple-spec? [spec]
  (= r/TUPLE (r/safe-spec-type spec "tuple-spec?")))

(declare replace-specvars-with-spec)

(defn replace-specvars-with-spec [type specvar-name replace-spec]
  (case+ (r/spec-type type)
         r/SPECVAR (if (= specvar-name (:name type)) replace-spec type)

         (r/OR,r/AND) (-> type
                      (update :arglist (partial map #(replace-specvars-with-spec % specvar-name replace-spec)))
                      (update :arglist set))

         (r/USERDEFINED, r/COMPOUND, r/TUPLE) (-> type
                                            (update :arglist (partial map #(replace-specvars-with-spec % specvar-name replace-spec)))
                                            (update :arglist (partial apply vector)))
         r/LIST (update type :type #(replace-specvars-with-spec % specvar-name replace-spec))
         type
         ))

(defn resolve-definition-with-parameters
  "User-defined specs can have parameters and when in use in spec annotations,
  there are values assigned to these parameters. To get the correct definition,
  we have to replace the parameter with their value."
  [{n :name arglist :arglist :as user-def-spec}]
  (let [beautify (fn [spec] (if (or-spec? spec) (update spec :arglist set) spec))
        res (if (nil? arglist)
              (get @user-typedefs user-def-spec nil)
              (let [alias (->> @user-typedefs
                               keys
                               (filter #(= (:name %) n))
                               (filter #(= (count (:arglist %)) (count arglist)))
                               first)
                    definition (get @user-typedefs alias nil)
                    replace-map (apply hash-map (interleave (map :name (:arglist alias)) arglist))
                    result (reduce-kv replace-specvars-with-spec definition replace-map)]
                (some-> result beautify)
                ))]
    (or
     res
     (do
       (log/error "Spec " n " was used, but not defined, falling back to any.")
       (r/->AnySpec)))))

(defn- supertype? [parent child]
  (if (= r/OR (r/safe-spec-type parent "supertype"))
    (some #(= child (intersect % child false)) (:arglist parent))
    (= child (intersect parent child false))))


(defn- remove-subsets-in-or [{arglist :arglist :as spec}]
  (if (= r/OR (r/safe-spec-type spec "remove subsets"))
    (r/->OneOfSpec (reduce
                    (fn [new-arglist type]
                      (if (some #(supertype? % type) (remove #(= type %) arglist))
                        new-arglist
                        (conj new-arglist type)))
                    #{}
                    arglist))
    spec))


(defn- same-user-defined-spec? [{name-a :name arglist-a :arglist} {name-b :name arglist-b :arglist}]
  (and (= name-a name-b) (= (count arglist-a) (count arglist-b))))

(defn- same-functors? [left right]
  (= (:functor left) (:functor right)))

(defn- same-arg-number? [left right]
  (= (count (:arglist left)) (count (:arglist right))))

(defn- error-if-empty-arglist [spec]
  (if (and (or-spec? spec) (empty? (:arglist spec))) (r/DISJOINT) spec))

(defn- extract-single [spec]
  (if (and (or-spec? spec) (= 1 (count (:arglist spec)))) (first (:arglist spec)) spec))

(defn- simplify-pair-tuples-in-or [{arglist :arglist :as spec}]
  (let [tuple-arglists (map :arglist arglist)]
    (if (apply = (map last tuple-arglists))
      (r/->TupleSpec [(r/->OneOfSpec (set (map first tuple-arglists))) (last (first tuple-arglists))])
      (if (apply = (map first tuple-arglists))
        (r/->TupleSpec [(first (first tuple-arglists)) (r/->OneOfSpec (set (map last tuple-arglists)))])
        spec))))

(defn extract-singleton-tuples
  ([{arglist :arglist :as spec} initial?]
   (if (and
        (or-spec? spec)
        (not-empty arglist)
        (every? tuple-spec? arglist))
     (if (every? #(= 1 (count (:arglist %))) arglist)
       (simplify
        (r/->TupleSpec [(r/->OneOfSpec (->> arglist
                                            (map :arglist)
                                            (map first)
                                            set))])
        initial?)
       (if (every? #(= 2 (count (:arglist %))) arglist)
         (simplify-pair-tuples-in-or spec)
         spec))
     spec)))

(defn remove-error-specs [spec]
  (if (or-spec? spec)
    (-> spec
        (update :arglist (partial remove #(error-spec? %)))
        (update :arglist set))
    spec))

(defn simplify
  ([spec] (simplify spec false))
  ([spec initial?]
   (case+ (r/safe-spec-type spec "simplify")
          r/OR (-> spec
                   (update :arglist (partial mapcat #(if (or-spec? %) (:arglist %) [%])))
                   remove-error-specs
                   (update :arglist (partial map #(simplify % initial?)))
                   (update :arglist set)
                   (extract-singleton-tuples initial?)
                   remove-subsets-in-or
                   remove-error-specs
                   error-if-empty-arglist
                   extract-single)
          r/AND (reduce #(intersect %1 %2 initial?) (r/->AnySpec) (:arglist spec))
          r/LIST (if (error-spec? (:type spec))
                   (:type spec)
                   (update spec :type simplify initial?))
          r/TUPLE (if (empty? (:arglist spec))
                    (r/->EmptyListSpec)
                    (-> spec
                        (update :arglist (partial map #(simplify % initial?)))
                        (update :arglist (partial apply vector))))
          spec)))

(defn intersect* [left right initial? swap?]
  (let [swap (fn [] (when swap?
                     (intersect* right left initial? false)))
        intersection (duocase [(r/safe-spec-type left "left") (r/safe-spec-type right "right")]
                              [r/ANY :idclol] right

                              [r/VAR r/VAR] right
                              [r/VAR r/ANY] left
                              [r/VAR r/USERDEFINED] (swap)
                              [r/VAR r/OR] (swap)
                              [r/VAR r/AND] (swap)
                              [r/VAR r/PLACEHOLDER] (swap)
                              [r/VAR :idclol] (when initial? right)
                              [r/GROUND r/LIST] (update right :type #(intersect left % initial?))
                              [r/GROUND r/COMPOUND] (if (nil? (:arglist right))
                                                      right
                                                      (update right :arglist (partial map #(intersect left % initial?))))
                              [r/GROUND r/TUPLE] (update right :arglist (partial map #(intersect left % initial?)))
                              [r/GROUND r/NONVAR] left
                              [r/GROUND r/VAR] (when initial? left)
                              [r/GROUND r/ANY] left
                              [r/GROUND r/OR] (swap)
                              [r/GROUND r/AND] (swap)
                              [r/GROUND r/USERDEFINED] (swap)
                              [r/GROUND r/PLACEHOLDER] (swap)
                              [r/GROUND :idclol] right

                              [r/NONVAR r/VAR] (when initial? left)
                              [r/NONVAR r/ANY] left
                              [r/NONVAR r/USERDEFINED] (swap)
                              [r/NONVAR r/PLACEHOLDER] (swap)
                              [r/NONVAR :idclol] right

                              [r/EMPTYLIST r/EMPTYLIST] right
                              [r/EMPTYLIST r/ATOMIC] left
                              [r/EMPTYLIST r/LIST] left
                              [r/EMPTYLIST r/COMPOUND] (when (nil? (:functor right)) left)
                              [r/EMPTYLIST r/TUPLE] (when (zero? (count (:arglist right))) left)
                              [r/EMPTYLIST :idclol] (swap)

                              [r/ATOMIC r/ATOMIC] right
                              [r/ATOMIC r/ATOM] right
                              [r/ATOMIC r/STRING] right
                              [r/ATOMIC r/NUMBER] right
                              [r/ATOMIC r/FLOAT] right
                              [r/ATOMIC r/INTEGER] right
                              [r/ATOMIC r/LIST] (r/->EmptyListSpec)
                              [r/ATOMIC r/EXACT] right
                              [r/ATOMIC :idclol] (swap)

                              [r/ATOM r/ATOM] right
                              [r/ATOM r/EXACT] right
                              [r/ATOM :idclol] (swap)

                              [r/EXACT r/EXACT] (when (= (:value left) (:value right)) left)
                              [r/EXACT :idclol] (swap)

                              [r/NUMBER r/NUMBER] right
                              [r/NUMBER r/INTEGER] right
                              [r/NUMBER r/FLOAT] right
                              [r/NUMBER :idclol] (swap)

                              [r/INTEGER r/INTEGER] right
                              [r/INTEGER :idclol] (swap)

                              [r/FLOAT r/FLOAT] right
                              [r/FLOAT :idclol] (swap)

                              [r/STRING r/STRING] right
                              [r/STRING :idclol] (swap)

                              [r/LIST r/LIST] (update left :type #(intersect % (:type right) initial?))
                              [r/LIST r/COMPOUND] (cond
                                                    (nil? (:functor right)) left
                                                    (r/incomplete-list-spec? right) left
                                                    :else nil)
                              [r/LIST r/TUPLE] (update right :arglist (partial map #(intersect % (:type left) initial?)))
                              [r/LIST :idclol] (swap)

                              [r/TUPLE r/TUPLE] (when (same-arg-number? left right)
                                                  (update left :arglist (partial map #(intersect %1 %2 initial?) (:arglist right))))
                              [r/TUPLE r/COMPOUND] (cond
                                                     (nil? (:functor right)) left
                                                     (r/incomplete-list-spec? right) left
                                                     :else nil)
                              [r/TUPLE :idclol] (swap)

                              [r/COMPOUND r/COMPOUND] (cond
                                                        (nil? (:functor left)) right
                                                        (nil? (:functor right)) left
                                                        (and (same-functors? left right) (same-arg-number? left right))
                                                        (update left :arglist (partial map #(intersect %1 %2 initial?) (:arglist right)))
                                                        :default nil)
                              [r/COMPOUND :idclol] (swap)

                              [r/USERDEFINED r/USERDEFINED] (cond
                                                              (and
                                                               (= (:name left) (:name right))
                                                               (= (:arglist left) (:arglist right) nil))
                                                              left

                                                              (and
                                                               (= (:name left) (:name right))
                                                               (= (count (:arglist left)) (count (:arglist right))))
                                                              (update left :arglist (partial map #(intersect %1 %2 initial?) (:arglist right)))

                                                              :default
                                                              (intersect (resolve-definition-with-parameters left) right initial?))
                              [r/USERDEFINED :idclol] (intersect (resolve-definition-with-parameters left) right initial?)

                              [r/OR r/OR] (let [new-arglist (set (for [a (:arglist left)
                                                                       b (:arglist right)]
                                                                   (intersect a b initial?)))]
                                            (assoc left :arglist new-arglist))
                              [r/OR r/AND] (swap)
                              [r/OR :idclol] (update left :arglist (partial map #(intersect % right initial?)))

                              [r/AND r/AND] (reduce #(intersect %1 %2 initial?) (r/->AnySpec) (concat (:arglist left) (:arglist right)))
                              [r/AND r/OR] (let [new-left (reduce #(intersect %1 %2 initial?) (r/->AnySpec) (:arglist left))
                                                 new-arglist (set (for [a (:arglist right)]
                                                                    (intersect new-left a initial?)))]
                                             (assoc right :arglist new-arglist))
                              [r/AND :idclol] (reduce #(intersect %1 %2 initial?) right (:arglist left))

                              [r/PLACEHOLDER r/ANY] left
                              [r/PLACEHOLDER :idclol] (-> left
                                                          (update :alias #(or % (r/->AnySpec)))
                                                          (update :alias intersect right initial?))
                              [r/ERROR :idclol] left

                              nil)]
    (simplify (or intersection (r/DISJOINT left right)) initial?)))


(defn intersect [spec-a spec-b initial?]
  (intersect* spec-a spec-b initial? true))


(defn to-tuple-spec
  "Transforms a bunch of `specs` to a tuple spec."
  [& specs]
  (if (empty? specs)
    (r/->TupleSpec [])
    (r/->TupleSpec specs)))

(defn to-head-tail-list [& terms]
  (if (empty? terms)
    (r/->EmptyListTerm)
    (r/->ListTerm (first terms) (apply to-head-tail-list (rest terms)))))

(defn replace-specvar-name-with-value [spec specvar-name replace-value]
  (case+ (r/safe-spec-type spec "replace-with-value")
         r/SPECVAR (if (= specvar-name (:name spec)) (assoc spec :name replace-value) spec)

         (r/OR,r/AND) (-> spec
                      (update :arglist (partial map #(replace-specvar-name-with-value % specvar-name replace-value)))
                      (update :arglist set))

         (r/USERDEFINED, r/COMPOUND, r/TUPLE)
         (-> spec
             (update :arglist (partial map #(replace-specvar-name-with-value % specvar-name replace-value)))
             (update :arglist (partial apply vector)))
         r/LIST
         (update spec :type #(replace-specvar-name-with-value % specvar-name replace-value))

         spec
         ))

(defn- remove-alias [placeholder-spec]
  (dissoc placeholder-spec :alias))

(defn- pack-together [placeholders]
  (->> placeholders
       (group-by :name)
       (reduce-kv #(assoc %1 %2 (remove nil? (map :alias %3))) {})
       (reduce-kv #(assoc %1 %2 (if (empty? %3) nil (simplify (r/->OneOfSpec (set (map remove-alias %3))) false))) {})
       (reduce-kv #(conj %1 (if (nil? %3) (r/->PlaceholderSpec %2) (assoc (r/->PlaceholderSpec %2) :alias %3))) [])))

(defn create-replace-map [placeholders]
  (->> placeholders
       (remove #(contains-placeholder? (or (:alias %) (r/->PlaceholderSpec "X"))))
       (reduce #(assoc %1 (:name %2) (:alias %2)) {})
       ))

(defn replace-alias [replace-map placeholder-spec]
  (if (contains? placeholder-spec :alias)
    (update placeholder-spec :alias (partial replace-placeholder-with-alias replace-map))
    placeholder-spec))

(defn simplify-placeholders [placeholders]
  (let [replace-map (create-replace-map placeholders)]
    (if (->> placeholders
             (map :alias)
             (remove nil?)
             (some contains-placeholder?))
      (simplify-placeholders (map (partial replace-alias replace-map) placeholders))
      placeholders)))

(defn find-placeholders [spec]
  (let [placeholders (case+ (r/safe-spec-type spec "Find Placeholders")
                            r/PLACEHOLDER (if (contains-placeholder? (or (:alias spec) (r/->AnySpec))) (conj (find-placeholders (:alias spec)) spec) [spec])
                            (r/OR, r/AND, r/COMPOUND, r/TUPLE) (set (mapcat find-placeholders (.arglist spec)))
                            r/USERDEFINED (if (contains? spec :arglist) (set (mapcat find-placeholders (:arglist spec))) [])
                            r/LIST (find-placeholders (.type spec))
                            [])]
    (->> placeholders
         set
         pack-together
         simplify-placeholders
         set)))

(defn contains-placeholder? [spec]
  (case+ (r/safe-spec-type spec "Contains Placeholder")
         r/PLACEHOLDER true
         (r/OR, r/AND, r/COMPOUND, r/TUPLE) (some contains-placeholder? (:arglist spec))
         r/USERDEFINED (if (contains? spec :arglist) (some contains-placeholder? (:arglist spec)) false)
         r/LIST (contains-placeholder? (:type spec))
         false))

(defn- compatible [super sub]
  (if (error-spec? (intersect super sub false))
    (r/->ErrorSpec (str "Placeholder was not compatible"))
    super))

(defn- fill-placeholder [{super-of :super-of n :name :as spec} alias-map]
  (if (nil? super-of)
    (get alias-map n (r/->AnySpec))
    (compatible (get alias-map n (r/->AnySpec)) (get alias-map super-of (r/->AnySpec)))))

(defn replace-placeholder-with-alias [alias-map spec]
  (case+ (r/safe-spec-type spec "Replace Placeholder")
         r/PLACEHOLDER (fill-placeholder spec alias-map)
         (r/OR, r/AND) (-> spec
                           (update :arglist (partial map (partial replace-placeholder-with-alias alias-map)))
                           (update :arglist set))
         (r/COMPOUND, r/TUPLE) (-> spec
                                   (update :arglist (partial map (partial replace-placeholder-with-alias alias-map)))
                                   (update :arglist (partial apply vector)))
         r/USERDEFINED (if (contains? spec :arglist)
                         (-> spec
                             (update :arglist (partial map (partial replace-placeholder-with-alias alias-map)))
                             (update :arglist (partial apply vector)))
                         spec)
         r/LIST (update spec :type (partial replace-placeholder-with-alias alias-map))
         spec
         ))

(defn replace-var-with-any [spec]
  (case+ (r/safe-spec-type spec "Replace Placeholder")
         r/VAR (r/->AnySpec)
         r/PLACEHOLDER (if (contains? spec :alias) (update spec :alias replace-var-with-any) spec)
         (r/OR, r/AND, r/COMPOUND, r/TUPLE) (-> spec
                                                (update :arglist (partial map replace-var-with-any))
                                                (update :arglist (partial apply vector)))
         r/USERDEFINED (if (contains? spec :arglist)
                         (-> spec
                             (update :arglist (partial map replace-var-with-any))
                             (update :arglist (partial apply vector)))
                         spec)
         r/LIST (update spec :type replace-var-with-any)
         spec
         ))


(defn length-of-list-term [{head :head tail :tail :as list}]
  (cond
    (nil? head) 0
    (nil? tail) 1
    (= r/VAR (r/term-type tail)) :inf
    :default
    (let [tail-length (length-of-list-term tail)]
      (if (= :inf tail-length)
        :inf
        (inc tail-length)))))


(defn nonvar-term? [term]
  (not= (r/term-type term) r/VAR))

(defn maybe-spec [spec]
  (cond
    (:arglist spec) (r/->OneOfSpec #{(r/->VarSpec) (update spec :arglist (partial map (fn [x] (r/->AnySpec))))})
    (:type spec) (r/->OneOfSpec #{(r/->VarSpec) (assoc spec :type (r/->AnySpec))})
    :else (if (any-spec? spec) spec (r/->OneOfSpec #{(r/->VarSpec) spec}))))

(defn list-term? [term]
  (= r/LIST (r/term-type term)))

(defn compound-term? [term]
  (= r/COMPOUND (r/term-type term)))

(defn single-term? [term]
  (and (not (list-term? term))
       (not (compound-term? term))))

(defn head [list-term]
  (:head list-term))

(defn tail [list-term]
  (:tail list-term))

(defn arg [compound-or-tuple n]
  (nth (:arglist compound-or-tuple) n))

(defn compound-with-anys [fun n]
  (r/->CompoundSpec fun (apply vector (repeat n (r/->AnySpec)))))

(defn tuple-with-anys [n]
  (r/->TupleSpec (apply vector (repeat n (r/->AnySpec)))))

(defn list-with-anys []
  (r/->ListSpec (r/->AnySpec)))

(defn non-empty-intersection [spec1 spec2 initial?]
  (not (error-spec? (intersect spec1 spec2 initial?))))

(defn replace-var-with-any [spec]
  (case+ (r/spec-type spec)
         r/VAR (r/->AnySpec)
         r/LIST (update spec :type replace-var-with-any)
         (r/TUPLE, r/COMPOUND) (update spec :arglist #(->> %
                                                           (map replace-var-with-any)
                                                           (apply vector)))
         (r/USERDEFINED) (if (nil? (:arglist spec)) spec (update spec :arglist #(->> % (map replace-var-with-any) (apply vector))))
         spec))

(defn same? [spec1 spec2]
  (if (= (r/spec-type spec1) (r/spec-type spec2))
    (case+ (r/spec-type spec1)
           r/TUPLE (every? #(apply same? %) (map vector (:arglist spec1) (:arglist spec2)))
           r/LIST (same? (:type spec1) (:type spec2))
           r/COMPOUND (and (= (:functor spec1) (:functor spec2)) (every? #(apply same? %) (map vector (:arglist spec1) (:arglist spec2))))
           (r/OR, r/AND) (= (set (:arglist spec1)) (set (:arglist spec2)))
           r/EXACT (= (:value spec1) (:value spec2))
           true)
    false))

(def init false)

(simplify
 (intersect
  (r/->ListSpec (r/->AnySpec))
  (r/->OneOfSpec #{(r/->TupleSpec [(r/->IntegerSpec) (r/->ListSpec (r/->IntegerSpec))])
                   (r/->TupleSpec [(r/->IntegerSpec) (r/->VarSpec)])
                   (r/->TupleSpec [(r/->VarSpec) (r/->ListSpec (r/->IntegerSpec))])})
  init)
 init)
