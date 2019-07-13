(ns prolog-analyzer.intersect
  (:require [prolog-analyzer.records :as r]
            [prolog-analyzer.utils :as utils :refer [case+ duocase]]))

(declare intersect)
(declare simplify)

(defn replace-specvars-with-spec [type specvar-name replace-spec]
  (case+ (r/spec-type type)
         (r/SPECVAR, r/UNION, r/COMPATIBLE) (if (= specvar-name (:name type)) replace-spec type)

         (r/OR, r/AND) (-> type
                           (update :arglist (partial map #(replace-specvars-with-spec % specvar-name replace-spec)))
                           (update :arglist set))

         (r/USERDEFINED, r/COMPOUND, r/TUPLE) (-> type
                                                  (update :arglist (partial map #(replace-specvars-with-spec % specvar-name replace-spec)))
                                                  (update :arglist (partial apply vector)))
         r/LIST (update type :type #(replace-specvars-with-spec % specvar-name replace-spec))
         type
         ))


(defn to-or-spec
  "Transforms a bunch of `specs` to a one-of spec."
  [defs & specs]
  (case (count specs)
    0 (r/->ErrorSpec "Cannot build empty one-of")
    1 (first specs)
    (simplify (r/->OneOfSpec (set specs)) defs)))


(defn resolve-definition-with-parameters
  "User-defined specs can have parameters and when in use in spec annotations,
  there are values assigned to these parameters. To get the correct definition,
  we have to replace the parameter with their value."
  [{n :name arglist :arglist :as user-def-spec} defs]
  (let [res (if (nil? arglist)
              (get defs user-def-spec nil)
              (let [alias (->> defs
                               keys
                               (filter #(= (:name %) n))
                               (filter #(= (count (:arglist %)) (count arglist)))
                               first)
                    definition (get defs alias nil)
                    replace-map (apply hash-map (interleave (map :name (:arglist alias)) arglist))
                    result (reduce-kv replace-specvars-with-spec definition replace-map)]
                (if (= r/OR (r/spec-type result))
                  (update result :arglist set)
                  result)))]
    (if (nil? res)
      (if (nil? user-def-spec)
        (throw (Exception. "User-defined spec was nil"))
        (throw (Exception. (str "Could not find definition of userdefined spec " (r/to-string user-def-spec)))))
      res)))

(defn- supertype? [defs parent child]
  (if (= r/OR (r/spec-type parent))
    (some #(= child (intersect % child defs)) (:arglist parent))
    (= child (intersect parent child defs))))

(defn remove-subsets-in-or [{arglist :arglist} defs]
  (r/->OneOfSpec (reduce
                  (fn [new-arglist type]
                    (if (some #(supertype? defs % type) (remove #(= type %) arglist))
                      new-arglist
                      (conj new-arglist type)))
                  #{}
                  arglist)))


(defn- same-user-defined-spec? [{name-a :name arglist-a :arglist} {name-b :name arglist-b :arglist}]
  (and (= name-a name-b) (= (count arglist-a) (count arglist-b))))

(defn- same-functors? [left right]
  (= (:functor left) (:functor right)))

(defn- same-arg-number? [left right]
  (= (count (:arglist left)) (count (:arglist right))))

(defn- error-if-empty-arglist [spec]
  (if (empty? (:arglist spec)) (r/DISJOINT) spec))

(defn- extract-single [spec]
  (if (= 1 (count (:arglist spec))) (first (:arglist spec)) spec))

(defn extract-singleton-tuples [{arglist :arglist :as spec}]
  (if (and (not-empty arglist) (every? #(and (= r/TUPLE (r/spec-type %)) (= 1 (count (:arglist %)))) arglist))
    (r/->TupleSpec [(r/->OneOfSpec (->> arglist
                                        (map :arglist)
                                        (map first)
                                        set))])
    spec))

(defn extract-lists [{arglist :arglist :as spec}]
  (if (and (not-empty arglist) (every? #(= r/LIST (r/spec-type %)) arglist))
    (r/->ListSpec (r/->OneOfSpec (->> arglist
                                  (map :arglist)
                                  (map first)
                                  set)))
    spec))


(extract-singleton-tuples (r/->OneOfSpec [(r/->TupleSpec [(r/->IntegerSpec)]) (r/->TupleSpec [(r/->AtomSpec)])]))


(defn simplify [spec defs]
  (case+ (r/spec-type spec)
         r/OR (-> spec
                  (update :arglist (partial mapcat #(if (= r/OR (r/spec-type %)) (:arglist %) [%])))
                  (update :arglist (partial remove #(= r/ERROR (r/spec-type %))))
                  (update :arglist (partial map #(simplify % defs)))
                  (update :arglist set)
                  extract-singleton-tuples
                  extract-lists
                  (remove-subsets-in-or defs)
                  error-if-empty-arglist
                  extract-single)
         r/AND (reduce #(intersect %1 %2 defs) (r/->AnySpec) (:arglist spec))
         r/LIST (if (r/error-spec? (:type spec))
                  (:type spec)
                  spec)
         spec))

(defn intersect [spec-a spec-b defs]
  (loop [left spec-a
         right spec-b
         first-time true]
    (let [intersection (duocase [(r/spec-type left) (r/spec-type right)]
                                [r/ANY :idclol] right

                                [r/VAR r/VAR] right
                                [r/VAR r/ANY] left
                                [r/VAR r/USERDEFINED] :swap
                                [r/VAR r/OR] :swap
                                [r/VAR r/AND] :swap
                                [r/VAR r/PLACEHOLDER] :swap
                                [r/VAR :idclol] nil

                                [r/GROUND r/LIST] (update right :type #(intersect left % defs))
                                [r/GROUND r/COMPOUND] (if (nil? (:arglist right))
                                                        right
                                                        (update right :arglist (partial map #(intersect left % defs))))
                                [r/GROUND r/TUPLE] (update right :arglist (partial map #(intersect left % defs)))
                                [r/GROUND r/NONVAR] left
                                [r/GROUND r/VAR] nil
                                [r/GROUND r/ANY] left
                                [r/GROUND r/OR] :swap
                                [r/GROUND r/AND] :swap
                                [r/GROUND r/USERDEFINED] :swap
                                [r/GROUND r/PLACEHOLDER] :wap
                                [r/GROUND :idclol] right

                                [r/NONVAR r/VAR] nil
                                [r/NONVAR r/ANY] left
                                [r/NONVAR r/USERDEFINED] :swap
                                [r/NONVAR r/PLACEHOLDER] :swap
                                [r/NONVAR :idclol] right

                                [r/EMPTYLIST r/EMPTYLIST] right
                                [r/EMPTYLIST r/ATOMIC] left
                                [r/EMPTYLIST r/LIST] left
                                [r/EMPTYLIST r/COMPOUND] (if (nil? (:functor right))
                                                           left
                                                           nil)
                                [r/EMPTYLIST r/TUPLE] (if (zero? (count (:arglist right)))
                                                        left
                                                        nil)
                                [r/EMPTYLIST :idclol] :swap

                                [r/ATOMIC r/ATOMIC] right
                                [r/ATOMIC r/ATOM] right
                                [r/ATOMIC r/STRING] right
                                [r/ATOMIC r/NUMBER] right
                                [r/ATOMIC r/FLOAT] right
                                [r/ATOMIC r/INTEGER] right
                                [r/ATOMIC r/LIST] (r/->EmptyListSpec)
                                [r/ATOMIC r/EXACT] right
                                [r/ATOMIC :idclol] :swap

                                [r/ATOM r/ATOM] right
                                [r/ATOM r/EXACT] right
                                [r/ATOM :idclol] :swap

                                [r/EXACT r/EXACT] (if (= (:value left) (:value right))
                                                    left
                                                    nil)
                                [r/EXACT :idclol] :swap

                                [r/NUMBER r/NUMBER] right
                                [r/NUMBER r/INTEGER] right
                                [r/NUMBER r/FLOAT] right
                                [r/NUMBER :idclol] :swap

                                [r/INTEGER r/INTEGER] right
                                [r/INTEGER :idclol] :swap

                                [r/FLOAT r/FLOAT] right
                                [r/FLOAT :idclol] :swap

                                [r/STRING r/STRING] right
                                [r/STRING :idclol] :swap

                                [r/LIST r/LIST] (update left :type #(intersect % (:type right) defs))
                                [r/LIST r/COMPOUND] (cond
                                                      (nil? (:functor right)) left
                                                      :else nil)
                                [r/LIST r/TUPLE] (update right :arglist (partial map #(intersect % (:type left) defs)))
                                [r/LIST :idclol] :swap

                                [r/TUPLE r/TUPLE] (if (same-arg-number? left right)
                                                    (update left :arglist (partial map #(intersect %1 %2 defs) (:arglist right)))
                                                    nil)
                                [r/TUPLE r/COMPOUND] (cond
                                                       (nil? (:functor right)) left
                                                       :else nil)
                                [r/TUPLE :idclol] :swap

                                [r/COMPOUND r/COMPOUND] (cond
                                                          (nil? (:functor left)) right
                                                          (nil? (:functor right)) left

                                                          (and (same-functors? left right) (same-arg-number? left right))
                                                          (update left :arglist (partial map #(intersect %1 %2 defs) (:arglist right)))

                                                          :default nil)
                                [r/COMPOUND :idclol] :swap

                                [r/USERDEFINED r/ANY] left
                                [r/USERDEFINED r/USERDEFINED] (cond
                                                                (and
                                                                 (= (:name left) (:name right))
                                                                 (= (:arglist left) (:arglist right) nil))
                                                                left

                                                                (and
                                                                   (= (:name left) (:name right))
                                                                   (= (count (:arglist left)) (count (:arglist right))))
                                                                (update left :arglist (partial map #(intersect %1 %2 defs) (:arglist right)))

                                                                :default
                                                                (intersect (resolve-definition-with-parameters left defs) right defs))
                                [r/USERDEFINED :idclol] (intersect (resolve-definition-with-parameters left defs) right defs)

                                [r/OR r/OR] (let [new-arglist (set (for [a (:arglist left)
                                                                         b (:arglist right)]
                                                                     (intersect a b defs)))]
                                              (assoc left :arglist new-arglist))
                                [r/OR r/AND] :swap
                                [r/OR :idclol] (update left :arglist (partial map #(intersect % right defs)))

                                [r/AND r/AND] (reduce #(intersect %1 %2 defs) (r/->AnySpec) (concat (:arglist left) (:arglist right)))
                                [r/AND r/OR] (let [new-left (reduce #(intersect %1 %2 defs) (r/->AnySpec) (:arglist left))
                                                   new-arglist (set (for [a (:arglist right)]
                                                                      (intersect new-left a defs)))]
                                               (assoc right :arglist new-arglist))
                                [r/AND :idclol] (reduce #(intersect %1 %2 defs) right (:arglist left))

                                [r/PLACEHOLDER r/ANY] left
                                [r/PLACEHOLDER :idclol] (assoc left :alias right)

                                [r/UNION :idclol] right
                                [r/COMPATIBLE :idclol] right

                                [r/ERROR :idclol] left


                                nil)
          to-error-spec (fn [i] (if (or (nil? i) (= :swap i)) (r/DISJOINT left right) i))]
      (if (and first-time (= :swap intersection))
        (recur right left false)
        (simplify (to-error-spec intersection) defs)))))



(defn- list-term? [term]
  (and (= r/COMPOUND (r/term-type term))
       (= "." (.functor term))
       (= 2 (count (.arglist term)))))

(defn- get-head-and-tail [term]
  {:head (first (.arglist term)) :tail (second (.arglist term))})

(defn next-steps [term spec defs]
  (case+ [(r/spec-type spec)]
         (r/NONVAR, r/GROUND, r/ANY) (if (contains? #{r/LIST r/COMPOUND} (r/term-type term))
                             [term (intersect spec (r/initial-spec term) defs)]
                             [])
         r/LIST (cond
                  (list-term? term) (let [{head :head tail :tail} (get-head-and-tail term)]
                                      [head type tail spec])
                  (= r/LIST (r/term-type term)) (if (or (r/empty-list? (.tail term)) (= r/VAR (r/term-type (.tail term)))) [(.head term) type] [(.head term) type (.tail term) spec])
                  :else [])
         r/TUPLE (cond
                   (list-term? term) (let [{head :head tail :tail} (get-head-and-tail term)]
                                       [head (first (:arglist spec))
                                        tail (update spec :arglist rest)])
                   (= r/LIST (r/term-type term)) (if (empty? (:arglist spec)) [(.head term) (r/->ErrorSpec "Empty Tuple")] [(.head term) (first (:arglist spec)) (.tail term) (update spec :arglist rest)])
                   :else [])
         r/COMPOUND (let [{functor :functor arglist :arglist} spec]
                      (cond
                        (= r/COMPOUND (r/term-type term)) (if (= (count arglist) (count (:arglist term)))
                                                            (interleave (.arglist term) arglist)
                                                            [])
                        (and (= "." functor) (= 2 (count arglist)) (= r/LIST (r/term-type term))) [(.head term) (first arglist) (.tail term) (second arglist)]
                        (and (= "." functor) (= 2 (count arglist)) (list-term? term)) [(first (.arglist term)) (first arglist) (second (.arglist term)) (second arglist)]
                        :else []
                        ))
         r/AND (interleave (repeat term) (:arglist spec))
         r/OR (if-let [{arglist :arglist :as suitable-spec} (intersect spec (r/initial-spec term) defs)]
                (if (= r/OR (r/spec-type suitable-spec))
                  (->> arglist
                       (map #(next-steps term % defs))
                       (map (partial apply hash-map))
                       (map (partial reduce-kv (fn [m k v] (update m k #(set (conj % v)))) {}))
                       (apply merge-with into)
                       (reduce-kv (fn [m k v] (conj m k (simplify (r/->OneOfSpec (apply vector v))))) []))
                  [term suitable-spec])
                [])
         []))
