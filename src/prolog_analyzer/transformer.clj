(ns prolog-analyzer.transformer
  (:require [clojure.tools.logging :as log]
            [prolog-analyzer.parser :as parser]))


(defn- has-args? [tree]
  (= :Arglist (get-in tree [2 0])))

(defn- get-args [tree]
  (if (has-args? tree)
    (rest (get tree 2))
    []))

(defn- add-arglist [tree]
  (if (has-args? tree)
    tree
    (let [[before after] (split-at 2 tree)]
      (vec (concat before [[:Arglist]] after)))))


(defn split-at-first-delimiter [goals]
  (split-with #(not (contains? % :delimiter)) goals))

(defn split-at-semicolons [goals]
  (let [[left right] (split-with #(not (= :semicolon (:delimiter %))) goals)
        right (rest right)]
    (if (empty? right)
      [left]
      (vec (concat [left] (split-at-semicolons right))))))

(defn transform-body [goals]
  (let [filtered (vec (remove #(= :komma (:delimiter %)) goals))
        and-parts (vec (split-at-semicolons filtered))]
    (if (> (count and-parts) 1)
      [{:goal :or
        :arity (count and-parts)
        :arglist (vec and-parts)}]
      filtered)))

(defmulti transform-to-map first)

(defmethod transform-to-map :Rule [tree]
  (let [[_ [_ functor] [_ & arglist] & body] (add-arglist tree)]
    {functor {:arity (count arglist)
              :arglist (vec (map transform-to-map arglist))
              :body (vec (transform-body (map transform-to-map body)))}}))

(defmethod transform-to-map :Fact [tree]
  (let [[_ [_ functor] [_ & arglist]] (add-arglist tree)]
    {functor {:arity (count arglist)
              :arglist (vec (map transform-to-map arglist))
              :body []}}))

(defmethod transform-to-map :DirectCall [[_ & body]]
  {:direct-call {:body (vec (transform-body (map transform-to-map body)))}})

(defmethod transform-to-map :Number [[_ value]]
  {:term value
   :type :number})
(defmethod transform-to-map :Atom [[_ functor]]
  {:term functor
   :type :atom})
(defmethod transform-to-map :Var [[_ functor]]
  (if (= "_" functor)
    {:term :anonymous
     :type :var}
    {:term functor
     :type :var}))
(defmethod transform-to-map :Compound [tree]
  (if (= :Functor (get-in tree [1 0]))
    (let [[_ [_ functor] [_ & arglist]] tree]
      {:term functor
       :type :compound
       :arity (count arglist)
       :arglist (vec (map transform-to-map arglist))
       :infix false})
    (let [[_ left [_ special-char] right] tree]
      {:term special-char
       :type :compound
       :arity 2
       :arglist [(transform-to-map left) (transform-to-map right)]
       :infix true})))

(defmethod transform-to-map :List [[_ l]]
  (transform-to-map l))


(defmethod transform-to-map :HeadTailList [[_ & tree]]
  (let [tail (transform-to-map (last tree))
        head (map transform-to-map (reverse (rest (reverse tree))))]
    {:term :list
     :type :list
     :head (vec head)
     :tail tail}))

(defmethod transform-to-map :ExplicitList [[_ & tree]]
  {:term :list
   :type :list
   :content (vec (map transform-to-map tree))})

(defmethod transform-to-map :EmptyList [_]
  {:term :list
   :type :list})

(defmethod transform-to-map :Goal [tree]
  (if (= :Name (get-in tree [1 0]))
    (let [[_ [_ functor] [_ & arglist]] (add-arglist tree)]
      {:goal functor
       :arity (count arglist)
       :arglist (vec (map transform-to-map arglist))
       :module :user})
    (let [[_ [& x]] tree]
      (transform-to-map x))
    ))

(defmethod transform-to-map :InBrackets [[_ & body]]
  {:goal :in-brackets
   :body (vec (transform-body (map transform-to-map body)))})

(defmethod transform-to-map :Not [[_ goal]]
  {:goal :not
   :body [(transform-to-map goal)]})

(defmethod transform-to-map :If [[_ & body]]
  (let [[cond remaining] (split-with (complement #{[:Then]}) body)
        [then else] (split-with (complement #{[:Else]}) remaining)
        cond (vec (transform-body (map transform-to-map cond)))
        then (vec (transform-body (map transform-to-map (rest then))))
        else (vec (transform-body (map transform-to-map (rest else))))]
    {:goal :if
     :cond cond
     :then then
     :else else
     :module :built-in}
    ))

(filter (complement #{[:Komma]}) [1 [:Komma] 2 3])
(defmethod transform-to-map :Komma [_]
  {:delimiter :komma})

(defmethod transform-to-map :Semicolon [_]
  {:delimiter :semicolon})

(defmethod transform-to-map :IsAssignment [[_ left & right]]
  {:goal :is-assignment
   :left (transform-to-map left)
   :right (map transform-to-map right)
   :module :built-in})

(defmethod transform-to-map :UnifyAssignment [[_ left right]]
  {:goal :unify-assignment
   :left (transform-to-map left)
   :right (transform-to-map right)
   :module :built-in})

(defmethod transform-to-map :Cut [_]
  {:goal :Cut
   :arity 0
   :arglist []
   :module :built-in})

(defmethod transform-to-map :default [tree]
  (when (not (nil? tree)) 
    (log/warn (str "when tranforming the parse-tree for " (first tree) " no matching method was found!")))
  tree)

(defn error-handling [error-object]
  (log/error error-object)
  error-object)

(defn transform [string parse-fn]
    (->> (parse-fn string)
         (map transform-to-map)
         (map first)
         (map (fn [[k v]] {k [v]}))
         (apply (partial merge-with into))))

(transform "foo(X,a) :- (a,b -> b;c)." parser/parse)
