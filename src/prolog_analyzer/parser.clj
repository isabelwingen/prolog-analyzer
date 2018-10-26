(ns prolog-analyzer.parser
  (:require [instaparse.core :as insta]
            [clojure.pprint :refer [pprint]]
            [clojure.tools.logging :as log]))


(def prolog-parser
<<<<<<< HEAD
   (insta/parser
    "<S> = <OptionalWs> (Rule <Comment>? | Fact | DirectCall | <Comment> | <OptionalWs>) ((<Ws> | <Single-Line-Comment> ) S)?
=======
  (insta/parser
   "<S> = <OptionalWs> (Rule | Fact | DirectCall | <Comment> | <OptionalWs>) ((<Ws> | <Comment>) S)?
    <Comment> = <Single-Line-Comment> | <Multi-Line-Comment>
>>>>>>> e49a39a8a49fb2dc3dcdc4ee516d4812db49d6c9

    Rule = Name Arglist? <StartOfBody> Goals <Period>
    Fact = Name Arglist? <Period>
    DirectCall = <StartOfBody> Goals <Period>
    <Comment> = <Single-Line-Comment> <OptionalWs> | <Multi-Line-Comment> <OptionalWs>

    <Goals> = Goal <Comment>* | Goals (Komma | Semicolon) <Comment>* Goals | InBrackets
    Goal = If | Module? Name Arglist? | Cut | True | False | Fail | Not | Assignment
    Module = Name <':'>
    InBrackets = <OpenBracket> Goals <CloseBracket>

    <Assignment> = IsAssignment | UnifyAssignment

    UnifyAssignment = Term <OptionalWs> <'='> <OptionalWs> Term
    IsAssignment = Term <OptionalWs> <'is'> <OptionalWs> Expr
    <Expr> = Var | Number | Expr <Op> Expr | <OpenBracket> Expr <CloseBracket>
    Op = <OptionalWs> ('+' | '-' | '*' | '**' | '^') <OptionalWs>

    Arglist = <OpenBracket> Terms <CloseBracket>
    <Terms> = Term <Comment>* (<Komma> <Comment>* Term)* <Comment>*
    <Term> = Var | Atom | Number | Compound | List | String
    Compound = Functor Arglist | Term SpecialChar Term
    List = EmptyList | ExplicitList  | HeadTailList
    EmptyList = <'['> <']'>
    HeadTailList = <'['> <OptionalWs> Terms <OptionalWs> <'|'> <OptionalWs> (List | Var) <OptionalWs> <']'>
    ExplicitList = <'['> <OptionalWs> Terms <OptionalWs> <']'>
    Cut = <'!'>
    True = <'true'>
    False = <'false'>
    Fail = <'fail'>
    Repeat = <'repeat'>
    Not = <'not('> <OptionalWs> Goal <OptionalWs> <')'> | <'\\\\+'> <OptionalWs> Goal
    If = <OpenBracket> AndListOfGoals Then AndListOfGoals Else AndListOfGoals <CloseBracket>
    <AndListOfGoals> = Goal | AndListOfGoals Komma AndListOfGoals | InBrackets
    SpecialChar = #'[^A-Za-z0-9_,;\\(\\)<>=]'

    Var = #'[A-Z_][a-zA-Z0-9_]*'
    String = <'\\''> #'[^\\']*' <'\\''>
    Functor = #'[a-z][a-zA-Z0-9_]*'
    Name = #'[a-z][a-zA-Z0-9_]*'
    Atom = #'[a-z][a-zA-Z0-9_]*'
    Number = #'[0-9]+'

    Komma = <OptionalWs> <','> <OptionalWs>
    Semicolon = <OptionalWs> <';'> <OptionalWs>
    Period = <OptionalWs> <'.'> <Comment>*
    OpenBracket = <OptionalWs> <'('> <OptionalWs>
    CloseBracket = <OptionalWs> <')'> <OptionalWs>
    StartOfBody = <OptionalWs> <':-'> <OptionalWs>
    Then = <OptionalWs> <'->'> <OptionalWs>
    Else = <Semicolon>
    <Ws> = #'\\s+'
<<<<<<< HEAD
    <OptionalWs> = #'\\s*'
    Single-Line-Comment = <OptionalWs> #'%.*\n'
    Multi-Line-Comment = <OptionalWs> #'/\\*+[^*]*\\*+(?:[^/*][^*]*\\*+)*/'
=======
    <OptionalWs> = (#'\\s*' <Comment>*)*
    Single-Line-Comment = #'%.*\n'
    Multi-Line-Comment = '/*' #'.*' '*/'
>>>>>>> e49a39a8a49fb2dc3dcdc4ee516d4812db49d6c9
"
  :output-format :hiccup))
(prolog-parser
"foo([a,
      b,
      c]).")

<<<<<<< HEAD
(defn- has-args? [tree]
  (= :Arglist (get-in tree [2 0])))

(defn- get-args [tree]
  (if (has-args? tree)
    (rest (get tree 2))
    []))

(defn- add-arglist [tree pos]
  (if (has-args? tree)
    tree
    (let [[before after] (split-at pos tree)]
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
  (let [[_ [_ functor] [_ & arglist] & body] (add-arglist tree 2)]
    {functor {:arity (count arglist)
              :arglist (vec (map transform-to-map arglist))
              :body (vec (transform-body (map transform-to-map body)))}}))

(defmethod transform-to-map :Fact [tree]
  (let [[_ [_ functor] [_ & arglist]] (add-arglist tree 2)]
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
(defmethod transform-to-map :String [[_ functor]]
    {:term functor
     :type :string})
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
  (case (get-in tree [1 0])
    :Name (let [[_ [_ functor] [_ & arglist]] (add-arglist tree 2)]
            {:goal functor
             :arity (count arglist)
             :arglist (vec (map transform-to-map arglist))
             :module :user})
    :Module (let [[_ [_ [_ module]] [_ functor] [_ & arglist]] (add-arglist tree 3)]
              {:goal functor
               :arity (count arglist)
               :arglist (vec (map transform-to-map arglist))
               :module module})
    (let [[_ [& x]] tree]
      (transform-to-map x))))

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
     :else else}
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

(defn- error-handling [error-object]
  (log/error error-object)
  error-object
  )

(defn- post-processing [list-of-preds]
  (if (insta/failure? list-of-preds)
    (error-handling list-of-preds)
    (->> list-of-preds
         (map transform-to-map)
         (map first)
         (map (fn [[k v]] {k [v]}))
         (apply (partial merge-with into)))))

;;(apply (partial merge-with into) (map (comp (fn [[k v]] {k [v]}) first) (map transform-to-map list-of-preds)))
=======
>>>>>>> e49a39a8a49fb2dc3dcdc4ee516d4812db49d6c9

(defn parse [string]
  (let [result (insta/parse prolog-parser (str string "\n"))]
    (if (insta/failure? result)
      (do
        (log/error result)
        result)
      result)
    ))

<<<<<<< HEAD
(defn process-source [file]
  (-> file
      slurp
      process-string))


(prolog-parser "+" :start :SpecialChar)
=======
(parse ":- module(a, [arg1,
                      % comment
                      arg2]).")
>>>>>>> e49a39a8a49fb2dc3dcdc4ee516d4812db49d6c9
