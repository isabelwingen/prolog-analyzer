(ns prolog-analyzer.parser
  (:require [instaparse.core :as insta]
            [clojure.pprint :refer [pprint]]
            [clojure.tools.logging :as log]))


(def prolog-parser
  (insta/parser
   "<S> = <OptionalWs> (Rule <Single-Line-Comment>?| Fact | DirectCall | <Single-Line-Comment> | <Multi-Line-Comment> | <OptionalWs>) ((<Ws> | <Single-Line-Comment> ) S)?

    Rule = Name Arglist? <StartOfBody> Goals <Period>
    Fact = Name Arglist? <Period>
    DirectCall = <StartOfBody> Goals <Period>

    <Goals> = Goal | Goals (Komma | Semicolon) Goals | InBrackets
    Goal = If | Name Arglist? | Cut | True | False | Fail | Not | Assignment
    InBrackets = <OpenBracket> Goals <CloseBracket>

    <Assignment> = IsAssignment | UnifyAssignment

    UnifyAssignment = Term <OptionalWs> <'='> <OptionalWs> Term
    IsAssignment = Term <OptionalWs> <'is'> <OptionalWs> Expr
    <Expr> = Var | Number | Expr <Op> Expr | <OpenBracket> Expr <CloseBracket>
    Op = <OptionalWs> ('+' | '-' | '*' | '**' | '^') <OptionalWs>

    Arglist = <OpenBracket> Terms <CloseBracket>
    <Terms> = Term (<Komma> Term)*
    <Term> = Var | Atom | Number | Compound | List
    Compound = Functor Arglist | Term SpecialChar Term
    List = EmptyList | ExplizitList  | HeadTailList
    EmptyList = <'['> <']'>
    HeadTailList = <'['> Terms <'|'> (List | Var) <']'>
    ExplizitList = <'['> <OptionalWs> Terms <OptionalWs> <']'>
    Cut = <'!'>
    True = <'true'>
    False = <'false'>
    Fail = <'fail'>
    Repeat = <'repeat'>
    Not = <'not('> Goal <')'>
    If = <OpenBracket> AndListOfGoals Then AndListOfGoals Else AndListOfGoals <CloseBracket>
    <AndListOfGoals> = Goal | AndListOfGoals Komma AndListOfGoals | InBrackets
    SpecialChar = '-' | '/'

    Var = #'[A-Z_][a-zA-Z0-9_]*'
    Functor = #'[a-z][a-zA-Z0-9_]*'
    Name = #'[a-z][a-zA-Z0-9_]*'
    Atom = #'[a-z][a-zA-Z0-9_]*'
    Number = #'[0-9]+'

    Komma = <OptionalWs> <','> <OptionalWs>
    Semicolon = <OptionalWs> <';'> <OptionalWs>
    Period = <OptionalWs> <'.'>
    OpenBracket = <'('>
    CloseBracket = <')'>
    StartOfBody = <OptionalWs> <':-'> <OptionalWs>
    Then = <OptionalWs> <'->'> <OptionalWs>
    Else = <Semicolon>
    <Ws> = #'\\s+'
    <OptionalWs> = #'\\s*'
    Single-Line-Comment = #'%.*\n'
    Multi-Line-Comment = '/*' #'.*' '*/'
"
   :output-format :hiccup))


(defn error-handling [result]
  (if (insta/failure? result)
    '()
    result))

(defn has-args? [tree]
  (= :Arglist (get-in tree [2 0])))

(defn get-args [tree]
  (if (has-args? tree)
    (rest (get tree 2))
    []))

(defn add-arglist [tree]
  (if (has-args? tree)
    tree
    (let [[before after] (split-at 2 tree)]
      (vec (concat before [[:Arglist]] after)))))

(defmulti transform-to-map first)

(defmethod transform-to-map :Rule [tree]
  (let [[_ [_ functor] [_ & arglist] & body] (add-arglist tree)]
    {functor {:arity (count arglist)
              :arglist (vec (map transform-to-map arglist))
              :body (vec (map transform-to-map body))}}))

(defmethod transform-to-map :Fact [tree]
  (let [[_ [_ functor] [_ & arglist]] (add-arglist tree)]
    {functor {:arity (count arglist)
              :arglist (vec (map transform-to-map arglist))
              :body []}}))



(defmethod transform-to-map :Atom [[_ functor]]
  {:type :atom
   :name functor
   :dom :atom})

(defmethod transform-to-map :Var [[_ functor]]
  {:type :var
   :name functor
   :dom :var})

(defmethod transform-to-map :Goal [tree]
  (if (= :Name (get-in tree [1 0]))
    (let [[_ [_ functor] [_ & arglist]] (add-arglist tree)]
      {:goal functor
       :arity (count arglist)
       :arglist (vec (map transform-to-map arglist))
       :type :normal})
    (let [[_ [& x]] tree]
      (transform-to-map x))
    ))

(defmethod transform-to-map :IsAssignment [tree]
  (let [[_ left & right] tree
        transformed-left (transform-to-map left)
        transformed-right (map transform-to-map right)]
    (if (= 1 (count right))
      {:goal :unify-assignment
       :arity 2
       :arglist [transformed-left (first transformed-right)]}
      {:goal :is-assignment
       :arity (inc (count transformed-right))
       :arglist (concat [transformed-left] transformed-right)})))

(-> "foo :- X is A+B+2." parse post-processing)
(defmethod transform-to-map :default [tree]
  (when (not (nil? tree)) 
    (log/warn (str "when transforming the parse-tree for " (first tree) " no matching method was found!")))
  tree)

(defn post-processing [list-of-preds]
  (map transform-to-map list-of-preds))

(defn parse [string]
  (insta/parse prolog-parser (str string "\n")))

(defn process-source [file]
  (-> file
      slurp
      parse
      error-handling
      post-processing))

(process-source "resources/test.pl")
(-> "foo :- X is A+2." parse post-processing)
(first nil)
