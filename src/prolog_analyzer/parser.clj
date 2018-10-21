(ns prolog-analyzer.parser
  (:require [instaparse.core :as insta]
            [clojure.pprint :refer [pprint]]
            [clojure.tools.logging :as log]))


(def prolog-parser
  (insta/parser
   "<S> = <OptionalWs> (Rule <Single-Line-Comment>?| Fact | DirectCall | <Single-Line-Comment> | <Multi-Line-Comment> | <OptionalWs>) ((<Ws> | <Single-Line-Comment> ) S)?

    Rule = Name Arglist? StartOfBody Goals <Period>
    Fact = Name Arglist? <Period>
    DirectCall = <StartOfBody> Goals <Period>

    <Goals> = Goal | Goals (Komma | Semicolon) Goals | InBrackets
    Goal = If | Name Arglist? | Cut | True | False | Fail | Not | Assignment
    InBrackets = <OpenBracket> Goals <CloseBracket>

    <Assignment> = IsAssignment | UnifyAssignment

    UnifyAssignment = Term <OptionalWs> <'='> <OptionalWs> Term

    IsAssignment = Term <OptionalWs> <'is'> <OptionalWs> Expr
    Expr = Term | Expr Op Expr | <OpenBracket> Expr <CloseBracket>
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


(defmulti post-processing insta/failure?)

(defmethod post-processing true [failure]
  failure)

(defmethod post-processing false [result]
  result)

(defn parse [string]
  (insta/parse prolog-parser (str string "\n")))

(defn process-source [file]
  (-> file
      slurp
      parse
      post-processing))

