(ns prolog-analyzer.parser
  (:require [instaparse.core :as insta]
            [clojure.pprint :refer [pprint]]
            [clojure.tools.logging :as log]))


(def prolog-parser

  (insta/parser
   "<S> = <OptionalWs> (Rule | Fact | DirectCall | <Comment> | <OptionalWs>) ((<Ws> | <Comment>) S)?
    <Comment> = <Single-Line-Comment> | <Multi-Line-Comment>

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
    <OptionalWs> = (#'\\s*' <Comment>*)*
    Single-Line-Comment = #'%.*\n'
    Multi-Line-Comment = '/*' #'.*' '*/'
"
  :output-format :hiccup))

(defn parse [string]
  (let [result (insta/parse prolog-parser (str string "\n"))]
    (if (insta/failure? result)
      (do
        (log/error result)
        result)
      result)
    ))

