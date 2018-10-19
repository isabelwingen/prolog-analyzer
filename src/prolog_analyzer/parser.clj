(ns prolog-analyzer.parser
  (:require [instaparse.core :as insta]
            [clojure.pprint :refer [pprint]]))


(def prolog-parser
  (insta/parser
   "<S> = ((Rule <OptionalWs>) | (Fact <OptionalWs>) | (DirectCall <OptionalWs>) | <Single-Line-Comment> | <Multi-Line-Comment>)*
    Rule = Goal StartOfBody Goals <Period>
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
    List = EmptyList | <'['> <OptionalWs> Terms <OptionalWs> <']'> | <'['> Terms <'|'> List  <']'>
    EmptyList = <'['> <']'>
    Cut = <'!'>
    True = <'true'>
    False = <'false'>
    Fail = <'fail'>
    Repeat = <'repeat'>
    Not = <'not('> Goal <')'>
    If = <OpenBracket> AndListOfGoals Then AndListOfGoals Else AndListOfGoals <CloseBracket>
    <AndListOfGoals> = Goal | AndListOfGoals Komma AndListOfGoals | InBrackets
    SpecialChar = '-' | '/'

    Var = #'[A-Z][a-zA-Z0-9_]*'
    Functor = #'[a-z][a-zA-Z0-9_]*'
    Name = #'[a-z][a-zA-Z0-9_]*'
    Atom = #'[a-z][a-zA-Z0-9_]*'
    Number = #'[0-9]+'

    Komma = <OptionalWs> <','> <OptionalWs>
    Semicolon = <OptionalWs> <';'> <OptionalWs>
    Period = <OptionalWs> <'.'> <OptionalWs>
    OpenBracket = <'('>
    CloseBracket = <')'>
    StartOfBody = <OptionalWs> <':-'> <OptionalWs>
    Then = <OptionalWs> <'->'> <OptionalWs>
    Else = <Semicolon>

    <OptionalWs> = #'\\s*'

    Single-Line-Comment = #'%.*\n'
    Multi-Line-Comment = '/*' #'.*' '*/'
"
   :output-format :hiccup))

(defn parse [str]
  (insta/parse prolog-parser str))

(def test-parser2
  (insta/parser
   "<S> = Or
  Or = Or <';'> Or | <'('> Or <')'> | And
  And = And <','> And | A | <'('> And <')'> | <'('> Or <')'> <','> And | And <','> <'('> Or <')'>
    A = #'^\\s' | <'('> A <')'>"
   :output-format :enlive))
