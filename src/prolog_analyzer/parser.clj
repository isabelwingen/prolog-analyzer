(ns prolog-analyzer.parser
  (:require [instaparse.core :as insta]
            [clojure.pprint :refer [pprint]]))


(def prolog-parser
  (insta/parser
   "<S> = ((Rule <OptionalWs>) | (Fact <OptionalWs>) | <Single-Line-Comment> | <Multi-Line-Comment>)*
    Rule = Head <OptionalWs> <':-'> <OptionalWs> Body <Period>
    Fact = Name Arglist? <Period>
    Compound = Name Arglist
    Goal = Name Arglist? | Cut | True | False | Fail | Not | If
    <Goals> = Goal (<Komma> Goal)*
    Head = Goal
    Body = Expr
    <Expr> = Or
    Or = Or <Semicolon> Or* | <'('> Or <')'> | And
    And = And <Komma> And | <OpenBracket> And <CloseBracket> | <OpenBracket> Or <CloseBracket> <Komma> And | And <Komma> <OpenBracket> Or <CloseBracket> | Goal
    <Arglist> = <OpenBracket> Args <CloseBracket>
    Args = Arg (<Komma> Arg)*
    <Arg> = Var | Atom | Number | Arg SpecialChar Arg | Compound
    Cut = <'!'>
    True = <'true'>
    False = <'false'>
    Fail = <'fail'>
    Repeat = <'repeat'>
    Not = <'not('> Goal <')'>
    If = Body <OptionalWs> <'->'> <OptionalWs> Body <Semicolon> Body | <'('> If <')'>
    SpecialChar = '-' | '/'
    Var = #'[A-Z][a-zA-Z0-9_]*'
    Name = #'[a-z][a-zA-Z0-9_]*'
    Atom = #'[a-z][a-zA-Z0-9_]*'
    Number = #'[0-9]*'
    Komma = <OptionalWs> <','> <OptionalWs>
    Semicolon = <OptionalWs> <';'> <OptionalWs>
    Period = <OptionalWs> <'.'> <OptionalWs>
    OpenBracket = <'('>
    CloseBracket = <')'>
    <Ws> = #'\\s+'
    <OptionalWs> = #'\\s*'
    Single-Line-Comment = #'%.*\n'
    Multi-Line-Comment = '/*' #'.*' '*/'
"
   :output-format :hiccup))

(def test-parser2
  (insta/parser
   "<S> = Or
  Or = Or <';'> Or | <'('> Or <')'> | And
  And = And <','> And | A | <'('> And <')'> | <'('> Or <')'> <','> And | And <','> <'('> Or <')'>
    A = #'^\\s' | <'('> A <')'>"
   :output-format :enlive))
