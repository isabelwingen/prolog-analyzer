(ns prolog-analyzer.parser
  (:require [instaparse.core :as insta]
            [clojure.pprint :refer [pprint]]))


(def prolog-parser
  (insta/parser
   "<S> = ((Rule <OptionalWs>) | (Fact <OptionalWs>) | (DirectCall <OptionalWs>) | <Single-Line-Comment> | <Multi-Line-Comment>)*
    Rule = Goal StartOfBody Goals <Period>
    Fact = Name Arglist? <Period>
    DirectCall = <StartOfBody> Goals <Period>
    Compound = Name Arglist
    Goal = If | Name Arglist? | Cut | True | False | Fail | Not
    <Goals> = Goal | Goals (Komma | Semicolon) Goals | InBrackets
    InBrackets = <OpenBracket> Goals <CloseBracket>
    <Arglist> = <OpenBracket> Args <CloseBracket>
    Args = Arg (Komma Arg)*
    <Arg> = Var | Atom | Number | Arg SpecialChar Arg | Compound
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
    Name = #'[a-z][a-zA-Z0-9_]*'
    Atom = #'[a-z][a-zA-Z0-9_]*'
    Number = #'[0-9]*'
    Komma = <OptionalWs> <','> <OptionalWs>
    Semicolon = <OptionalWs> <';'> <OptionalWs>
    Period = <OptionalWs> <'.'> <OptionalWs>
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

(defn parse
  (insta/parse prolog-parser str))

(def test-parser2
  (insta/parser
   "<S> = Or
  Or = Or <';'> Or | <'('> Or <')'> | And
  And = And <','> And | A | <'('> And <')'> | <'('> Or <')'> <','> And | And <','> <'('> Or <')'>
    A = #'^\\s' | <'('> A <')'>"
   :output-format :enlive))
