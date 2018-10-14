(ns prolog-analyzer.parser
  (:require [instaparse.core :as insta]
            [clojure.pprint :refer [pprint]]))


(def prolog-parser
  (insta/parser
   "<S> = ((Rule <Ws>) | (Fact <Ws>) | (DirectCall <Ws>) | <Single-Line-Comment> | <Multi-Line-Comment>)*
    Rule = Head <Ws> <':-'> <Ws> Body <'.'>
    Fact = Name Arglist? <'.'>
    Single-Line-Comment = <'%'> #'.*\n'
    Multi-Line-Comment = <'/*'> #'.*' <'*/'>
    DirectCall = <':-'> Fact <'.'>
    Goal = Name Arglist?
    Head = Name Arglist?
    <BodyElement> = Goal | <'!'> | ControlPredicate
    <ControlPredicate> = '!' | 'false' | 'true' | 'repeat' | Not | Or | If
    Not = <'not('> Goal <')'>
    Or = Body <Ws>* <';'> <Ws>* Body (<Ws>* <';'> <Ws>* Body)*
    If = 'if'
    Body = BodyElement (<','> <Ws> BodyElement)*
    <Arglist> = <'('> Args <')'>
    Args = Arg (<','> Arg)*
    <Arg> = Var | Atom | Number
    Var = #'[A-Z][a-zA-Z0-9_]*'
    Name = #'[a-z][a-zA-Z0-9_]*'
    Atom = #'[a-z][a-zA-Z0-9_]*'
    Number = #'[0-9]*'
    <Ws> = #'\\s+'
"
   :output-format :hiccup))

(prolog-analyzer.parser/prolog-parser (slurp "resources/parse-example.pl"))
