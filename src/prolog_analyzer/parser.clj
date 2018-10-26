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
    DirectCall =  <StartOfBody> (Dynamic | Metapredicate | Goals) <Period>
    Dynamic = <'dynamic '> <OptionalWs> Declaration (<Komma> Declaration)*
    Declaration = Name <'/'> Arity
    Metapredicate = <'meta_predicate '> <OptionalWs> Metagoal (<Komma> Metagoal)*
    Metagoal = Module? Name Arglist?
    Arity = #'[0-9]+'
    <Comment> = <Single-Line-Comment> <OptionalWs> | <Multi-Line-Comment> <OptionalWs>

    <Goals> = Goal <Comment>* | Goals (Komma | Semicolon | IfArrow) <OptionalWs> Goals | InBrackets
    Goal = If | Module? Name Arglist? | Cut | True | False | Fail | Not | Assignment | BoolExpr
    Module = Name <':'>
    InBrackets = <OpenBracket> Goals <CloseBracket>

    <Assignment> = IsAssignment | UnifyAssignment

    UnifyAssignment = Term <OptionalWs> <'='> <OptionalWs> Term
    IsAssignment = Term <OptionalWs> <'is'> <OptionalWs> Expr
    <Expr> = Var | Number | Expr <Op> Expr | <OpenBracket> Expr <CloseBracket>
    BoolExpr =  Expr <BoolOp> Expr | <OpenBracket> BoolExpr <CloseBracket>
    BoolOp = <OptionalWs> ('<' | '>' | '=') <OptionalWs>
    Op = <OptionalWs> ('+' | '-' | '*' | '**' | '^' | '/') <OptionalWs>

    Arglist = <OpenBracket> Terms <CloseBracket>
    <Terms> = (Term | Inner) (<Komma> (Term | Inner))*
    Inner = <OpenBracket> Terms <CloseBracket>
    <Term> = Number | NotNumber
    <NotNumber> = Var | Atom | Compound | List | String
    Compound = Functor Arglist | Term SpecialChar_Compound Term | SpecialChar_Compound NotNumber | '\\'' SpecialChar_Compound '\\'' Arglist?
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

    Var = #'[A-Z_][a-zA-Z0-9_]*'
    String = <'\\''> #'[^\\']*' <'\\''> | <'\\\"'> #'[^\\\"]*' <'\\\"'>
    Functor = #'[a-z][a-zA-Z0-9_]*'
    Name = #'[a-z][a-zA-Z0-9_]*'
    Atom = #'[a-z][a-zA-Z0-9_]*' | SpecialChar_Atom+
    <Number> = Integer | Float
    Integer = #'[\\-\\+]?\\s*[0-9]+'
    Float = #'[\\-\\+]?\\s*[0-9]+\\.[0-9]+'
    <SpecialChar_Atom> = '$' | '&' | '*' | '+' | '-' | '.' | '/' | ':' | '<' | '>' | '=' | '?' | '@' | '^' | '~'
    SpecialChar_Compound = #'[\\+\\-\\/:]+'

    Komma = <OptionalWs> <','> <OptionalWs>
    Semicolon = <OptionalWs> <';'> <OptionalWs>
    Period = <OptionalWs> <'.'> <Comment>*
    OpenBracket = <OptionalWs> <'('> <OptionalWs>
    CloseBracket = <OptionalWs> <')'> <OptionalWs>
    StartOfBody = <OptionalWs> <':-'> <OptionalWs>
    IfArrow = <OptionalWs> <'->'> <OptionalWs>
    Then = <OptionalWs> <'->'> <OptionalWs>
    Else = <Semicolon>
    <Ws> = #'\\s+'
    <OptionalWs> = (#'\\s*' <Comment>*)*
    Single-Line-Comment = #'%.*\n'
    Multi-Line-Comment = #'/\\*+[^*]*\\*+(?:[^/*][^*]*\\*+)*/'
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

