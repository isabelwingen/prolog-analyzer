# prolog-analyzer

A static analyzing tool written in Clojure and SWI Prolog. 
The tool uses specs for predicates (similar to clojure.specs) to find errors statically.
At the moment the feature range is limited and under developtment, but some simple errors can already be spotted.


## Installation

Download from https://github.com/isabelwingen/prolog-analyzer

## Usage

SWI Prolog is used to parse the prolog source code and transform it to [edn](https://github.com/edn-format/edn).

To enable the parsing process, place the `prolog_analyzer.pl` in the directory of your source file and add the following lines to it:

```
:- use_module(term_expander,[enable_write_out,declare_spec/1,define_spec/2,spec_pre/2,spec_post/3,spec_invariant/2]).
:- enable_write_out.
```

Make sure that you have SWI Prolog installed.

Create an uberjar with `lein uberjar` and execute
```
java -jar targer/uberjar/prolog-analyzer-X.Y.Z-standalone.jar <path-to-your-file>

```

### Built-in Specs
A couple of specs are already defined:
* integer/int
    * both will be handled as integer
* float
* number
    * parent of ints and floats
    * at the moment does not support other number types
* atom
* exact(X), where `X` is an atom
    * describes atom which are exact the specified `X`
    * e.g `exact(empty)` describes the atom `empty`
* atomic
    * parent of atom and number
* ground
* nonvar
* var
* compound
    * example: compound(foo(int,atom)) - a compound with functor foo, first element an integer, second element an atom
* list 
    * example: `list(int)` - list of integers
* tuple
    * example: `tuple([int,float])` - a list, where the first position is an int, and the second position is a float
* any
    * all of the above
* specvar(X)
    * A variable for specs. Can be referred to under its name `X`
    * In a scope, all references to `specvar(X)` describe the same spec.
    * e.g. `define_spec(foo(specvar(X)), one_of([specvar(X), compound(foo(specvar(X)))]))`
* one_of
    * e.g. `one_of([int,atom])` - int or atom
* and
    * e.g. `and([ground,compound])` - ground and compound
    
### Define your own specs
You can define your own specs right there in your prolog file. First, declare it by calling declare/1, then define it by setting an alias for it.
For example, lets say, that we want to define a spec for binary trees called *tree*.
```
:- declare(tree).
```
It is important to declare the spec *before* you define it, so prolog-analyzer can check, if your defitions (even recursive ones) are correct.
Then define it:
```
:- define(tree(specvar(X)),one_of([compound(node(tree(specvar(X)),specvar(X),tree(specvar(X)))),exact(empty)])).
```
So, a tree is either a compound with three components, where the first and the last one are trees, and the element in the middle is a value, or the atom `empty`.

### Annotate your code
* `:- spec_pre(foo/2,[int,atom]).` - Pre conditions, which must be fullfilled when calling predicate `foo`.
* `:- spec_post(bar/1,[var],[int]).` - If the first condition is valid when entering the predicate, the second must be valid, when the predicate is done
* `:- spec_inv(ahh/1,[ground]).` - This condition must be valid during the whole execution

## Goal
Using spec annoations, we have a good describtion on how a term has to *look* regarding its structure. With this abstract describtions, we can find errors (wrong argument order, wrong return values, ...) without executing the prolog code itself. 

Instead we use abstact interpretation, which means, that we go through the code and collect every information we can about the terms *without* executing the code, just using the provided specs.

This knowledge collection is basically a list for every term, which holds the specs that are assigned to this term by a goal, which is called, or a relationship to other term.

If we find a spec which is not compatible with the term, or if some of the found specs are disjoint, we have found an error in the code (or the annotations ;-))

## Current Features
Every clause is analyzed in combination with a pre spec of the predicate it belongs to. 

For every term used in the clause, we collect the **pre-specs** that can be derived of the goals the term is used in. This collection is called a *domain*.
If a term collides with a expected spec somewhere, we have found an error, which is logged and saved as an error spec in the collection.
Furthermore we add relationship between terms, like *This Term is the head of that List*. This relationship are not used for further analysis at the moment.

The found collection is printed nicely, and some simple error can already be spotted.


## Planned Features

### Further Analysis
#### Relations from Goals
Some goals create relationships between terms and the domains of these term. An example is `A == B`. Every domain of `A` must be a domain of `B` and vice versa.

#### Usage of Post Specs
Post Specs contain more information than pre specs. If a condition of post spec is fullfilled when a goal is called, we *know* the domain of the spec afterwards. Keep in mind that only non-ground terms can change their type at all.

#### Usage of Relations
The relationships between terms contain valuable information. If a term is the head of the list, which has the domain `[list(int)]`, we know, that the head cannot be an `atom`. By going through the relations and updating the domains, we can find errors currently hidden.

#### Not that spec
Sometimes it could be necessary to say `I don't know what it is, but it is not that`. We do not have the option to save this kind of information.


## License

Copyright © 2018 Isabel Wingen

Distributed under the MIT License
