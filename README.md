# prolog-analyzer

An static analyzing tool written in Clojure and SWI Prolog. 
The tool uses specs for predicates (similar to clojure.specs) to find errors statically.
At the moment it does not do much. See below for planned features.


## Installation

Download from https://github.com/Winis04/prolog-analyzer

## Usage

SWI Prolog is used to parse the prolog source code and transform it to [edn](https://github.com/edn-format/edn).

To enable the parsing process, place the `term_expander.pl` in the directory of your source file and add the following lines to it:

```
:- use_module(term_expander,[enable_write_out,declare_spec/1,deffne_spec/2,spec_pre/2,spec_post/3,spec_invariant/2]).
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
* atom(X), where X is an atom
    * describes atom which are exact the specified X
* atomic
    * parent of atom and number
    * Attention: we choose to handle the empty list not as atomic, but as a list
* ground
* nonvar
* var
* compound
    * example: compound(foo(int,atom)) - a compound with functor foo, first element an integer, second element an atom
* list 
    * example: list(int): list of integers
* tuple
    * example: tuple([int,float]) - a list, where the first position is an int, and the second position is a float
* any
    * all of the above
* any(X)
    * could be anything, but if you refer to any(X) somewhere else in the spec defition, these will be the same
* one_of
    * one_of([int,atom]) - int or atom
* and
    * and([ground,compound]) - ground and compound
    
### Define your own specs
You can define your own specs right there in your prolog file. First, declare it by calling declare/1, then define it by setting an alias for it.
For example, lets say, that we want to define a spec for binary trees called *tree*.
```
:- declare(tree).
```
It is important to declare the spec *before* you define it, so prolog-analyzer can check, if your defitions (even recursive ones) are correct.
Then define it:
```
:- define(tree(any(X)),one_of([compound(node(tree(any(X)),any(X),tree(any(X)))),atom(empty)])).
```
So, a tree is either a compound with three components, where the first and the last one are trees, and the element in the middle is a value, or the atom `empty`.

### Annotate your code
* `:- spec_pre(foo/2,[int,atom]).` - Pre conditions, which must be fullfilled when calling predicate `foo`.
* `:- spec_post(bar/1,[var],[int]).` - If the first condition is valid when entering the predicate, the second must be valid, when the predicate is done
* `:- spec_inv(ahh/1,[ground]).` - This condition must be valid during the whole execution

## Planned Features

- Use Specs for static analysis
- Derive Specs for unspec'd predicates

## License

Copyright © 2018 Isabel Wingen

Distributed under the MIT License
