# prolog-analyzer

A static analyzing tool for Prolog written in Clojure and Prolog. 
The tool uses specs for predicates based on [plspec](https://github.com/wysiib/plspec) to find errors statically.


## Usage

* Download the provided ZIP
* Unzip it somewhere and open a terminal in the directory `prolog-analyzer`
* Execute
`java -jar prolog-analyzer-1.0.0-standalone.jar <dialect> "prolog/prolog_analyzer.pl" <path-to-your-code> <path-to-prolog-exe>` where 

    * `<dialect>` is "swipl" or "sicstus"
    * `<path-to-your-code>` is a directory containing your source code, or your load file
    * `<path-to-prolog-exe>` is the path to the SWI or SICSTUS executable

### Optional: Annotate your Code
With the provided `annotations` module, you can spec your own code.

#### Built-in Specs
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
* one_of
    * e.g. `one_of([int,atom])` - int or atom
* and
    * e.g. `and([ground,compound])` - ground and compound
    
#### Define your own specs
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

#### Annotate your code
* `:- spec_pre(foo/2,[int,atom]).` - Pre conditions, which must be fullfilled when calling predicate `foo`.
* `:- spec_post(bar/1,[var],[int]).` - If the first condition is valid when entering the predicate, the second must be valid, when the predicate is done

## Goal
Using spec annoations, we have a good describtion on how a term has to *look* regarding its structure. With this abstract describtions, we can find errors (wrong argument order, wrong return values, ...) without executing the prolog code itself. 

Instead we use abstact interpretation, which means, that we go through the code and collect every information we can about the terms *without* executing the code, just using the provided specs.

This knowledge collection is basically a list for every term, which holds the specs that are assigned to this term by a goal, which is called, or a relationship to other term.

If we find a spec which is not compatible with the term, or if some of the found specs are disjoint, we have found an error in the code (or the annotations ;-))


## License

Copyright © 2018 Isabel Wingen

Distributed under the MIT License
