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
:- use_module(term_expander,[enable_write_out]).
:- enable_write_out.
```

Make sure that you have SWI Prolog installed.

Create an uberjar with `lein uberjar` and execute
```
java -jar targer/uberjar/prolog-analyzer-X.Y.Z-standalone.jar <path-to-your-file>

```

## Planned Features

- Annote code with specs
- Use Specs for static analysis
- Derive Specs for unspec'd predicates

## License

Copyright © 2018 Isabel Wingen

Distributed under the MIT License
