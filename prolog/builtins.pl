:- module(builtins, []).
:- use_module(annotations,[spec_pre/2, spec_post/3, declare_spec/1, define_spec/2]).

:- spec_pre(user:acyclic_term/1, [any]).
:- spec_post(user:acyclic_term/1, [any], [any]).

:- spec_pre(user:atom/1, [any]).
:- spec_post(user:atom/1, [any], [atom]).

:- spec_pre(user:atomic/1, [any]).
:- spec_post(user:atomic/1, [any], [atomic]).

:- declare_spec(callable).

:- define_spec(callable, one_of([compound, atom])).

:- spec_pre(user:callable/1, [any]).
:- spec_post(user:callable/1, [any], [callable]).


:- spec_pre(user:compound/1, [any]).
:- spec_post(user:compound/1, [any], [compound]).

:- spec_pre(user:float/1, [any]).
:- spec_post(user:float/1, [any], [float]).

:- spec_pre(user:ground/1, [any]).
:- spec_post(user:ground/1, [any], [ground]).

:- spec_pre(user:integer/1, [any]).
:- spec_post(user:integer/1, [any], [integer]).

:- spec_pre(user:number/1, [any]).
:- spec_post(user:number/1, [any], [number]).

:- spec_pre(user:nonvar/1, [any]).
:- spec_post(user:nonvar/1, [any], [nonvar]).

:- spec_pre(user:var/1, [any]).
:- spec_post(user:var/1, [any], [var]).


:- declare_spec(maybe(specvar(_X))).
:- define_spec(maybe(specvar(X)), one_of([var, specvar(X)])).

:- spec_pre(user:atom_chars/2, [atom, list(atom)]).
:- spec_pre(user:atom_chars/2, [atom, var]).
:- spec_pre(user:atom_chars/2, [var, list(atom)]).
:- spec_post(user:atom_chars/2, [any, any], [atom, list(atom)]).

:- spec_pre(user:atom_codes/2, [atom, list(int)]).
:- spec_pre(user:atom_codes/2, [atom, var]).
:- spec_pre(user:atom_codes/2, [var, list(int)]).
:- spec_post(user:atom_codes/2, [any, any], [atom, list(int)]).

:- spec_pre(user:atom_concat/3, [var, atom, atom]).
:- spec_pre(user:atom_concat/3, [atom, var, atom]).
:- spec_pre(user:atom_concat/3, [atom, atom, atom]).
:- spec_pre(user:atom_concat/3, [var, var, atom]).
:- spec_pre(user:atom_concat/3, [atom, atom, var]).
:- spec_post(user:atom_concat/3, [any, any, any], [atom, atom, atom]).

:- spec_pre(user:atom_length/2, [atom, int]).
:- spec_pre(user:atom_length/2, [atom, var]).
:- spec_post(user:atom_length/2, [any, any], [atom, int]).

:- spec_pre(user:number_chars/2, [int, list(atom)]).
:- spec_pre(user:number_chars/2, [int, var]).
:- spec_pre(user:number_chars/2, [var, list(atom)]).
:- spec_post(user:number_chars/2, [any, any], [atom, list(atom)]).

:- spec_pre(user:number_codes/2, [int, list(int)]).
:- spec_pre(user:number_codes/2, [int, var]).
:- spec_pre(user:number_codes/2, [var, list(int)]).
:- spec_post(user:number_codes/2, [any, any], [int, list(int)]).

:- spec_pre(user:halt/1, [int]).
% no spec_post

:- spec_pre(user:throw/1, [nonvar]).
% no spec_post

:- spec_pre(user:term_variables/2, [any, list(var)]).
:- spec_pre(user:term_variables/2, [any, var]).
:- spec_post(user:term_variables/2, [any, any], [any, list(var)]).

:- spec_pre(user:sort/2, [list(specvar(X)), list(specvar(X))]).
:- spec_pre(user:sort/2, [list(any), var]).
:- spec_post(user:sort/2, [any, any], [list(specvar(X)), list(specvar(X))]).

:- spec_pre(user:sub_atom/5, [atom, maybe(int), maybe(int), maybe(int), maybe(atom)]).
:- spec_post(user:sub_atom/5, [any, any, any, any, any], [atom, int, int, int, atom]).

:- declare_spec(pair).
:- define_spec(pair, compound('-'(any, any))).
:- declare_spec(pair(specvar(_X), specvar(_Y))).
:- define_spec(pair(specvar(X), specvar(Y)), compound('-'(specvar(X), specvar(Y)))).

:- spec_pre(user:keysort/2, [list(pair), list(pair)]).
:- spec_pre(user:keysort/2, [list(pair), var]).
:- spec_post(user:keysort/2, [any, any], [list(pair), list(pair)]).

:- spec_pre(user:copy_term/2, [any, any]). % not a specvar!
:- spec_post(user:copy_term/2, [any, any], [any, any]).

:- spec_pre(user:op/3, [int, one_of([atom(xfx), atom(xfy), atom(yfx), atom(fx), atom(fy), atom(xf), atom(yf)]), one_of([atom, list(atom)])]).
% no spec_post

:- spec_pre(user:current_op/3, [maybe(int), maybe(one_of([atom(xfx), atom(xfy), atom(yfx), atom(fx), atom(fy), atom(xf), atom(yf)])), maybe(atom)]).
:- spec_post(user:current_op/3, [any, any, any], [int, one_of([atom(xfx), atom(xfy), atom(yfx), atom(fx), atom(fy), atom(xf), atom(yf)]), atom]).

:- spec_pre(user:'=..'/2, [nonvar, maybe(list(any))]).
:- spec_pre(user:'=..'/2, [var, list(any)]).
:- spec_post(user:'=..'/2, [any, any], [nonvar, list(any)]).

:- spec_pre(user:subsumes_term/2, [any, any]).
% no spec_post

:- spec_pre(user:functor/3, [nonvar, maybe(atomic), maybe(int)]).
:- spec_pre(user:functor/3, [var, atomic, int]).
:- spec_post(user:functor/3, [any, any, any], [nonvar, atomic, int]).


:- spec_pre(user:arg/3, [int, compound, maybe(any)]).
:- spec_post(user:arg/3, [any, any, any], [int, compound, any]).

:- spec_pre(user:catch/3, [callable, maybe(any), callable]).
:- spec_post(user:catch/3, [any, any, any], [callable, nonvar, callable]).

:- spec_pre(user:bagof/3, [any, callable, maybe(list(any))]).
:- spec_post(user:bagof/3, [any, any, any], [any, callable, list(any)]).

:- spec_pre(user:findall/3, [any, callable, maybe(list(any))]).
:- spec_post(user:findall/3, [any, any, any], [any, callable, list(any)]).

:- spec_pre(user:setof/3, [any, callable, maybe(list(any))]).
:- spec_post(user:setof/3, [any, any, any], [any, callable, list(any)]).

:- spec_pre(user:char_code/2, [atom, maybe(int)]).
:- spec_pre(user:char_code/2, [var, int]).
:- spec_post(user:char_code/2, [any, any], [atom, int]).

:- spec_pre(user:current_char_conversion/2, [maybe(atom), maybe(atom)]).
:- spec_post(user:current_char_conversion/2, [any, any], [atom, atom]).
% no spec_post

:- spec_pre(user:char_conversion/2, [atom, atom]).
% no spec_post

:- spec_pre(user:compare/3, [one_of([atom(=), atom(<), atom(>)]), nonvar, nonvar]).
% no spec_post

:- declare_spec(arithmetic_expression).
:- define_spec(arithmetic_expression, one_of([number,
                                              compound(+(arithmetic_expression)),
                                              compound(-(arithmetic_expression)),
                                              compound(arithmetic_expression + arithmetic_expression),
                                              compound(arithmetic_expression - arithmetic_expression),
                                              compound(arithmetic_expression * arithmetic_expression),
                                              compound(arithmetic_expression / arithmetic_expression),
                                              compound(arithmetic_expression // arithmetic_expression),
                                              compound(arithmetic_expression div arithmetic_expression),
                                              compound(arithmetic_expression rem arithmetic_expression),
                                              compound(arithmetic_expression mod arithmetic_expression),
                                              compound(float_integer_part(arithmetic_expression)),
                                              compound(float_fractional_part(arithmetic_expression)),
                                              compound(float(arithmetic_expression)),
                                              compound(arithmetic_expression /\ arithmetic_expression),
                                              compound(arithmetic_expression \/ arithmetic_expression),
                                              compound(xor(arithmetic_expression, arithmetic_expression)),
                                              compound(\(arithmetic_expression)),
                                              compound(arithmetic_expression << arithmetic_expression),
                                              compound(arithmetic_expression >> arithmetic_expression),
                                              compound(abs(arithmetic_expression)),
                                              compound(sign(arithmetic_expression)),
                                              compound(min(arithmetic_expression, arithmetic_expression)),
                                              compound(max(arithmetic_expression, arithmetic_expression)),
                                              compound(round(arithmetic_expression)),
                                              compound(truncate(arithmetic_expression)),
                                              compound(floor(arithmetic_expression)),
                                              compound(ceiling(arithmetic_expression)),
                                              compound(sin(arithmetic_expression)),
                                              compound(cos(arithmetic_expression)),
                                              compound(tan(arithmetic_expression)),
                                              compound(cot(arithmetic_expression)),
                                              compound(sinh(arithmetic_expression)),
                                              compound(cosh(arithmetic_expression)),
                                              compound(tanh(arithmetic_expression)),
                                              compound(coth(arithmetic_expression)),
                                              compound(asin(arithmetic_expression)),
                                              compound(acos(arithmetic_expression)),
                                              compound(atan(arithmetic_expression)),
                                              compound(atan2(arithmetic_expression, arithmetic_expression)),
                                              compound(sqrt(arithmetic_expression)),
                                              compound(log(arithmetic_expression)),
                                              compound(exp(arithmetic_expression)),
                                              compound(arithmetic_expression ** arithmetic_expression),
                                              compound(arithmetic_expression ^ arithmetic_expression),
                                              atom(pi) ])).

:- spec_pre(user:is/2, [maybe(number), arithmetic_expression]).
:- spec_post(user:is/2, [any, any], [number, arithmetic_expression]).

:- spec_pre(user:'<'/2, [arithmetic_expression, arithmetic_expression]).
% no spec post

:- spec_pre(user:'>'/2, [arithmetic_expression, arithmetic_expression]).
% no spec post

:- spec_pre(user:'=<'/2, [arithmetic_expression, arithmetic_expression]).
% no spec post

:- spec_pre(user:'>='/2, [arithmetic_expression, arithmetic_expression]).
% no spec post

:- spec_pre(user:'=:='/2, [arithmetic_expression, arithmetic_expression]).
% no spec post

:- spec_pre(user:'=\\='/2, [arithmetic_expression, arithmetic_expression]).
% no spec post

:- spec_pre(user:'='/2, [any, any]).
:- spec_post(user:'='/2, [any, any], [specvar(X), specvar(X)]).
% no spec post

:- spec_pre(user:'\\='/2, [any, any]).
% no spec post

:- spec_pre(user:'=='/2, [any, any]).
:- spec_post(user:'=='/2, [any, any], [specvar(X), specvar(X)]).

:- spec_pre(user:'\\=='/2, [any, any]).
% no spec post

:- spec_pre(user:'@>2'/2, [any, any]).
% no spec post

:- spec_pre(user:'@<2'/2, [any, any]).
% no spec post

:- spec_pre(user:'@>=2'/2, [any, any]).
% no spec post

:- spec_pre(user:'@=<2'/2, [any, any]).
% no spec post

:- spec_pre(user:'\\+'/1, [callable]).
% no spec post

:- spec_pre(user:';'/2, [callable, callable]).
% no spec post

:- spec_pre(user:','/2, [callable, callable]).
% no spec post

:- spec_pre(user:'->'/2, [callable, callable]).
% no spec post

:- spec_pre(user:asserta/1, [callable]).
% no spec post

:- spec_pre(user:assertz/1, [callable]).
% no spec post

:- spec_pre(user:retract/1, [callable]).
% no spec post

:- spec_pre(user:retractall/1, [callable]).
% no spec post

:- spec_pre(user:once/1, [callable]).
% no spec post

:- declare_spec(stream).
:- define_spec(stream, ground). %TODO: improve

:- spec_pre(user:at_end_of_stream/1, [stream]).
% no spec post

:- spec_pre(user:current_input/1, [maybe(stream)]).
:- spec_post(user:current_input/1, [any], [stream]).

:- spec_pre(user:current_output/1, [maybe(stream)]).
:- spec_post(user:current_output/1, [any], [stream]).

:- spec_pre(user:flush_output/1, [stream]).
% no spec post

:- spec_pre(user:get_byte/1, [maybe(int)]).
:- spec_post(user:get_byte/1, [any], [int]).

:- spec_pre(user:get_byte/2, [stream, maybe(int)]).
:- spec_post(user:get_byte/2, [any, any], [stream, int]).

:- spec_pre(user:get_code/1, [maybe(int)]).
:- spec_post(user:get_code/1, [any], [int]).

:- spec_pre(user:get_code/2, [stream, maybe(int)]).
:- spec_post(user:get_code/2, [any, any], [stream, int]).

:- spec_pre(user:get_char/1, [maybe(atom)]).
:- spec_post(user:get_char/1, [any], [atom]).

:- spec_pre(user:get_char/2, [stream, maybe(atom)]).
:- spec_post(user:get_char/2, [any, any], [stream, atom]).

:- spec_pre(user:peek_byte/1, [maybe(int)]).
:- spec_post(user:peek_byte/1, [any], [int]).

:- spec_pre(user:peek_byte/2, [stream, maybe(int)]).
:- spec_post(user:peek_byte/2, [any, any], [stream, int]).

:- spec_pre(user:peek_code/1, [maybe(int)]).
:- spec_post(user:peek_code/1, [any], [int]).

:- spec_pre(user:peek_code/2, [stream, maybe(int)]).
:- spec_post(user:peek_code/2, [any, any], [stream, int]).

:- spec_pre(user:peek_char/1, [maybe(atom)]).
:- spec_post(user:peek_char/1, [any], [atom]).

:- spec_pre(user:peek_char/2, [stream, maybe(atom)]).
:- spec_post(user:peek_char/2, [any, any], [stream, atom]).

:- spec_pre(user:put_byte/1, [int]).
% no spec post

:- spec_pre(user:put_byte/2, [stream, int]).
% no spec post

:- spec_pre(user:put_code/1, [int]).
% no spec post

:- spec_pre(user:put_code/2, [stream, int]).
% no spec post

:- spec_pre(user:put_char/1, [atom]).
% no spec post

:- spec_pre(user:put_char/2, [stream, atom]).
% no spec post

:- spec_pre(user:read/1, [maybe(any)]).
:- spec_post(user:read/1, [any], [any]).

:- spec_pre(user:read/2, [stream, maybe(any)]).
:- spec_post(user:read/2, [stream, any], [stream, any]).

:- declare_spec(boolean).
:- define_spec(boolean, one_of([atom(true), atom(false)])).

:- declare_spec(read_term_option).
:- define_spec(read_term_option, one_of([compound(syntax_errors(atom)),
                                         compound(variables(list(any))),
                                         compound(variable_names(list(compound(=(maybe(atom), any))))),
                                         compound(singletons(list(any))),
                                         compound(cycles(boolean)) ])).

:- spec_pre(user:read_term/2, [maybe(any), list(read_term_option)]). % any already includes var?
:- spec_post(user:read_term/2, [any, any], [any, list(read_term_option)]). %TODO: clarify if nonvar

:- spec_pre(user:read_term/3, [stream, maybe(any), list(read_term_option)]).
:- spec_post(user:read_term/3, [any, any, any], [stream, any, list(read_term_option)]).

:- spec_pre(user:close/1, [stream]).
:- spec_post(user:close/1, [any], [stream]).

:- spec_pre(user:close/2, [stream, list(compound(force(atom(true))))]).
% no spec post

:- spec_pre(user:nl/1, [stream]).
% no spec post

:- declare_spec(file).
:- define_spec(file, one_of([atom, string, and([compound,ground])])).

:- spec_pre(user:open/3, [file, one_of([atom(read), atom(write), atom(append)]), maybe(stream)]).
:- spec_post(user:open/3, [any, any, any], [file, one_of([atom(read), atom(write), atom(append)]), stream]).

:- declare_spec(open_option).
:- define_spec(open_option, one_of([compound(alias(atom)),
                                    compound(encoding(atom)),
                                    compound(eof_action(one_of([atom(error), atom(eof_code), atom(reset)]))) ])).

:- spec_pre(user:open/4, [file, one_of([atom(read), atom(write), atom(append)]), list(open_option), maybe(stream)]).
:- spec_post(user:open/4, [any, any, any, any], [file, one_of([atom(read), atom(write), atom(append)]), list(open_option), stream]).

:- spec_pre(user:set_input/1, [stream]).
% no spec post

:- spec_pre(user:set_output/1, [stream]).
% no spec post

:- spec_pre(user:unify_with_occurs_check/2, [any, any]).
% no spec post

:- spec_pre(user:write/1, [any]).
% no spec post

:- spec_pre(user:write/2, [stream, any]).
% no spec post

:- spec_pre(user:write_canonical/1, [any]).
% no spec post

:- spec_pre(user:write_canonical/2, [stream, any]).
% no spec post

:- spec_pre(user:writeq/1, [any]).
% no spec post

:- spec_pre(user:writeq/2, [stream, any]).
% no spec post

:- spec_pre(user:ensure_loaded/1, [one_of([file, list(file)])]).
% no spec post

:- spec_pre(user:clause/2, [callable, maybe(callable)]).
:- spec_post(user:clause/2, [any, any], [callable, callable]).

:- spec_pre(user:abolish/1, [callable]).
% no spec post

:- spec_pre(user:current_predicate/1, [callable]).
% no spec post

:- spec_pre(user:member/2, [any, maybe(list(any))]).
:- spec_post(user:member/2, [any,any], [specvar(X), list(specvar(X))]).

%    11.3.56 current_prolog_flag/2   ISO
%    11.3.206 set_prolog_flag/2   ISO
%    11.3.207 set_stream_position/2   ISO
%    11.3.221 stream_property/2   ISO
%    11.3.253 write_term/[2,3]   hookable, ISO

%    11.3.30 call/[1,2,...,255]   ISO
%    11.3.65 discontiguous/1   declaration, ISO
%    11.3.68 dynamic/1   declaration, ISO
%    11.3.104 include/1   declaration, ISO
%    11.3.105 initialization/1   declaration, ISO
%    11.3.125 multifile/1   declaration, ISO


% TODO: Keys must be ground
:- declare_spec(avl_tree(specvar(_Key), specvar(_Value))).
:- define_spec(avl_tree(specvar(Key), specvar(Value)), one_of([compound(node(specvar(Key),
                                                                             specvar(Value),
                                                                             integer,
                                                                             avl_tree(specvar(Key),specvar(Value)),
                                                                             avl_tree(specvar(Key),specvar(Value)))),
                                                               atom(empty)])).

:- declare_spec(ordset(specvar(_Type))).
:- define_spec(ordset(specvar(X)), list(specvar(X))). %% TODO: this would make a great deferred spec

:- spec_pre(avl:avl_to_list/2,   [avl_tree(specvar(Key), specvar(Value)), maybe(list(pair(specvar(Key), specvar(Value))))]).
:- spec_post(avl:avl_to_list/2,  [any, any], [avl_tree(specvar(Key), specvar(Value)), list(pair(specvar(Key), specvar(Value)))]).

:- spec_pre(avl:avl_domain/2,   [avl_tree(specvar(Key), specvar(_Value)), maybe(list(specvar(Key)))]).
:- spec_post(avl:avl_domain/2,  [any, any], [avl_tree(specvar(Key), specvar(_Value)), list(specvar(Key))]).

:- spec_pre(avl:avl_fetch/2,  [specvar(Key), avl_tree(specvar(Key), specvar(_Value))]).
% no spec post

:- spec_pre(avl:avl_fetch/3,  [specvar(Key), avl_tree(specvar(Key), specvar(Value)), maybe(specvar(Value))]).
:- spec_post(avl:avl_fetch/3, [any, any, any], [specvar(Key), avl_tree(specvar(Key), specvar(Value)), specvar(Value)]).

:- spec_pre(avl:avl_height/2,  [avl_tree(specvar(_Key), specvar(_Value)), maybe(integer)]).
:- spec_post(avl:avl_height/2, [any, any], [avl_tree(specvar(_Key), specvar(_Value)), integer]).

:- spec_pre(avl:avl_max/2,  [avl_tree(specvar(_Key), specvar(_Value)), any]).
:- spec_post(avl:avl_max/2, [any, any], [avl_tree(specvar(Key), specvar(_Value)), specvar(Key)]).

:- spec_pre(avl:avl_max/3,  [avl_tree(specvar(_Key), specvar(_Value)), any, any]).
:- spec_post(avl:avl_max/3, [any, any, any], [avl_tree(specvar(Key), specvar(Value)), specvar(Key), specvar(Value)]).

:- spec_pre(avl:avl_min/2,  [avl_tree(specvar(_Key), specvar(_Value)), any]).
:- spec_post(avl:avl_min/2, [any, any], [avl_tree(specvar(Key), specvar(_Value)), specvar(Key)]).

:- spec_pre(avl_min/3,  [avl_tree(specvar(_Key), specvar(_Value)), any, any]).
:- spec_post(avl:avl_min/3, [any, any, any], [avl_tree(specvar(Key), specvar(Value)), specvar(Key), specvar(Value)]).

:- spec_pre(avl:avl_member/2,  [var, avl_tree(specvar(_Key), specvar(_Value))]).
:- spec_post(avl:avl_member/2,  [any, any], [specvar(Key), avl_tree(specvar(Key), specvar(_Value))]).

:- spec_pre(avl:avl_member/3,  [var, avl_tree(specvar(_Key), specvar(_Value)), any]).
:- spec_post(avl:avl_member/3,  [any, any, any], [specvar(Key), avl_tree(specvar(Key), specvar(Value)), specvar(Value)]).

:- spec_pre(avl:avl_next/3,  [specvar(Key), avl_tree(specvar(Key), specvar(_Value)), maybe(specvar(Key))]).
:- spec_post(avl:avl_next/3,  [any, any, any], [specvar(Key), avl_tree(specvar(Key), specvar(_Value)), specvar(Key)]).

:- spec_pre(avl:avl_next/4,  [specvar(Key), avl_tree(specvar(Key), specvar(Value)), maybe(specvar(Key)), maybe(specvar(Value))]).
:- spec_post(avl:avl_next/4, [any, any, any, any], [specvar(Key), avl_tree(specvar(Key), specvar(Value)), specvar(Key), specvar(Value)]).

:- spec_pre(avl:avl_prev/3,  [specvar(Key), avl_tree(specvar(Key), specvar(_Value)), maybe(specvar(Key))]).
:- spec_post(avl:avl_prev/3,  [any, any, any], [specvar(Key), avl_tree(specvar(Key), specvar(_Value)), specvar(Key)]).

:- spec_pre(avl:avl_prev/4,  [specvar(Key), avl_tree(specvar(Key), specvar(Value)), maybe(specvar(Key)), maybe(specvar(Value))]).
:- spec_post(avl:avl_prev/4, [any, any, any, any], [specvar(Key), avl_tree(specvar(Key), specvar(Value)), specvar(Key), specvar(Value)]).


:- spec_pre(avl:avl_range/2,  [avl_tree(specvar(_Key), specvar(Value)), maybe(list(specvar(Value)))]).
:- spec_post(avl:avl_range/2, [any, any],  [avl_tree(specvar(_Key), specvar(Value)), list(specvar(Value))]).

:- spec_pre(avl:avl_size/2,  [avl_tree(specvar(_Key), specvar(_Value)), maybe(integer)]).
:- spec_post(avl:avl_size/2, [any, any], [avl_tree(specvar(_Key), specvar(_Value)), integer]).

% crazy
:- spec_pre(avl:avl_store/4,  [ground, avl_tree(specvar(_Key), specvar(_Value)), any, avl_tree(specvar(_Key2), specvar(_Value2))]).
:- spec_post(avl:avl_store/4, [any, any, any, any], [specvar(NewKey), avl_tree(specvar(Key), specvar(Value)), specvar(NewValue),
                                                 avl_tree(one_of([specvar(NewKey), specvar(Key)]),
                                                          one_of([specvar(NewValue), specvar(Value)]))]).

:- spec_pre(avl:avl_delete/4,  [specvar(Key), avl_tree(specvar(Key), specvar(Value)), maybe(specvar(Value)), maybe(avl_tree(specvar(Key), specvar(Value)))]).
:- spec_post(avl:avl_delete/4, [any, any, any, any], [specvar(Key), avl_tree(specvar(Key), specvar(Value)), specvar(Value), avl_tree(specvar(Key), specvar(Value))]).

:- spec_pre(avl:avl_del_max/4,  [avl_tree(specvar(Key), specvar(Value)), maybe(specvar(Key)), maybe(specvar(Value)), maybe(avl_tree(specvar(Key), specvar(Value)))]).
:- spec_post(avl:avl_del_max/4, [any,any,any,any],  [avl_tree(specvar(Key), specvar(Value)), specvar(Key), specvar(Value), avl_tree(specvar(Key), specvar(Value))]).

:- spec_pre(avl:avl_del_min/4,  [avl_tree(specvar(Key), specvar(Value)), maybe(specvar(Key)), maybe(specvar(Value)), maybe(avl_tree(specvar(Key), specvar(Value)))]).
:- spec_post(avl:avl_del_min/4, [any,any,any,any],  [avl_tree(specvar(Key), specvar(Value)), specvar(Key), specvar(Value), avl_tree(specvar(Key), specvar(Value))]).

:- spec_pre(avl:ord_list_to_avl/2,  [ordset(pair), any]).
:- spec_post(avl:ord_list_to_avl/2, [any,any], [ordset(pair(specvar(Key), specvar(Value))), avl_tree(specvar(Key), specvar(Value))]).

:- spec_pre(avl:avl_change/5,  [specvar(Key), avl_tree(specvar(Key), specvar(Value)), maybe(specvar(Value)),
                                          maybe(avl_tree(specvar(Key), one_of([specvar(Value), specvar(Value2)]))),
                                          maybe(specvar(Value2))]).
:- spec_post(avl:avl_change/5, [any,any,any,any,any], [specvar(Key), avl_tree(specvar(Key), specvar(Value)), specvar(Value),
                                                  avl_tree(specvar(Key), one_of([specvar(Value), specvar(Value2)])),
                                                  specvar(Value2)]).



:- spec_pre(user:length/2, [maybe(list(any)), maybe(int)]).
:- spec_post(user:length/2, [any, any], [list(any), int]).

:- spec_pre(user:is_list/1, [any]).
:- spec_post(user:is_list/1, [any], [list(any)]).

:- spec_pre(user:memberchk/2, [any, maybe(list(any))]).
:- spec_post(user:memberchk/2, [any,any], [specvar(X), list(specvar(X))]).

:- spec_pre(user:msort/2, [list(specvar(X)), list(specvar(X))]).
:- spec_pre(user:msort/2, [list(any), var]).
:- spec_post(user:msort/2, [any, any], [list(specvar(X)), list(specvar(X))]).

:- spec_pre(lists:append/2,  [list(list(any)), maybe(list(any))]).
:- spec_post(lists:append/2,  [any, any], [list(list(any)), list(any)]).

:- spec_pre(lists:flatten/2,  [list(list(any)), maybe(list(any))]).
:- spec_post(lists:flatten/2,  [any, any], [list(list(any)), list(any)]).

:- spec_pre(append/3,  [maybe(list(any)), maybe(list(any)), maybe(list(any))]).
:- spec_post(append/3, [any,any,any], [list(specvar(X)), list(specvar(Y)), list(one_of([specvar(X), specvar(Y)]))]).

:- spec_pre(lists:prefix/2,  [maybe(list(any)), maybe(list(any))]).
:- spec_post(lists:prefix/2, [any,any], [list(specvar(X)), list(specvar(X))]).

:- spec_pre(lists:select/3,  [any, maybe(list(any)), maybe(list(any))]).
:- spec_post(lists:select/3, [any,any,any], [compatible(X), and([list(union(X)),compatible(Y)]), and([list(union(X)),compatible(Y)])]).

:- spec_pre(lists:selectchk/3,  [any, maybe(list(any)), maybe(list(any))]).
:- spec_post(lists:selectchk/3, [any,any,any], [specvar(X), list(specvar(X)), list(specvar(X))]).

:- spec_pre(lists:delete/3,  [maybe(list(any)), any, maybe(list(any))]).
:- spec_post(lists:delete/3,  [any,any,any], [list(specvar(X)), specvar(X), list(specvar(X))]).

:- spec_pre(lists:nth0/3,  [maybe(int), maybe(list(any)), any]).
:- spec_post(lists:nth0/3,  [any,any,any], [int, list(specvar(X)), specvar(X)]).

:- spec_pre(lists:nth1/3,  [maybe(int), maybe(list(any)), any]).
:- spec_post(lists:nth1/3,  [any,any,any], [int, list(specvar(X)), specvar(X)]).

:- spec_pre(lists:nth0/4,  [maybe(int), maybe(list(any)), any, maybe(list(any))]).
:- spec_post(lists:nth0/4,  [any,any,any,any], [int, list(specvar(X)), specvar(X), list(specvar(X))]).

:- spec_pre(lists:nth1/4,  [maybe(int), maybe(list(any)), any, maybe(list(any))]).
:- spec_post(lists:nth1/4,  [any,any,any,any], [int, list(specvar(X)), specvar(X), list(specvar(X))]).

:- spec_pre(lists:proper_length/2, [list(any), maybe(int)]).
:- spec_post(lists:proper_length/2, [any, any], [list(any), int]).

:- spec_pre(lists:reverse/2, [maybe(list(specvar(X))), list(specvar(X))]).
:- spec_pre(lists:reverse/2, [list(specvar(X)), maybe(list(specvar(X)))]).
:- spec_post(lists:reverse/2, [any, any], [list(specvar(X)), list(specvar(X))]).

:- spec_pre(lists:permutation/2, [maybe(list(specvar(X))), list(specvar(X))]).
:- spec_pre(lists:permutation/2, [list(specvar(X)), maybe(list(specvar(X)))]).
:- spec_post(lists:permutation/2, [any, any], [list(specvar(X)), list(specvar(X))]).

:- spec_pre(lists:max_member/2, [any, list(any)]).
:- spec_post(lists:max_member/2, [any,any], [specvar(X), list(specvar(X))]).

:- spec_pre(lists:min_member/2, [any, list(any)]).
:- spec_post(lists:min_member/2, [any,any], [specvar(X), list(specvar(X))]).

:- spec_pre(lists:sum_list/2, [list(number), maybe(number)]).
:- spec_post(lists:sum_list/2, [any,any], [list(number), number]).

:- spec_pre(lists:max_list/2, [list(number), maybe(number)]).
:- spec_post(lists:max_list/2, [any,any], [list(number), number]).

:- spec_pre(lists:min_list/2, [list(number), maybe(number)]).
:- spec_post(lists:min_list/2, [any,any], [list(number), number]).

:- spec_pre(lists:numlist/3, [integer, integer, maybe(list(integer))]).
:- spec_post(lists:numlist/3, [any,any,any], [integer, integer, list(integer)]).
