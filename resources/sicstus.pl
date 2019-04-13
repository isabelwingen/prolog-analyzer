:- module(sicstus,[]).
:- use_module('../prolog/annotations',[spec_pre/2,spec_post/3,declare_spec/1,define_spec/2]).

time_if_debug(_140437):-debug_mode(off),!,_140437.

debug_mode(off).

expects_waitflag_and_span(_).
expects_waitflag(_).
type_ok_for_wf0(_).
reversible_unary_function(_).
kernel_call_or(_:_,_,_,_,_).
must_succ_kernel_call(_:_,_,_,_).

unary_kernel_call(_2899,_2991,_3043,_3099,_3143,_3195,_3247,_3299):-(expects_waitflag_and_span(_2991)->_3529=..[_2991,_3043,_3099,_3299,_3143];expects_waitflag(_2991)->_3529=..[_2991,_3043,_3099,_3143];_3529=..[_2991,_3043,_3099]),(type_ok_for_wf0(_3247)->_2899:_3529;reversible_unary_function(_2991)->kernel_call_or(_2899:_3529,_3043,_3099,_3143,_3195);must_succ_kernel_call(_2899:_3529,_3043,_3143,_3195)).
