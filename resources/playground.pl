:- module(playground, []).

unary_kernel_call(Module,KernelFunction,ESV1,Value,WF,Expr,Type,Span) :-
    (expects_waitflag_and_span(KernelFunction)
    ->  KernelCall =.. [KernelFunction,ESV1,Value,Span,WF]
    ;  expects_waitflag(KernelFunction) ->  KernelCall =.. [KernelFunction,ESV1,Value,WF]
    ;  KernelCall =.. [KernelFunction,ESV1,Value]
    ),
    %print(call(Type,KernelCall)),nl,
    (type_ok_for_wf0(Type) -> %print(direct_call(Type,KernelCall)),nl,
         Module:KernelCall
    ; reversible_unary_function(KernelFunction)
      -> kernel_call_or(Module:KernelCall,ESV1,Value,WF,Expr)
    ;  must_succ_kernel_call(Module:KernelCall,ESV1,WF,Expr)
    ).
