:- module(specfile, []).
%% csp_and_b_transition(concrete_constants(C),OpName,Trans,NewState,TransInfo) :-  !,
%%     Trans = tau(Op), OpName = '$CSP',
%%     b_trans(concrete_constants(C),_,Op,InitialBState,TransInfo),
%%     csp_initialisation_for_b(InitialCSPState),
%%     NewState = csp_and_b(InitialCSPState,InitialBState).

%% b_trans(State,OpName,Operation,NewState,PathInfo) :-
%% 	 compute_operation_effect_max(State,OpName,Operation,NewState,PathInfo,_Max).

%% compute_operation_effect_max([],OpName,Operation,NewState,PathInfo,Max) :-
%%      compute_operation_on_expanded_store([],OpName,Operation,Updates,PathInfo,Max),
%%      store:store_updates_and_normalise(Updates,[],NewState).
%% compute_operation_effect_max([H|T],OpName,Operation,NewState,PathInfo,Max) :-
%%      compute_operation_on_expanded_store([H|T],OpName,Operation,Updates,PathInfo,Max),
%%      store:store_updates_and_normalise(Updates,[H|T],NewState).
%% compute_operation_effect_max(const_and_vars(ConstID,Vars),OpName,Operation,ResultingStore,PathInfo,Max) :-
%%      prepare_state_for_specfile_trans(const_and_vars(ConstID,Vars),R),
%%      %print(expanded_const_and_vars(ConstID)),nl,
%%      compute_operation_effect_max(R,OpName,Operation,ResultingStore,PathInfo,Max).
compute_operation_effect_max(concrete_constants(ConstantsStore),'$initialise_machine',OpInit,ResultingStore,PathInfo,Max) :-
     get_max_enablings_for_init(Max,'$initialise_machine',MaxForCall),
     succeed_max_call_id('$initialise_machine',
                    b_interpreter:b_initialise_machine(ConstantsStore,InitialVars,InitialStore,PathInfo),MaxForCall),
     create_initialisation_operation(InitialVars,OpInit),
     %user:do_one_gui_event,
     (\+ preferences:preference(symmetry_mode,flood), /* TO DO : improve permutation to allow using const_and_vars optimisation */
       visited_expression(ConstID,concrete_constants(ConstantsStore))
      -> ResultingStore = const_and_vars(ConstID,InitialVars)  /* avoid storing constant values again */
      ;  ResultingStore = InitialStore).
%% compute_operation_effect_max(expanded_const_and_vars(ConstID,Vars,FullStore),OpName,Operation,ResultingStore,PathInfo,Max) :-
%%      %print(const_and_vars(ConstID,Vars)),nl,
%%      compute_operation_on_expanded_store(FullStore,OpName,Operation,Updates,PathInfo,Max),
%%      ResultingStore = const_and_vars(ConstID,NewVars),
%%      store:store_updates_and_normalise(Updates,Vars,NewVars).
%%      %store:store_updates(Updates,Vars,NewVars). % we could use this when operation re-used / cached
%%      %ErrorMsg = compute_operation_effect(const_and_vars(ConstID,Vars),Operation,ResultingStore,PathInfo),
%%      %reconstruct_constants_store(ConstID,ConstantsStore,NewState,ErrorMsg,ResultingStore).
compute_operation_effect_max(root,'$setup_constants',OpSetup,concrete_constants(FilteredConstantsStore),[],Max) :-
     b_machine_has_constants_or_properties,
     get_max_enablings_for_init(Max,'$setup_constants',MaxForCall),
     compute_constants(ConstantsStore,MaxForCall),
     create_setup_constants_operation(ConstantsStore,OpSetup),
     %%print_message('FOUND_CONSTANTS'(OpName)),
     (get_preference(filter_unused_constants,true)
       -> exclude(unused_binding,ConstantsStore,FilteredConstantsStore)
       ;  FilteredConstantsStore=ConstantsStore).
     %user:do_one_gui_event.
compute_operation_effect_max(root,'$initialise_machine',OpInit,InitialStore,PathInfo,Max) :-
     \+ b_machine_has_constants_or_properties,
     get_max_enablings_for_init(Max,'$initialise_machine',MaxForCall),
     %user:do_one_gui_event, user:do_one_gui_event, user:do_one_gui_event,
     empty_state(EmptyState),
     succeed_max_call_id('$initialise_machine',
                     b_interpreter:b_initialise_machine(EmptyState,InitialVars,InitialStore,PathInfo),MaxForCall),
     create_initialisation_operation(InitialVars,OpInit).
     %user:do_one_gui_event.
