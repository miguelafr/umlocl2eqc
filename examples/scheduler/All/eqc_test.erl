-module(eqc_test).

-include_lib("eqc/include/eqc.hrl").
-include_lib("eqc/include/eqc_statem.hrl").

-behaviour(eqc_statem).

%% EQC callbacks
-export([initial_state/0, command/1, precondition/2, dynamic_precondition/2, postcondition/3, next_state/3]).

%% Wrapper functions
-export([init/0, new/1, ready/1, swap/0]).

%% Testing function
-export([test/0]).


%% State
-record(ts, {
    active = undefined, 
    ready = undefined, 
    waiting = undefined
}).

%%====================================================================
%% property
%%====================================================================

test() ->
    ?FORALL(Cmds, commands(?MODULE),
        begin
            {H, PreState, Res} = run_commands(?MODULE, Cmds),
            ?WHENFAIL(io:format("H: ~p\nPreState: ~p\nRes: ~p\n", [H, PreState, Res]),
                aggregate(command_names(Cmds), Res==ok))
        end).

%%====================================================================
%% command
%%====================================================================

command(PreState) ->
    eqc_gen:oneof([
        {call, ?MODULE, init, []}, 
        {call, ?MODULE, new, [gen_process()]}, 
        {call, ?MODULE, ready, [gen_process()]}, 
        {call, ?MODULE, swap, []}
    ]).

%%====================================================================
%% initial_state
%%====================================================================

%%--------------------------------------------------------------------
%% initial_state: 
%%--------------------------------------------------------------------
initial_state() ->
    #ts {
        active = scheduler:get_active(), 
        ready = scheduler:get_ready(), 
        waiting = scheduler:get_waiting()
    }.

%%====================================================================
%% precondition
%%====================================================================

precondition(_PreState, _C) ->
    true.

%%--------------------------------------------------------------------
%% Init: preconditions
%%--------------------------------------------------------------------
dynamic_precondition(PreState, {call, ?MODULE, init, []})->
    true;

%%--------------------------------------------------------------------
%% New: preconditions
%%--------------------------------------------------------------------    
dynamic_precondition(PreState, {call, ?MODULE, new, [P]})->
    (ocl:neq(P, PreState#ts.active) andalso not(ocl_set:includes(P, ocl_set:union(PreState#ts.waiting, PreState#ts.ready))));

%%--------------------------------------------------------------------
%% Ready: preconditions
%%--------------------------------------------------------------------    
dynamic_precondition(PreState, {call, ?MODULE, ready, [P]})->
    ocl_set:includes(P, PreState#ts.waiting);

%%--------------------------------------------------------------------
%% Swap: preconditions
%%--------------------------------------------------------------------    
dynamic_precondition(PreState, {call, ?MODULE, swap, []})->
    ocl:neq(PreState#ts.active, undefined);

%%--------------------------------------------------------------------
%% preconditions: default
%%--------------------------------------------------------------------
dynamic_precondition(_PreState, _C) ->
    false.

%%====================================================================
%% next_state
%%====================================================================

%%--------------------------------------------------------------------
%% next_state: Init
%%--------------------------------------------------------------------
next_state(PreState, Result, {call, ?MODULE, init, []}) ->
    DynAfterState = {call, erlang, element, [3, Result]},
    PreState#ts { 
        active = {call, erlang, element, [2, DynAfterState]},
        ready = {call, erlang, element, [3, DynAfterState]},
        waiting = {call, erlang, element, [4, DynAfterState]}
    };

%%--------------------------------------------------------------------
%% next_state: New
%%--------------------------------------------------------------------
next_state(PreState, Result, {call, ?MODULE, new, [P]}) ->
    DynAfterState = {call, erlang, element, [3, Result]},
    PreState#ts { 
        active = {call, erlang, element, [2, DynAfterState]},
        ready = {call, erlang, element, [3, DynAfterState]},
        waiting = {call, erlang, element, [4, DynAfterState]}
    };

%%--------------------------------------------------------------------
%% next_state: Ready
%%--------------------------------------------------------------------
next_state(PreState, Result, {call, ?MODULE, ready, [P]}) ->
    DynAfterState = {call, erlang, element, [3, Result]},
    PreState1 = case ocl:eq(PreState#ts.active, undefined) of 
        true -> 
            PreState;
        false -> 
            PreState
    end, 
    PreState1#ts { 
        active = {call, erlang, element, [2, DynAfterState]},
        ready = {call, erlang, element, [3, DynAfterState]},
        waiting = {call, erlang, element, [4, DynAfterState]}
    };

%%--------------------------------------------------------------------
%% next_state: Swap
%%--------------------------------------------------------------------
next_state(PreState, Result, {call, ?MODULE, swap, []}) ->
    DynAfterState = {call, erlang, element, [3, Result]},
    PreState2 = case ocl_set:is_empty(PreState#ts.ready) of 
        true -> 
            PreState;
        false -> 
            PreState
    end, 
    PreState2#ts { 
        active = {call, erlang, element, [2, DynAfterState]},
        ready = {call, erlang, element, [3, DynAfterState]},
        waiting = {call, erlang, element, [4, DynAfterState]}
    };

%%--------------------------------------------------------------------
%% next_state: default
%%--------------------------------------------------------------------
next_state(PreState, Result, C) ->
    PreState.

%%====================================================================
%% postcondition
%%====================================================================

postcondition(PreState, C, R)->
    AfterState = eqc_symbolic:eval(next_state(PreState, R, C)), 
    postcondition(PreState, AfterState, C, R).

%%--------------------------------------------------------------------
%% Init: postconditions
%%--------------------------------------------------------------------    
postcondition(PreState, AfterState, {call, ?MODULE, init, []}, {Result, DynPreState, DynAfterState})->
    ((ocl_set:eq(AfterState#ts.ready, ocl_set:new_set()) andalso ocl_set:eq(AfterState#ts.waiting, ocl_set:new_set())) andalso ocl:eq(AfterState#ts.active, undefined));

%%--------------------------------------------------------------------
%% New: postconditions
%%--------------------------------------------------------------------    
postcondition(PreState, AfterState, {call, ?MODULE, new, [P]}, {Result, DynPreState, DynAfterState})->
    ((ocl_set:eq(AfterState#ts.waiting, ocl_set:including(P, PreState#ts.waiting)) andalso ocl_set:eq(AfterState#ts.ready, PreState#ts.ready)) andalso ocl:eq(AfterState#ts.active, PreState#ts.active));

%%--------------------------------------------------------------------
%% Ready: postconditions
%%--------------------------------------------------------------------    
postcondition(PreState, AfterState, {call, ?MODULE, ready, [P]}, {Result, DynPreState, DynAfterState})->
    (ocl_set:eq(AfterState#ts.waiting, ocl_set:excluding(P, PreState#ts.waiting)) andalso case ocl:eq(PreState#ts.active, undefined) of 
        true -> 
            (ocl_set:eq(AfterState#ts.ready, PreState#ts.ready) andalso ocl:eq(AfterState#ts.active, P));
        false -> 
            (ocl_set:eq(AfterState#ts.ready, ocl_set:including(P, PreState#ts.ready)) andalso ocl:eq(AfterState#ts.active, PreState#ts.active))
    end);

%%--------------------------------------------------------------------
%% Swap: postconditions
%%--------------------------------------------------------------------    
postcondition(PreState, AfterState, {call, ?MODULE, swap, []}, {Result, DynPreState, DynAfterState})->
    (case ocl_set:is_empty(PreState#ts.ready) of 
        true -> 
            (ocl:eq(AfterState#ts.active, undefined) andalso ocl_set:eq(AfterState#ts.ready, ocl_set:new_set()));
        false -> 
            (ocl_set:includes(AfterState#ts.active, PreState#ts.ready) andalso ocl_set:eq(AfterState#ts.ready, ocl_set:excluding(AfterState#ts.active, PreState#ts.ready)))
    end andalso ocl_set:eq(AfterState#ts.waiting, ocl_set:including(PreState#ts.active, PreState#ts.waiting)));

%%--------------------------------------------------------------------
%% postconditions: default
%%--------------------------------------------------------------------
postcondition(_PreState, _AfterState, _C, _R) ->
    false.

%%====================================================================
%% generators
%%====================================================================

%%--------------------------------------------------------------------
%% generator: process
%%--------------------------------------------------------------------
gen_process() ->
    {process, [{id, ocl_gen:gen_integer()}]}.


%%====================================================================
%% wrappers
%%====================================================================

init()->
    DynPreState = #ts {
        
    }, 
    Result = scheduler:init(),
    DynAfterState = #ts {
        active = scheduler:get_active(), 
        ready = scheduler:get_ready(), 
        waiting = scheduler:get_waiting()
    }, 
    {Result, DynPreState, DynAfterState}.

new(P)->
    DynPreState = #ts {
        
    }, 
    Result = scheduler:new(P),
    DynAfterState = #ts {
        active = scheduler:get_active(), 
        ready = scheduler:get_ready(), 
        waiting = scheduler:get_waiting()
    }, 
    {Result, DynPreState, DynAfterState}.

ready(P)->
    DynPreState = #ts {
        
    }, 
    Result = scheduler:ready(P),
    DynAfterState = #ts {
        active = scheduler:get_active(), 
        ready = scheduler:get_ready(), 
        waiting = scheduler:get_waiting()
    }, 
    {Result, DynPreState, DynAfterState}.

swap()->
    DynPreState = #ts {
        
    }, 
    Result = scheduler:swap(),
    DynAfterState = #ts {
        active = scheduler:get_active(), 
        ready = scheduler:get_ready(), 
        waiting = scheduler:get_waiting()
    }, 
    {Result, DynPreState, DynAfterState}.
