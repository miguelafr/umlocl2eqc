-module(eqc_test).

-include_lib("eqc/include/eqc.hrl").
-include_lib("eqc/include/eqc_statem.hrl").

-behaviour(eqc_statem).

%% EQC callbacks
-export([initial_state/0, command/1, precondition/2, postcondition/3, next_state/3]).

%% Wrapper functions
-export([init/0, new/1, ready/1]).

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
        {call, ?MODULE, ready, [gen_process()]}
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

%%--------------------------------------------------------------------
%% Init: preconditions
%%--------------------------------------------------------------------
precondition(PreState, {call, ?MODULE, init, []})->
    true;

%%--------------------------------------------------------------------
%% New: preconditions
%%--------------------------------------------------------------------    
precondition(PreState, {call, ?MODULE, new, [P]})->
    (ocl:neq(P, PreState#ts.active) andalso not(ocl_set:includes(P, ocl_set:union(PreState#ts.waiting, PreState#ts.ready))));

%%--------------------------------------------------------------------
%% Ready: preconditions
%%--------------------------------------------------------------------    
precondition(PreState, {call, ?MODULE, ready, [P]})->
    ocl_set:includes(P, PreState#ts.waiting);

%%--------------------------------------------------------------------
%% preconditions: default
%%--------------------------------------------------------------------
precondition(_PreState, _C) ->
    false.

%%====================================================================
%% next_state
%%====================================================================

%%--------------------------------------------------------------------
%% next_state: Init
%%--------------------------------------------------------------------
next_state(PreState, Result, {call, ?MODULE, init, []}) ->
    DynAfterState = {call, erlang, element, [3, Result]},
    PreState37 = PreState#ts {
        ready = ocl_set:new_set()
    },
    PreState38 = PreState37#ts {
        waiting = ocl_set:new_set()
    },
    PreState39 = PreState38#ts {
        active = undefined
    }, 
    PreState39;

%%--------------------------------------------------------------------
%% next_state: New
%%--------------------------------------------------------------------
next_state(PreState, Result, {call, ?MODULE, new, [P]}) ->
    DynAfterState = {call, erlang, element, [3, Result]},
    PreState40 = PreState#ts {
        waiting = ocl_set:including(P, PreState#ts.waiting)
    },
    PreState41 = PreState40#ts {
        ready = PreState#ts.ready
    },
    PreState42 = PreState41#ts {
        active = PreState#ts.active
    }, 
    PreState42;

%%--------------------------------------------------------------------
%% next_state: Ready
%%--------------------------------------------------------------------
next_state(PreState, Result, {call, ?MODULE, ready, [P]}) ->
    DynAfterState = {call, erlang, element, [3, Result]},
    PreState43 = PreState#ts {
        waiting = ocl_set:excluding(P, PreState#ts.waiting)
    },
    PreState44 = case ocl:eq(PreState#ts.active, undefined) of 
        true -> 
            PreState45 = PreState43#ts {
                ready = PreState#ts.ready
            },
            PreState46 = PreState45#ts {
                active = P
            }, 
            PreState46;
        false -> 
            PreState47 = PreState43#ts {
                ready = ocl_set:including(P, PreState#ts.ready)
            },
            PreState48 = PreState47#ts {
                active = PreState#ts.active
            }, 
            PreState48
    end, 
    PreState44;

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
