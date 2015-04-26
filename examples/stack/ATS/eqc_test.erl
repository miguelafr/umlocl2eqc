-module(eqc_test).

-include_lib("eqc/include/eqc.hrl").
-include_lib("eqc/include/eqc_statem.hrl").

-behaviour(eqc_statem).

%% EQC callbacks
-export([initial_state/0, command/1, precondition/2, dynamic_precondition/2, postcondition/3, next_state/3]).

%% Wrapper functions
-export([pop/0, push/1, top/0, size/0, isEmpty/0]).

%% Testing function
-export([test/0]).


%% State
-record(ts, {
    isEmpty = undefined, 
    size = undefined, 
    top = undefined
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
        {call, ?MODULE, pop, []}, 
        {call, ?MODULE, push, [ocl_gen:gen_integer()]}, 
        {call, ?MODULE, top, []}, 
        {call, ?MODULE, size, []}, 
        {call, ?MODULE, isEmpty, []}
    ]).

%%====================================================================
%% initial_state
%%====================================================================

%%--------------------------------------------------------------------
%% initial_state: 
%%--------------------------------------------------------------------
initial_state() ->
    #ts {
        isEmpty = undefined, 
        size = undefined, 
        top = undefined
    }.

%%====================================================================
%% precondition
%%====================================================================

precondition(_PreState, _C) ->
    true.

%%--------------------------------------------------------------------
%% pop: preconditions
%%--------------------------------------------------------------------    
dynamic_precondition(PreState, {call, ?MODULE, pop, []})->
    (PreState#ts.isEmpty == false);

%%--------------------------------------------------------------------
%% push: preconditions
%%--------------------------------------------------------------------
dynamic_precondition(PreState, {call, ?MODULE, push, [O]})->
    true;

%%--------------------------------------------------------------------
%% top: preconditions
%%--------------------------------------------------------------------    
dynamic_precondition(PreState, {call, ?MODULE, top, []})->
    (PreState#ts.isEmpty == false);

%%--------------------------------------------------------------------
%% size: preconditions
%%--------------------------------------------------------------------
dynamic_precondition(PreState, {call, ?MODULE, size, []})->
    true;

%%--------------------------------------------------------------------
%% isEmpty: preconditions
%%--------------------------------------------------------------------
dynamic_precondition(PreState, {call, ?MODULE, isEmpty, []})->
    true;

%%--------------------------------------------------------------------
%% preconditions: default
%%--------------------------------------------------------------------
dynamic_precondition(_PreState, _C) ->
    false.

%%====================================================================
%% next_state
%%====================================================================

%%--------------------------------------------------------------------
%% next_state: pop
%%--------------------------------------------------------------------
next_state(PreState, Result, {call, ?MODULE, pop, []}) ->
    DynAfterState = {call, erlang, element, [3, Result]},
    PreState#ts { 
        isEmpty = {call, erlang, element, [2, DynAfterState]},
        size = {call, erlang, element, [3, DynAfterState]},
        top = {call, erlang, element, [4, DynAfterState]}
    };

%%--------------------------------------------------------------------
%% next_state: push
%%--------------------------------------------------------------------
next_state(PreState, Result, {call, ?MODULE, push, [O]}) ->
    DynAfterState = {call, erlang, element, [3, Result]},
    PreState#ts { 
        isEmpty = {call, erlang, element, [2, DynAfterState]},
        size = {call, erlang, element, [3, DynAfterState]},
        top = {call, erlang, element, [4, DynAfterState]}
    };

%%--------------------------------------------------------------------
%% next_state: top
%%--------------------------------------------------------------------
next_state(PreState, Result, {call, ?MODULE, top, []}) ->
    DynAfterState = {call, erlang, element, [3, Result]},
    PreState#ts { 
        isEmpty = {call, erlang, element, [2, DynAfterState]},
        size = {call, erlang, element, [3, DynAfterState]},
        top = {call, erlang, element, [4, DynAfterState]}
    };

%%--------------------------------------------------------------------
%% next_state: size
%%--------------------------------------------------------------------
next_state(PreState, Result, {call, ?MODULE, size, []}) ->
    DynAfterState = {call, erlang, element, [3, Result]},
    PreState#ts { 
        isEmpty = {call, erlang, element, [2, DynAfterState]},
        size = {call, erlang, element, [3, DynAfterState]},
        top = {call, erlang, element, [4, DynAfterState]}
    };

%%--------------------------------------------------------------------
%% next_state: isEmpty
%%--------------------------------------------------------------------
next_state(PreState, Result, {call, ?MODULE, isEmpty, []}) ->
    DynAfterState = {call, erlang, element, [3, Result]},
    PreState#ts { 
        isEmpty = {call, erlang, element, [2, DynAfterState]},
        size = {call, erlang, element, [3, DynAfterState]},
        top = {call, erlang, element, [4, DynAfterState]}
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
%% pop: postconditions
%%--------------------------------------------------------------------    
postcondition(PreState, AfterState, {call, ?MODULE, pop, []}, {Result, DynPreState, DynAfterState})->
    ((Result == DynPreState#ts.top) andalso (AfterState#ts.size == (DynPreState#ts.size - 1)));

%%--------------------------------------------------------------------
%% push: postconditions
%%--------------------------------------------------------------------    
postcondition(PreState, AfterState, {call, ?MODULE, push, [O]}, {Result, DynPreState, DynAfterState})->
    (AfterState#ts.top == O);

%%--------------------------------------------------------------------
%% top: postconditions
%%--------------------------------------------------------------------
postcondition(PreState, AfterState, {call, ?MODULE, top, []}, {Result, DynPreState, DynAfterState})->
    true;

%%--------------------------------------------------------------------
%% size: postconditions
%%--------------------------------------------------------------------
postcondition(PreState, AfterState, {call, ?MODULE, size, []}, {Result, DynPreState, DynAfterState})->
    true;

%%--------------------------------------------------------------------
%% isEmpty: postconditions
%%--------------------------------------------------------------------
postcondition(PreState, AfterState, {call, ?MODULE, isEmpty, []}, {Result, DynPreState, DynAfterState})->
    true;

%%--------------------------------------------------------------------
%% postconditions: default
%%--------------------------------------------------------------------
postcondition(_PreState, _AfterState, _C, _R) ->
    false.

%%====================================================================
%% generators
%%====================================================================

%%====================================================================
%% wrappers
%%====================================================================

pop()->
    DynPreState = #ts {
        size = stack:size(), 
        top = stack:top()
    }, 
    Result = stack:pop(),
    DynAfterState = #ts {
        isEmpty = stack:isEmpty(), 
        size = stack:size()
    }, 
    {Result, DynPreState, DynAfterState}.

push(O)->
    DynPreState = #ts {
        
    }, 
    Result = stack:push(O),
    DynAfterState = #ts {
        isEmpty = stack:isEmpty(), 
        top = stack:top()
    }, 
    {Result, DynPreState, DynAfterState}.

top()->
    DynPreState = #ts {
        
    }, 
    Result = stack:top(),
    DynAfterState = #ts {
        isEmpty = stack:isEmpty()
    }, 
    {Result, DynPreState, DynAfterState}.

size()->
    DynPreState = #ts {
        
    }, 
    Result = stack:size(),
    DynAfterState = #ts {
        isEmpty = stack:isEmpty()
    }, 
    {Result, DynPreState, DynAfterState}.

isEmpty()->
    DynPreState = #ts {
        
    }, 
    Result = stack:isEmpty(),
    DynAfterState = #ts {
        isEmpty = stack:isEmpty()
    }, 
    {Result, DynPreState, DynAfterState}.
