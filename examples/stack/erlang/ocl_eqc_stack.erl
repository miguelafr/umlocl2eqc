-module(ocl_eqc_stack).

-include_lib("eqc/include/eqc.hrl").
-include_lib("eqc/include/eqc_statem.hrl").

-record(testing_state, {top, size, isEmpty}).

%% API
-export([prop/0]).

%% EQC callbacks
-export([initial_state/0, command/1, precondition/2, postcondition/3,
        next_state/3, dynamic_precondition/2]).

%% Wrapper functions
-export([pop/0, push/1, top/0, size/0, isEmpty/0]).

prop() ->
    ?FORALL(Cmds, commands(?MODULE),
        begin
            stack:init(), 
            {_H, _S, Res} = run_commands(?MODULE, Cmds),
            %?WHENFAIL(io:format("H: ~p\nS: ~p\nRes: ~p\n", [H, S, Res]), Res == ok)
            stack:init(), 
            Res == ok
        end).

%%====================================================================
%% Callbacks
%%====================================================================
%%--------------------------------------------------------------------
%% command
%%--------------------------------------------------------------------
command(_S) ->
    oneof(
        %pop
        [{call, ?MODULE, pop, []}]

        ++

        %push
        [{call, ?MODULE, push, [eqc_gen:int()]}]

        ++

        %top
        [{call, ?MODULE, top, []}]

        ++

        %size
        [{call, ?MODULE, size, []}]

		++

		%isEmpty
        [{call, ?MODULE, isEmpty, []}]

    ).

%%--------------------------------------------------------------------
%% initial_state
%%--------------------------------------------------------------------
initial_state()->
    #testing_state {}.

%%--------------------------------------------------------------------
%% precondition
%%--------------------------------------------------------------------
precondition(_S, _C)->
    true.

%%--------------------------------------------------------------------
%% dynamic_precondition
%%--------------------------------------------------------------------
%context Stack::pop(): Object
%pre notEmpty: isEmpty() = false
%post topElementReturned: result = self@pre.top()
%post elementRemoved: size() = self@pre.size() - 1
dynamic_precondition(S, {call, ?MODULE, pop, []}) ->
    S#testing_state.isEmpty == false;

%context Stack::push(anObject: Object): void
%post pushedObjectIsOnTop: top() = anObject
dynamic_precondition(_S, {call, ?MODULE, push, [_X]}) ->
    true;

%context Stack::top(): Object
%pre notEmpty: isEmpty() = false
dynamic_precondition(S, {call, ?MODULE, top, []}) ->
    S#testing_state.isEmpty == false;

dynamic_precondition(_S, {call, ?MODULE, size, []}) ->
    true;

dynamic_precondition(_S, {call, ?MODULE, isEmpty, []}) ->
    true;

dynamic_precondition(_S, _C)->
    false.

%%--------------------------------------------------------------------
%% next_state
%%--------------------------------------------------------------------   
next_state(S, R, _C) ->
    DynAfterState = {call, erlang, element, [3, R]},
    Top = {call, erlang, element, [2, DynAfterState]},
    Size = {call, erlang, element, [3, DynAfterState]},
    IsEmpty = {call, erlang, element, [4, DynAfterState]},
    S1 = S#testing_state {
        top = Top
    },
    S2 = S1#testing_state {
        size = Size
    },
    S3 = S2#testing_state {
        isEmpty = IsEmpty
    },
    S3.

%%--------------------------------------------------------------------
%% postcondition
%%--------------------------------------------------------------------
postcondition(S, C, R)->
    AfterState = eqc_symbolic:eval(next_state(S, R, C)),
    postcondition(S, AfterState, C, R).
    
%%--------------------------------------------------------------------
%% postcondition
%%--------------------------------------------------------------------
%context Stack::pop(): Object
%pre notEmpty: isEmpty() = false
%post topElementReturned: result = self@pre.top()
%post elementRemoved: size() = self@pre.size() - 1
postcondition(_PreState, AfterState, {call, ?MODULE, pop, []}, {R, DynPreState, _DynAfterState}) ->
    (DynPreState#testing_state.top == R) andalso
        (AfterState#testing_state.size == DynPreState#testing_state.size - 1);

%context Stack::push(anObject: Object): void
%post pushedObjectIsOnTop: top() = anObject
postcondition(_PreState, AfterState, {call, ?MODULE, push, [X]}, {_R, _DynPreState, _DynAfterState}) ->
    AfterState#testing_state.top == X;

%context Stack::top(): Object
%pre notEmpty: isEmpty() = false
postcondition(_PreState, _AfterState, {call, ?MODULE, top, []}, {_R, _DynPreState, _DynAfterState}) ->
    true;

postcondition(_PreState, _AfterState, {call, ?MODULE, size, []}, {_R, _DynPreState, _DynAfterState}) ->
    true;

postcondition(_PreState, _AfterState, {call, ?MODULE, isEmpty, []}, {_R, _DynPreState, _DynAfterState}) ->
    true;

postcondition(_PreState, _AfterState, _C, _R)->
    false.

%%====================================================================
%% Wrappers
%%====================================================================
pop()->
    DynPreState = #testing_state {
        top = stack:top(),
        size = stack:size()
    },
    R = stack:pop(),
    DynAfterState = #testing_state {
        size = stack:size(),
        isEmpty = stack:isEmpty()
    },
    {R, DynPreState, DynAfterState}.

push(X)->
    DynPreState = #testing_state {},
    R = stack:push(X),
    DynAfterState = #testing_state {
        top = stack:top(),
        isEmpty = stack:isEmpty()
    },
    {R, DynPreState, DynAfterState}.

top()->
    DynPreState = #testing_state {},
    R = stack:top(),
    DynAfterState = #testing_state {
        isEmpty = stack:isEmpty()
    },
    {R, DynPreState, DynAfterState}.

size()->
    DynPreState = #testing_state {},
    R = stack:size(),
    DynAfterState = #testing_state {
        isEmpty = stack:isEmpty()
    },
    {R, DynPreState, DynAfterState}.

isEmpty()->
    DynPreState = #testing_state {},
    R = stack:isEmpty(),
    DynAfterState = #testing_state {
        isEmpty = stack:isEmpty()
    },
    {R, DynPreState, DynAfterState}.
