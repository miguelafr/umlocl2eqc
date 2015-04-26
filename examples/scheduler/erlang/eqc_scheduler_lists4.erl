-module(eqc_scheduler_lists4).

-include_lib("eqc/include/eqc.hrl").
-include_lib("eqc/include/eqc_statem.hrl").

-record(testing_state, {
	    init    = false,
        active  = undefined,
        ready   = [],
        waiting = []
    }).

%% API
-export([prop_scheduler/0]).

%% EQC callbacks
-export([initial_state/0, command/1, precondition/2, postcondition/3, next_state/3]).

%% Wrapper functions
-export([init/0, new/1, ready/1, swap/0]).

prop_scheduler() ->
    ?FORALL(Cmds, commands(?MODULE),
        begin 
            scheduler:start(),
            {H, S, Res} = run_commands(?MODULE, Cmds),
            scheduler:stop(),
	        %?WHENFAIL(io:format("H: ~p\nS: ~p\nRes: ~p\n", [H, S, Res]),
                Res == ok%)
        end).

%%====================================================================
%% Callbacks
%%====================================================================
%%--------------------------------------------------------------------
%% command
%%--------------------------------------------------------------------
command(S) ->
    oneof(
        %init
        [{call, ?MODULE, init, []}                                || S#testing_state.init == false] ++
        %new
        [{call, ?MODULE, new, [gen_process()]}                    || S#testing_state.init == true] ++
        %ready
        [{call, ?MODULE, ready, [oneof(S#testing_state.waiting)]} || S#testing_state.init == true, S#testing_state.waiting /= []] ++
        %swap
        [{call, ?MODULE, swap, []}                                || S#testing_state.init == true]
    ).

%%--------------------------------------------------------------------
%% initial_state
%%--------------------------------------------------------------------
initial_state()->
    #testing_state {
        init    = false,
        active  = undefined,
        ready   = [],
        waiting = []
    }.

%%--------------------------------------------------------------------
%% precondition
%%--------------------------------------------------------------------
% true
precondition(_S, {call, ?MODULE, init, []}) ->
    true;

% pre: p <> active and not (ready->union(waiting))->includes(p)
precondition(S, {call, ?MODULE, new, [P]}) ->
    (P /= S#testing_state.active)
	andalso (not is_member(P, S#testing_state.ready))
	andalso (not is_member(P, S#testing_state.waiting));

% pre: waiting->includes(p)
precondition(S, {call, ?MODULE, ready, [P]}) ->
    is_member(P, S#testing_state.waiting);

% pre: active <> null
precondition(S, {call, ?MODULE, swap, []}) ->
    S#testing_state.active /= undefined;

precondition(_S, _C)->
    false.

%%--------------------------------------------------------------------
%% next_state
%%--------------------------------------------------------------------
next_state(_S, _R, {call, ?MODULE, init, []}) ->
    #testing_state {
	    init    = true,
        active  = undefined,
        ready   = [],
        waiting = []
    };

next_state(S, R, {call, ?MODULE, new, [_P]}) ->
    S#testing_state {
        active  = {call, erlang, element, [1, R]},
        ready   = {call, erlang, element, [2, R]},
        waiting = {call, erlang, element, [3, R]}
    };

next_state(S, R, {call, ?MODULE, ready, [_P]}) ->
    S#testing_state {
        active  = {call, erlang, element, [1, R]},
        ready   = {call, erlang, element, [2, R]},
        waiting = {call, erlang, element, [3, R]}
    };

next_state(S, R, {call, ?MODULE, swap, []}) ->
    S#testing_state {
        active  = {call, erlang, element, [1, R]},
        ready   = {call, erlang, element, [2, R]},
        waiting = {call, erlang, element, [3, R]}
    };

next_state(S, _R, _C) ->
    S.

%%--------------------------------------------------------------------
%% postcondition
%%--------------------------------------------------------------------
postcondition(S, C, R)->
    AfterState = next_state(S, R, C),
    postcondition(S, AfterState, C, R).

% post: (ready->union(waiting)) = Set{} and active = null
postcondition(PreState, AfterState, {call, ?MODULE, init, []}, _R) ->
    Active = AfterState#testing_state.active,
    Ready = AfterState#testing_state.ready,
    Waiting = AfterState#testing_state.waiting,
    (is_empty(Ready))
	    andalso (is_empty(Waiting))
	    andalso (Active == undefined)
	    andalso invariant(PreState, AfterState);

% post: waiting = (waiting@pre->including(p)) and
% 		ready = ready@pre and active = active@pre
postcondition(PreState, AfterState, {call, ?MODULE, new, [P]}, _R) ->
    PreActive = PreState#testing_state.active,
    PreReady = PreState#testing_state.ready,
    PreWaiting = PreState#testing_state.waiting,

    Active = AfterState#testing_state.active,
    Ready = AfterState#testing_state.ready,
    Waiting = AfterState#testing_state.waiting,

    eq(Waiting, add_element(P, PreWaiting))
	    andalso eq(Ready, PreReady)
	    andalso (Active == PreActive)
	    andalso invariant(PreState, AfterState);

% post: waiting = waiting@pre->excluding(p) and
%		if active@pre = null then
%			(ready = ready@pre and active = p)
%		else
%			(ready = ready@pre->including(p) and active = active@pre)
%		endif
postcondition(PreState, AfterState, {call, ?MODULE, ready, [P]}, _R) ->
    PreActive = PreState#testing_state.active,
    PreReady = PreState#testing_state.ready,
    PreWaiting = PreState#testing_state.waiting,

    Active = AfterState#testing_state.active,
    Ready = AfterState#testing_state.ready,
    Waiting = AfterState#testing_state.waiting,

    eq(Waiting, del_element(P, PreWaiting))
	    andalso (case PreActive of
		         undefined ->
			        eq(Ready, PreReady) andalso (Active == P);
		         _ ->
			        eq(Ready, add_element(P, PreReady))
                        andalso (Active == PreActive)
		     end)
	    andalso invariant(PreState, AfterState);

% post: if ready@pre->isEmpty() then
%			(active = null and ready = Set{})
%		else (
%			ready@pre->includes(active) and
%					ready = ready@pre->excluding(active))
%		endif
%		and waiting = waiting@pre->including(active@pre)
postcondition(PreState, AfterState, {call, ?MODULE, swap, []}, _R) ->
    PreActive = PreState#testing_state.active,
    PreReady = PreState#testing_state.ready,
    PreWaiting = PreState#testing_state.waiting,

    Active = AfterState#testing_state.active,
    Ready = AfterState#testing_state.ready,
    Waiting = AfterState#testing_state.waiting,

    case is_empty(PreReady) of
        true ->
            (Active == undefined) andalso (is_empty(Ready));
        false ->
            is_member(Active, PreReady)
                andalso eq(Ready, del_element(Active, PreReady))
    end
	    andalso eq(Waiting, add_element(PreActive, PreWaiting))
	    andalso invariant(PreState, AfterState);

postcondition(_PreState, _AfterState, _C, _R)->
    false.

%%====================================================================
%% Invariant
%%====================================================================
% inv: (ready->intersection(waiting))->isEmpty() and
%		not ((ready->union(waiting))->includes(active)) and
%		(active = null implies ready->isEmpty())
invariant(_PreState, AfterState) ->
    Active = AfterState#testing_state.active,
    Ready = AfterState#testing_state.ready,
    Waiting = AfterState#testing_state.waiting,

    is_empty(intersection(Ready, Waiting))
	    andalso (not is_member(Active, union(Ready, Waiting)))
	    andalso ((Active /= undefined) orelse (is_empty(Ready))).

%%====================================================================
%% Generators
%%====================================================================
gen_process()->
    ?LET(X, int(), process:new(X)).

%%====================================================================
%% Wrappers
%%====================================================================
init()->
    scheduler:init(),
    {scheduler:get_active(),
     sets:to_list(scheduler:get_ready()),
     sets:to_list(scheduler:get_waiting())}.

new(P)->
    scheduler:new(P),
    {scheduler:get_active(),
     sets:to_list(scheduler:get_ready()),
     sets:to_list(scheduler:get_waiting())}.

ready(P)->
    scheduler:ready(P),
    {scheduler:get_active(),
     sets:to_list(scheduler:get_ready()),
     sets:to_list(scheduler:get_waiting())}.

swap()->
    scheduler:swap(),
    {scheduler:get_active(),
     sets:to_list(scheduler:get_ready()),
     sets:to_list(scheduler:get_waiting())}.

%%====================================================================
%% Utilities
%%====================================================================
eq(X, Y)->
    sets:from_list(X) == sets:from_list(Y).

is_empty(X)->
    X == [].

intersection(X, Y)->
    sets:to_list(sets:intersection(sets:from_list(X), sets:from_list(Y))).

union(X, Y)->
    sets:to_list(sets:union(sets:from_list(X), sets:from_list(Y))).

add_element(X, L) ->
    sets:to_list(sets:add_element(X, sets:from_list(L))).

del_element(X, L) ->
    lists:delete(X, L).

is_member(X, L) ->
    lists:member(X, L).
