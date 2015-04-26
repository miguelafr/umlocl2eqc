-module(eqc_scheduler_lists_static).

-include_lib("eqc/include/eqc.hrl").
-include_lib("eqc/include/eqc_statem.hrl").

-behaviour(eqc_statem).

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

    % Static test cases
    C=[{set,{var,1},{call,?MODULE,init,[]}},
         {set,{var,2},{call,?MODULE,new,['<0.12629.0>']}},
         {set,{var,3},{call,?MODULE,new,['<0.12630.0>']}},
         {set,{var,4},{call,?MODULE,new,['<0.12631.0>']}},
         {set,{var,5},{call,?MODULE,ready,['<0.12630.0>']}},
         {set,{var,6},{call,?MODULE,new,['<0.12632.0>']}},
         {set,{var,7},{call,?MODULE,ready,['<0.12631.0>']}},
         {set,{var,8},{call,?MODULE,swap,[]}},
         {set,{var,9},{call,?MODULE,new,['<0.12633.0>']}},
         {set,{var,10},{call,?MODULE,new,['<0.12634.0>']}},
         {set,{var,11},{call,?MODULE,new,['<0.12635.0>']}},
         {set,{var,12},{call,?MODULE,swap,[]}},
         {set,{var,13},{call,?MODULE,ready,['<0.12634.0>']}},
         {set,{var,14},{call,?MODULE,new,['<0.12636.0>']}}],

    %C=[{set,{var,1},{call,?MODULE,init,[]}},
    %     {set,{var,2},{call,?MODULE,new,[gen_process()]}},
    %     {set,{var,3},{call,?MODULE,new,[gen_process()]}},
    %     {set,{var,4},{call,?MODULE,new,[gen_process()]}},
    %     {set,{var,5},{call,?MODULE,ready,[gen_process()]}},
    %     {set,{var,6},{call,?MODULE,new,[gen_process()]}},
    %     {set,{var,7},{call,?MODULE,ready,[gen_process()]}},
    %     {set,{var,8},{call,?MODULE,swap,[]}},
    %     {set,{var,9},{call,?MODULE,new,[gen_process()]}},
    %     {set,{var,10},{call,?MODULE,new,[gen_process()]}},
    %     {set,{var,11},{call,?MODULE,new,[gen_process()]}},
    %     {set,{var,12},{call,?MODULE,swap,[]}},
    %     {set,{var,13},{call,?MODULE,ready,[gen_process()]}},
    %     {set,{var,14},{call,?MODULE,new,[gen_process()]}}],

    %?FORALL(Cmds, commands(?MODULE),
    ?FORALL(Cmds, C,
        begin 
            scheduler:start(),
            %{H, S, Res} = run_commands(?MODULE, Cmds),
            {H, S, Res} = x(?MODULE, Cmds),
            scheduler:stop(),
	        ?WHENFAIL(io:format("H: ~p\nS: ~p\nRes: ~p\n", [H, S, Res]),
                aggregate(command_names(Cmds), Res==ok))
        end).

x(EqcMod, Cmds) ->
    x(EqcMod, Cmds, {[], initial_state(), ok}).

x(EqcMod, [{set, {var, _V}, F} | MoreCmds], {H, S, R})->
    case EqcMod:precondition(S, F) of
        true ->
            {call, Module, Function, Args} = F,
            Result = erlang:apply(Module, Function, Args),
            case Module:postcondition(S, F, Result) of
                true ->
                    NextState = EqcMod:next_state(S, Result, F),
                    x(EqcMod, MoreCmds, {[{S, Result} | H], NextState, ok});
                false ->
                    x(EqcMod, [], {[{S, Result} | H], S, {postcondition, false}})
            end;
        false ->
            x(EqcMod, MoreCmds, {H, S, R})
    end;

x(_EqcMod, [], {H, S, R})->
    {lists:reverse(H), S, R}.

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

next_state(S, _R, {call, ?MODULE, new, [P]}) ->
    S#testing_state {
        waiting = add_element(P, S#testing_state.waiting)
    };

next_state(S, _R, {call, ?MODULE, ready, [P]}) ->
    case S#testing_state.active of
        undefined ->
            S#testing_state {
                active  = P,
                waiting = del_element(P, S#testing_state.waiting)
            };
        _ ->
            S#testing_state {
                ready   = add_element(P, S#testing_state.ready),
                waiting = del_element(P, S#testing_state.waiting)
            }
    end;

next_state(S, R, {call, ?MODULE, swap, []}) ->
    case is_empty(S#testing_state.ready) of
        true ->
            S#testing_state {
                active = undefined,
                waiting = add_element(S#testing_state.active, S#testing_state.waiting)
            };
        false ->
            %% TODO We don't know which will be the next "active" process,
            %% we need to use the symbolic value from the returned value "R".
            %NewActive = lists:last(S#testing_state.ready),
            %NewActive = {call, erlang, element, [2, R]},
            NewActive = erlang:element(2, R),
            S#testing_state {
                active = NewActive, 
                ready = del_element(NewActive, S#testing_state.ready),
                waiting = add_element(S#testing_state.active, S#testing_state.waiting)
            }
    end;

next_state(S, _R, _C) ->
    S.

%%--------------------------------------------------------------------
%% postcondition
%%--------------------------------------------------------------------

% post: (ready->union(waiting)) = Set{} and active = null
postcondition(S, {call, ?MODULE, init, []}, {R, Active, Ready, Waiting}) ->
    (is_empty(Ready))
	    andalso (is_empty(Waiting))
	    andalso (Active == undefined)
	    andalso invariant(S, {R, Active, Ready, Waiting});

% post: waiting = (waiting@pre->including(p)) and
% 		ready = ready@pre and active = active@pre
postcondition(S, {call, ?MODULE, new, [P]}, {R, Active, Ready, Waiting}) ->
    eq(Waiting, add_element(P, S#testing_state.waiting))
	    andalso eq(Ready, S#testing_state.ready)
	    andalso (Active == S#testing_state.active)
	    andalso invariant(S, {R, Active, Ready, Waiting});

% post: waiting = waiting@pre->excluding(p) and
%		if active@pre = null then
%			(ready = ready@pre and active = p)
%		else
%			(ready = ready@pre->including(p) and active = active@pre)
%		endif
postcondition(S, {call, ?MODULE, ready, [P]}, {R, Active, Ready, Waiting}) ->
    eq(Waiting, del_element(P, S#testing_state.waiting))
	    andalso (case S#testing_state.active of
		         undefined ->
			        eq(Ready, S#testing_state.ready) andalso (Active == P);
		         _ ->
			        eq(Ready, add_element(P, S#testing_state.ready))
                        andalso (Active == S#testing_state.active)
		     end)
	    andalso invariant(S, {R, Active, Ready, Waiting});

% post: if ready@pre->isEmpty() then
%			(active = null and ready = Set{})
%		else (
%			ready@pre->includes(active) and
%					ready = ready@pre->excluding(active))
%		endif
%		and waiting = waiting@pre->including(active@pre)
postcondition(S, {call, ?MODULE, swap, []}, {R, Active, Ready, Waiting}) ->
    case is_empty(S#testing_state.ready) of
        true ->
            (Active == undefined) andalso (is_empty(Ready));
        false ->
            is_member(Active, S#testing_state.ready)
                andalso eq(Ready, del_element(Active, S#testing_state.ready))
    end
	    andalso eq(Waiting, add_element(S#testing_state.active, S#testing_state.waiting))
	    andalso invariant(S, {R, Active, Ready, Waiting});

postcondition(_S, _C, _R)->
    false.

%%====================================================================
%% Invariant
%%====================================================================
% inv: (ready->intersection(waiting))->isEmpty() and
%		not ((ready->union(waiting))->includes(active)) and
%		(active = null implies ready->isEmpty())
invariant(_S, {_R, Active, Ready, Waiting}) ->
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
    R = scheduler:init(),
    {R, scheduler:get_active(),
        sets:to_list(scheduler:get_ready()),
        sets:to_list(scheduler:get_waiting())}.

new(P)->
    R = scheduler:new(P),
    {R, scheduler:get_active(),
        sets:to_list(scheduler:get_ready()),
        sets:to_list(scheduler:get_waiting())}.

ready(P)->
    R = scheduler:ready(P),
    {R, scheduler:get_active(),
        sets:to_list(scheduler:get_ready()),
        sets:to_list(scheduler:get_waiting())}.

swap()->
    R = scheduler:swap(),
    {R, scheduler:get_active(),
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
