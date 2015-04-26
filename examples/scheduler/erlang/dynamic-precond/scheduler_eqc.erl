-module(scheduler_eqc).

-include_lib("eqc/include/eqc.hrl").
-include_lib("eqc/include/eqc_statem.hrl").

-compile(export_all).

-record(state, {active  = [],
		ready   = [],
		waiting = []}).

prop_scheduler() ->
    ?FORALL(Cmds, commands(?MODULE),
	    begin 
		scheduler:start(),
		{H, S, Res} = run_commands(?MODULE, Cmds),
		scheduler:stop(),
		[ P ! stop || P <- S#state.active],
		[ P ! stop || P <- S#state.ready],
		[ P ! stop || P <- S#state.waiting],
		?WHENFAIL(io:format("H: ~p\nS: ~p\nRes: ~p\n", [H, S, Res]),
			  Res == ok)
	    end).

%%====================================================================
%% Callbacks
%%====================================================================
%%--------------------------------------------------------------------
%% command
%%--------------------------------------------------------------------
command(S) ->
    oneof([{call, ?MODULE, init, []}] ++
	  [{call, ?MODULE, new, [process()]}] ++
          [{call, ?MODULE, ready, [oneof(S#state.waiting)]} || S#state.waiting /= []] ++
          [{call, ?MODULE, swap, []}]).

%%--------------------------------------------------------------------
%% initial_state
%%--------------------------------------------------------------------
initial_state()->
    #state{active  = [],
	   ready   = [],
	   waiting = []}.

%%--------------------------------------------------------------------
%% precondition
%%--------------------------------------------------------------------
precondition(_S, _C)->
    true.

% pre: p <> active and not (ready->union(waiting))->includes(p)
dynamic_precondition(S, {call, ?MODULE, new, [P]}) ->
    (not lists:member(P, S#state.active)) andalso
	(not lists:member(P, lists:usort(S#state.ready ++ S#state.waiting)));
% pre: waiting->includes(p)
dynamic_precondition(S, {call, ?MODULE, ready, [P]}) ->
    lists:member(P, S#state.waiting);
% pre: active <> null
dynamic_precondition(S, {call, ?MODULE, swap, []}) ->
    not (S#state.active == []);
dynamic_precondition(_, _) ->
    true.


%%--------------------------------------------------------------------
%% next_state
%%--------------------------------------------------------------------
next_state(_S, _R, {call, ?MODULE, init, []}) ->
    #state{active = [],
	    ready = [],
	    waiting = []};
next_state(S, _R, {call, ?MODULE, new, [P]}) ->
    S#state{waiting = lists:usort([P | S#state.waiting])};
next_state(S, _R, {call, ?MODULE, ready, [P]}) ->
    case S#state.active of
        [] ->
            S#state{active = [P],
		    waiting = lists:delete(P, S#state.waiting)};
        _ ->
            S#state{ready = lists:usort([P | S#state.ready]),
		    waiting = lists:delete(P, S#state.waiting)}
    end;
next_state(S, R, {call, ?MODULE, swap, []}) ->
    case S#state.ready of
        [] ->
            S#state{active = [],
		    ready =  [],
		    waiting = lists:usort(S#state.active ++ S#state.waiting)};
        _ ->
            S#state {active = lists:flatten([R]),
		     ready = lists:delete(R, S#state.ready),
		     waiting = lists:usort(S#state.active ++ S#state.waiting)}
    end;
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
    (lists:usort(AfterState#state.ready ++ AfterState#state.waiting) == [])
	andalso (AfterState#state.active == [])
	andalso invariant(PreState, AfterState);
% post: waiting = (waiting@pre->including(p)) and
%               ready = ready@pre and active = active@pre
postcondition(PreState, AfterState, {call, ?MODULE, new, [P]}, _R) ->
    (AfterState#state.waiting == lists:usort([P | PreState#state.waiting]))
	andalso (AfterState#state.ready == PreState#state.ready)
	andalso (AfterState#state.active ==  PreState#state.active)
	andalso invariant(PreState, AfterState);
% post: waiting = waiting@pre->excluding(p) and
%               if active@pre = null then
%                       (ready = ready@pre and active = p)
%               else
%                       (ready = ready@pre->including(p) and active = active@pre)
%               endif
postcondition(PreState, AfterState, {call, ?MODULE, ready, [P]}, _R) ->
    (AfterState#state.waiting == lists:usort(PreState#state.waiting -- [P]))
	andalso (case PreState#state.active of
		     [] -> (AfterState#state.ready == PreState#state.ready) andalso (AfterState#state.active == [P]);
		     _  -> (AfterState#state.ready == lists:usort([P | PreState#state.ready]))
			       andalso (AfterState#state.active == PreState#state.active)
		 end)
	andalso invariant(PreState, AfterState);
% post: if ready@pre->isEmpty() then
%                       (active = null and ready = Set{})
%               else (
%                       ready@pre->includes(active) and
%                                       ready = ready@pre->excluding(active))
%               endif
%               and waiting = waiting@pre->including(active@pre)
postcondition(PreState, AfterState, {call, ?MODULE, swap, []}, _R) ->
    case PreState#state.ready of
	[] -> (AfterState#state.active == []) andalso (AfterState#state.ready == []);
	_  -> (lists:usort(PreState#state.ready -- AfterState#state.active) /= PreState#state.ready)
		  andalso (lists:usort(PreState#state.ready -- AfterState#state.active) == AfterState#state.ready)
    end andalso (AfterState#state.waiting == lists:usort(PreState#state.waiting ++ PreState#state.active))
	andalso invariant(PreState, AfterState);
postcondition(_PreState, _AfterState, _C, _R)->
    false.

%%====================================================================
%% Invariant
%%====================================================================
% inv: (ready->intersection(waiting))->isEmpty() and
%		not ((ready->union(waiting))->includes(active)) and
%		(active = null implies ready->isEmpty())
invariant(PreState, AfterState) ->
    ([P || P <- AfterState#state.ready,
	   lists:member(P, AfterState#state.waiting)] == [])
	andalso (lists:usort(lists:usort(AfterState#state.ready++AfterState#state.waiting)--AfterState#state.active)
		 == lists:usort(AfterState#state.ready++AfterState#state.waiting))
	andalso ((not (AfterState#state.active == [])) orelse (AfterState#state.ready == [])).

%%====================================================================
%% Generators
%%====================================================================
process()->
    spawn(?MODULE, process, [[]]).

%%
%%
%%
process(State) ->
    receive
	stop ->
	    ok;
	_Else -> process(State)
    end.
    
%%====================================================================
%% Wrappers
%%====================================================================
init()->
    scheduler:init(). % no hace nada

new(P)->
    scheduler:new(P). % pone P en waiting

ready(P)->
    scheduler:ready(P). % si no hay activos, quita P de waiting y lo pone en active
                        % si hay activos,    quita P de waiting y lo pone en ready

swap()->
    % este método devuelve ok!!
    scheduler:swap(), % si no hay ready, pone el activo a waiting y deja activo vacío
                      % si hay ready,    pone el activo a waiting y pone *un* ready en activo
    case scheduler:get_active() of
	undefined ->  [];
	Else      ->  Else
    end.
