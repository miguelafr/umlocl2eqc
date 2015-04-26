-module(eqc_scheduler).

-include_lib("eqc/include/eqc.hrl").
-include_lib("eqc/include/eqc_statem.hrl").

-record(testing_state, {
        active = undefined,
        ready = sets:new(),
        waiting = sets:new()
    }).

%% API
-export([prop_scheduler/0]).

%% EQC callbacks
-export([initial_state/0, command/1, precondition/2, postcondition/3,
        next_state/3]).

%% Wrapper functions
-export([init/0, new/1, ready/1, swap/0]).

prop_scheduler() ->
    ?FORALL(Cmds, commands(?MODULE),
        begin 
            scheduler:start(),
            {_H, _S, Res} = run_commands(?MODULE, Cmds),
            scheduler:stop(),
            %?WHENFAIL(io:format("H: ~p\nS: ~p\nRes: ~p\n", [H, S, Res]), Res == ok)
            Res == ok
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
        [{call, ?MODULE, init, []}]

        ++

        %new
        [{call, ?MODULE, new, [gen_process()]}]

        ++

        %ready
        [{call, ?MODULE, ready, [eqc_gen:elements(sets:to_list(S#testing_state.waiting))]} ||
                sets:to_list(S#testing_state.waiting) /= []]
            
        ++
                
        %swap
        [{call, ?MODULE, swap, []}]

    ).

%%--------------------------------------------------------------------
%% initial_state
%%--------------------------------------------------------------------
initial_state()->
    #testing_state {
        active = undefined,
        ready = sets:new(),
        waiting = sets:new()
    }.

%%--------------------------------------------------------------------
%% precondition
%%--------------------------------------------------------------------
% true
precondition(_S, {call, ?MODULE, init, []}) ->
    true;

% pre: p <> active and not (ready->union(waiting))->includes(p)
precondition(S, {call, ?MODULE, new, [P]}) ->
    (P /= S#testing_state.active) and
        not sets:is_element(P, sets:union(S#testing_state.ready, S#testing_state.waiting));

% pre: waiting->includes(p)
precondition(S, {call, ?MODULE, ready, [P]}) ->
    sets:is_element(P, S#testing_state.waiting);

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
        active = undefined,
        ready = sets:new(),
        waiting = sets:new()
    };

next_state(S, _R, {call, ?MODULE, new, [P]}) ->
    S#testing_state {
        active = S#testing_state.active,
        ready = S#testing_state.ready,
        waiting = sets:add_element(P, S#testing_state.waiting)
    };

next_state(S, _R, {call, ?MODULE, ready, [P]}) ->
    case S#testing_state.active of
        undefined ->
            S#testing_state {
                active = P,
                ready = S#testing_state.ready,
                waiting = sets:del_element(P, S#testing_state.waiting)
            };
        _ ->
            S#testing_state {
                active = S#testing_state.active,
                ready = sets:add_element(P, S#testing_state.ready),
                waiting = sets:del_element(P, S#testing_state.waiting)
            }
    end;

next_state(S, R, {call, ?MODULE, swap, []}) ->
    case sets:size(S#testing_state.ready) of
        0 ->
            S#testing_state {
                active = undefined,
                ready = sets:new(),
                waiting = sets:add_element(S#testing_state.active, S#testing_state.waiting)
            };
        _ ->
            NewActive = {call, erlang, element, [2, R]}, %lists:last(sets:to_list(S#testing_state.ready)),
            S#testing_state {
                active = NewActive, 
                ready = sets:del_element(NewActive, S#testing_state.ready),
                waiting = sets:add_element(S#testing_state.active, S#testing_state.waiting)
            }
    end;
        
next_state(S, _R, _C) ->
    S.

%%--------------------------------------------------------------------
%% postcondition
%%--------------------------------------------------------------------

% post: (ready->union(waiting)) = Set{} and active = null
postcondition(S, {call, ?MODULE, init, []}, {R, Active, Ready, Waiting}) ->
    (sets:union(Ready, Waiting) == sets:new()) and
        (Active == undefined) and
    invariant(S, {R, Active, Ready, Waiting});

% post: waiting = (waiting@pre->including(p)) and
% 		ready = ready@pre and active = active@pre
postcondition(S, {call, ?MODULE, new, [P]}, {R, Active, Ready, Waiting}) ->
    (Waiting == sets:add_element(P, S#testing_state.waiting)) and
        (Ready == S#testing_state.ready) and
        (Active == S#testing_state.active) and
    invariant(S, {R, Active, Ready, Waiting});

% post: waiting = waiting@pre->excluding(p) and
%		if active@pre = null then
%			(ready = ready@pre and active = p)
%		else
%			(ready = ready@pre->including(p) and active = active@pre)
%		endif
postcondition(S, {call, ?MODULE, ready, [P]}, {R, Active, Ready, Waiting}) ->
    (Waiting == sets:del_element(P, S#testing_state.waiting)) and
    case S#testing_state.active of
        undefined ->
            (Ready == S#testing_state.ready) and
                    (Active == P);
        _ ->
            (Ready == sets:add_element(P, S#testing_state.ready)) and
                    (Active == S#testing_state.active)
    end and
    invariant(S, {R, Active, Ready, Waiting});

% post: if ready@pre->isEmpty() then
%			(active = null and ready = Set{})
%		else (
%			ready@pre->includes(active) and
%					ready = ready@pre->excluding(active))
%		endif
%		and waiting = waiting@pre->including(active@pre)
postcondition(S, {call, ?MODULE, swap, []}, {R, Active, Ready, Waiting}) ->
    case sets:size(S#testing_state.ready) of
        0 ->
            (Active == undefined) and
                    (Ready == sets:new());
        _ ->
            (Ready == sets:del_element(Active,
                    S#testing_state.ready)) and
                    sets:is_element(Active, S#testing_state.ready)
    end and
    (Waiting == sets:add_element(S#testing_state.active,
            S#testing_state.waiting)) and
    invariant(S, {R, Active, Ready, Waiting});

postcondition(_S, _C, _R)->
    false.

%%====================================================================
%% Invariant
%%====================================================================
% inv: (ready->intersection(waiting))->isEmpty() and
%		not ((ready->union(waiting))->includes(active)) and
%		(active = null implies ready->isEmpty())
invariant(_S, {_R, Active, Ready, Waiting}) ->
    (sets:size(sets:intersection(Ready, Waiting)) == 0) and
        (not sets:is_element(Active,
                sets:union(Ready, Waiting))) and
        ((Active /= undefined) or (sets:size(Ready) == 0)).

%%====================================================================
%% Generators
%%====================================================================
gen_process()->
    process:new(eqc_gen:int()).

%%====================================================================
%% Wrappers
%%====================================================================
init()->
    R = scheduler:init(),
    {R, scheduler:get_active(), scheduler:get_ready(), scheduler:get_waiting()}.

new(P)->
    R = scheduler:new(P),
    {R, scheduler:get_active(), scheduler:get_ready(), scheduler:get_waiting()}.

ready(P)->
    R = scheduler:ready(P),
    {R, scheduler:get_active(), scheduler:get_ready(), scheduler:get_waiting()}.

swap()->
    R = scheduler:swap(),
    {R, scheduler:get_active(), scheduler:get_ready(), scheduler:get_waiting()}.
