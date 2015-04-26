-module(ocl_statem).

%% API
-export([run_commands/2, commands/1]).

%%====================================================================
%% API
%%====================================================================
%%--------------------------------------------------------------------
%% commands
%%--------------------------------------------------------------------
commands(Mod)->
    commands_nondeterministic(Mod).
    %commands_eqc(Mod).

%%--------------------------------------------------------------------
%% run_commands
%%--------------------------------------------------------------------
run_commands(Mod, Cmds)->
    run_commands_nondeterministic(Mod, Cmds).
    %run_commands_eqc(Mod, Cmds).

%%====================================================================
%% Internal functions
%%====================================================================
%%--------------------------------------------------------------------
%% commands_eqc
%%--------------------------------------------------------------------
commands_eqc(EqcMod) ->
    eqc_statem:commands(EqcMod).

%%--------------------------------------------------------------------
%% run_commands_eqc
%%--------------------------------------------------------------------
run_commands_eqc(EqcMod, Cmds) ->
    eqc_statem:run_commands(EqcMod, Cmds).

%%--------------------------------------------------------------------
%% commands_nondeterministic
%%--------------------------------------------------------------------
commands_nondeterministic(EqcMod) ->
    S = EqcMod:initial_state(),
    commands_nondeterministic(EqcMod, S, []).

%%--------------------------------------------------------------------
%% commands_nondeterministic
%%--------------------------------------------------------------------
commands_nondeterministic(EqcMod, S, R) ->
    case erlang:length(R) of
        10 ->
            lists:reverse(R);
        _ ->
            NextCommand = eqc_gen:pick(EqcMod:command(S)),
            commands_nondeterministic(EqcMod, S, [NextCommand | R])
    end.

%%--------------------------------------------------------------------
%% run_commands_nondeterministic
%%--------------------------------------------------------------------
run_commands_nondeterministic(EqcMod, Cmds) ->
    S = erlang:apply(EqcMod, initial_state, []),
    run_commands_nondeterministic(EqcMod, Cmds, {[], S, ok}).

%%--------------------------------------------------------------------
%% run_commands_nondeterministic
%%--------------------------------------------------------------------
run_commands_nondeterministic(EqcMod, [Cmd | MoreCmds], {H, S, R})->
    case EqcMod:precondition(S, Cmd) of
        true ->
            {call, Module, Function, Args} = Cmd,
            NextR = erlang:apply(Module, Function, Args),
            case Module:postcondition(S, Cmd, NextR) of
                true ->
                    NextS = EqcMod:next_state(S, NextR, Cmd),
                    run_commands_nondeterministic(EqcMod, MoreCmds,
                                                  {[{S, NextR} | H], NextS,
                                                   ok});
                false ->
                    run_commands_nondeterministic(EqcMod, [],
                                                  {[{S, NextR} | H], S,
                                                   {postcondition, false}})
            end;
        false ->
            run_commands_nondeterministic(EqcMod, MoreCmds, {H, S, R})
    end;

run_commands_nondeterministic(_EqcMod, [], {H, S, R})->
    {lists:reverse(H), S, R}.