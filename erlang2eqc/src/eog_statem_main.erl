-module(eog_statem_main).

-export([generate/7]).

%%====================================================================
%% API
%%====================================================================
%%--------------------------------------------------------------------
%% generate
%%--------------------------------------------------------------------
generate(FileName, ModuleName, ClassName, Exp, Operations, StateVariables,
         StateVariablesPre)->
    
    % Generate QuickCheck State Machine
    write_header(FileName, ModuleName),
    write_exports(FileName, Operations, StateVariables, Exp),
    write_state(FileName, StateVariables),
    
    write_functions_block_header(FileName, "property"),
    write_property(FileName),
    
    write_functions_block_header(FileName, "command"),
    write_command(FileName, Operations),
    
    write_functions_block_header(FileName, "initial_state"),
    write_initial_state(FileName, StateVariables, Exp),
    
    write_functions_block_header(FileName, "precondition"),
    write_constraints(FileName, Operations, StateVariables, Exp, preconditions),
    write_constraint_default(FileName, Operations, StateVariables, Exp,
                             preconditions),
    
    write_functions_block_header(FileName, "next_state"),
    write_next_state(FileName, Operations, StateVariables, Exp),
    write_next_state_default(FileName),
    
    write_functions_block_header(FileName, "postcondition"),
    write_constraints(FileName, Operations, StateVariables, Exp, postconditions),
    write_constraint_default(FileName, Operations, StateVariables, Exp,
                             postconditions),
    
    write_functions_block_header(FileName, "generators"),
    write_generators(FileName, Operations, Exp),
    
    write_functions_block_header(FileName, "wrappers"),
    write_wrappers(FileName, ClassName, Operations, StateVariables,
                   StateVariablesPre, Exp).

%%====================================================================
%% Internal functions
%%====================================================================
%%--------------------------------------------------------------------
%% write_header
%%--------------------------------------------------------------------
write_header(FileName, ModuleName)->
    file:write_file(FileName, eog_statem_header:get_header(ModuleName), []).

%%--------------------------------------------------------------------
%% write_exports
%%--------------------------------------------------------------------
write_exports(FileName, Operations, StateVariables, Classes)->
    file:write_file(FileName, eog_statem_header:get_exports(
        Operations, StateVariables, Classes), [append]).

%%--------------------------------------------------------------------
%% write_state
%%--------------------------------------------------------------------
write_state(FileName, StateVariables) ->
    file:write_file(FileName, eog_statem_state:get_state(StateVariables), [append]).

%%--------------------------------------------------------------------
%% write_property
%%--------------------------------------------------------------------
write_property(FileName) ->
    file:write_file(FileName, eog_statem_prop:get_property(), [append]).

%%--------------------------------------------------------------------
%% write_command
%%--------------------------------------------------------------------
write_command(FileName, Operations) ->
    file:write_file(FileName, eog_statem_command:get_command(Operations), [append]).

%%--------------------------------------------------------------------
%% write_initial_state
%%--------------------------------------------------------------------
write_initial_state(FileName, StateVariables, Classes) ->
    file:write_file(FileName, eog_statem_state:get_initial_state(
        StateVariables, Classes), [append]).

%%--------------------------------------------------------------------
%% write_functions_block_header
%%--------------------------------------------------------------------
write_functions_block_header(FileName, FunctionsBlock) ->
    file:write_file(FileName, eog_common_comment:get_block_header(FunctionsBlock),
                    [append]).

%%--------------------------------------------------------------------
%% write_next_state
%%--------------------------------------------------------------------
write_next_state(FileName, Operations, StateVariables, Classes) ->
    file:write_file(FileName, eog_statem_state:get_next_states(
                      Operations, StateVariables, Classes),
                    [append]).

%%--------------------------------------------------------------------
%% write_next_state_default
%%--------------------------------------------------------------------
write_next_state_default(FileName)->
    file:write_file(FileName, eog_statem_state:get_next_state_default(), [append]).

%%--------------------------------------------------------------------
%% write_constraints
%%--------------------------------------------------------------------
write_constraints(FileName, Operations, StateVariables, Classes, ConstraintType) ->
    file:write_file(FileName, eog_statem_constraints:get_constraints(
                      Operations, StateVariables, Classes, ConstraintType),
                    [append]).

%%--------------------------------------------------------------------
%% write_constraint_default
%%--------------------------------------------------------------------
write_constraint_default(FileName, Operations, StateVariables, Classes,
                         ConstraintType) ->
    file:write_file(FileName, eog_statem_constraints:get_constraint_default(
                      Operations, StateVariables, Classes, ConstraintType), [append]).

%%--------------------------------------------------------------------
%% write_generators
%%--------------------------------------------------------------------
write_generators(FileName, Operations, Classes) ->
    file:write_file(FileName, eog_common_gen:get_generators(Operations, Classes),
                    [append]).

%%--------------------------------------------------------------------
%% write_wrappers
%%--------------------------------------------------------------------
write_wrappers(FileName, Class, Operations, StateVariables, StateVariablesPre,
               Classes) ->
    file:write_file(FileName, eog_statem_wrapper:get_wrappers(
                      Class, Operations, StateVariables, StateVariablesPre,
                      Classes), [append]).