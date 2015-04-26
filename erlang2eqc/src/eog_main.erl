-module(eog_main).

-export([generate/1]).

%%====================================================================
%% API
%%====================================================================
%%--------------------------------------------------------------------
%% generate
%%--------------------------------------------------------------------
generate(ClassName) ->
    ModuleName = "eqc_test",
    FileName = "../output/" ++ ModuleName ++ ".erl",
    
    % Initialization
    eog_util_unique:start(),
    eog_util_unique:reset(),
    eog_util_indenter:set_value(0),
    
    % Get constraints
    Exp = get_exp(),

    % Transform constraints into a proplist: [{Class, Operations}, ...]
    OperationsByClass = lists:map(
        fun({class, Options}) ->
            Name = proplists:get_value(name, Options),
            Operations = proplists:get_value(operations, Options),
            {Name, Operations}
        end, Exp),

    case lists:keyfind(ClassName, 1, OperationsByClass) of
        {ClassName, Operations}->
            StateVariables = eog_statem_state:get_state_variables(Operations, Exp),
            case StateVariables of
                [] ->
                    % Generate test suite for a stateless component
                    eog_stateless_main:generate(FileName, ModuleName, ClassName,
                                                Exp, Operations);                    
                
                _ ->
                    % Generate QuickCheck State Machine
                    StateVariablesPre = eog_statem_state:get_state_variables_pre(
                                          Operations, Exp),
                    eog_statem_main:generate(FileName, ModuleName, ClassName,
                                             Exp, Operations, StateVariables,
                                             StateVariablesPre)
            end,
            ok;
        false ->
            ok
    end,
    
    file:copy("../eqc_gen_files/ocl.erl", "../output/ocl.erl"),
    file:copy("../eqc_gen_files/ocl_set.erl", "../output/ocl_set.erl"),
    file:copy("../eqc_gen_files/ocl_seq.erl", "../output/ocl_seq.erl"),
    file:copy("../eqc_gen_files/ocl_statem.erl", "../output/ocl_statem.erl"),
    file:copy("../eqc_gen_files/ocl_gen.erl", "../output/ocl_gen.erl"),
    file:copy("../eqc_gen_files/ocl_datatypes.erl", "../output/ocl_datatypes.erl"),

    ok.

%%====================================================================
%% Internal functions
%%====================================================================
%%--------------------------------------------------------------------
%% get_exp
%%--------------------------------------------------------------------
get_exp() ->
    {ok, [Result]} = file:consult("../../OCLToErlang/constraints.erl"),
    Result.