-module(eog_statem_wrapper).

%% API
-export([get_wrappers/5]).

%%====================================================================
%% API
%% - Class
%% - Operation
%% - OperationData
%% - StateVariable
%%====================================================================
%%--------------------------------------------------------------------
%% @spec get_wrappers(Class, [Operation], [StateVariable], [StateVariable],
%%      [Class]) -> string()
%%--------------------------------------------------------------------
get_wrappers(Class, Operations, StateVariables, StateVariablesAllPre, Classes) ->
    StateVariablesTypes = eog_statem_state:get_state_variables_types(
                            Operations, StateVariables, Classes),
    get_wrappers(Class, Operations, StateVariables, StateVariablesAllPre,
                 StateVariablesTypes, Classes).

%%====================================================================
%% Internal functions
%%====================================================================
%%--------------------------------------------------------------------
%% @spec get_variable_result_name() -> string()
%%--------------------------------------------------------------------
get_variable_result_name()->
    "Result".

%%--------------------------------------------------------------------
%% get_wrappers(Class, [Operation], [StateVariable], [StateVariable], [Class]) ->
%%      string()
%%--------------------------------------------------------------------
get_wrappers(Class, [{operation, OperationData} | MoreOperations],
             StateVariables, StateVariablesAllPre, StateVariablesTypes,
             Classes) ->
    
    StateVariablesPre = eog_statem_state:get_state_variables_post_prev(
                          [{operation, OperationData}], Classes),
    StateVariablesPostAfter = eog_statem_state:get_state_variables_post_after(
                           [{operation, OperationData}], Classes),
    
    StateVariablesOpAllPre = eog_statem_state:get_op_state_variables(
                            StateVariablesAllPre),
    StateVariablesOpPre = eog_statem_state:get_op_state_variables(
                            StateVariablesPre),

    get_wrappers_constraint(Class, OperationData, StateVariablesOpPre,
                            eog_datatype_setlists:append(StateVariablesOpAllPre,
                                                         StateVariablesPostAfter),
                            Classes) ++
        get_wrappers(Class, MoreOperations, StateVariables, StateVariablesAllPre,
                     StateVariablesTypes, Classes);  

get_wrappers(_Class, [], _StateVariables, _StateVariablesAllPre,
             _StateVariablesTypes, _Classes) ->
    "".

%%--------------------------------------------------------------------
%% get_wrappers_constraint(Class, OperationData, [StateVariable], [StateVariable]
%%      [Class]) -> string()
%%--------------------------------------------------------------------
get_wrappers_constraint(Class, OperationData, StateVariablesPrev,
                        StateVariablesAfter, Classes) ->
    get_wrappers_function_definition(OperationData) ++
        eog_util:line_break() ++ eog_util_indenter:indent(4) ++
        eog_statem_state:get_dyn_prev_state(StateVariablesPrev, Classes) ++
        get_wrappers_function_impl(Class, OperationData) ++ "," ++
        eog_util:line_break() ++ eog_util_indenter:indent() ++
        eog_statem_state:get_dyn_after_state(StateVariablesAfter, Classes) ++
        "{" ++ get_variable_result_name() ++ ", " ++
        eog_statem_state:get_dyn_state_name() ++ ", " ++
        eog_statem_state:get_dyn_after_state_name() ++ "}." ++
        eog_util:line_break().

%%--------------------------------------------------------------------
%% get_wrappers_function_definition(OperationData) -> string()
%%--------------------------------------------------------------------
get_wrappers_function_definition(OperationData) ->
    Name = proplists:get_value(name, OperationData),
    Params = proplists:get_value(params, OperationData),
    
    eog_util:line_break() ++ eog_util_indenter:set_value(0) ++
        eog_common_erlang:to_function_name(Name) ++ "(" ++
        get_function_call_params(Params) ++ ")->".

%%--------------------------------------------------------------------
%% get_wrappers_function_impl
%%--------------------------------------------------------------------
get_wrappers_function_impl(Class, OperationData) ->
    Name = proplists:get_value(name, OperationData),
    Params = proplists:get_value(params, OperationData),
    
    get_variable_result_name() ++ " = " ++
        eog_common_erlang:to_module_name(Class) ++ ":" ++
        eog_common_erlang:to_function_name(Name) ++ "(" ++
        get_function_call_params(Params) ++ ")".

%%--------------------------------------------------------------------
%% get_function_call_params
%%--------------------------------------------------------------------
get_function_call_params(Params)->
    lists:foldl(
      fun({param, ParamData}, Result) ->   
              Name = proplists:get_value(name, ParamData),
              case Result of
                  "" ->
                      Result ++ eog_common_erlang:to_variable_name(Name);
                  _ ->
                      Result ++ ", " ++ eog_common_erlang:to_variable_name(Name)
              end
      end,
      "", Params).