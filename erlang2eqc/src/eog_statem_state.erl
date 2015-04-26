-module(eog_statem_state).

%% API
-export([get_testing_state_record_name/0,
         get_state_name/0, get_after_state_name/0, get_dyn_state_name/0,
         get_dyn_after_state_name/0, get_dyn_state_access/1,
         new_empty_state/0, get_state_access/1, get_after_state_access/1,
         get_state_variables/2, get_state_variables_pre/2,
         get_state_variables_post/2,
         get_state_variables_post_after/2, get_state_variables_post_prev/2,
         get_dyn_state_variables_post/4, 
         get_op_state_variables/1, use_dynamic_preconditions/3, 
         get_next_states/3, get_next_state_default/0,
         get_state/1, get_initial_state/2,
         get_dyn_prev_state/2, get_dyn_after_state/2,
         get_state_variables_types/3]).

%%====================================================================
%% API
%% Types:
%% - Operation
%% - OperationData
%% - Class
%% - StateVariable
%% - StateVariableType = unknown |
%%                       real | symbolic | mix |
%%                       {list, real} | {list, symbolic} | {list, mix}
%% - Content
%% - Constraint
%% - ContraintType = preconditions | postconditions | invariants
%%====================================================================
%%--------------------------------------------------------------------
%% @spec get_testing_state_record_name() -> string()
%%--------------------------------------------------------------------
get_testing_state_record_name()->
    "ts".

%%--------------------------------------------------------------------
%% @spec get_state_name() -> string()
%%--------------------------------------------------------------------
get_state_name()->
    "PreState".

%%--------------------------------------------------------------------
%% @spec get_after_state_name() -> string()
%%--------------------------------------------------------------------
get_after_state_name()->
    "AfterState".

%%--------------------------------------------------------------------
%% @spec get_dyna_state_name() -> string()
%%--------------------------------------------------------------------
get_dyn_state_name()->
    "DynPreState".

%%--------------------------------------------------------------------
%% @spec get_after_state_name() -> string()
%%--------------------------------------------------------------------
get_dyn_after_state_name()->
    "DynAfterState".

%%--------------------------------------------------------------------
%% @spec new_empty_state() -> string()
%%--------------------------------------------------------------------
new_empty_state()->
    eog_common_erlang:to_new_empty_record(get_testing_state_record_name()).

%%--------------------------------------------------------------------
%% @spec get_dyn_state_access(StateVariable) -> string()
%%--------------------------------------------------------------------
get_dyn_state_access(StateVariable)->
    Name = get_state_variable_name(StateVariable),
    eog_common_erlang:to_record_field_access(get_dyn_state_name(),
                                             get_testing_state_record_name(),
                                             eog_common_erlang:to_field_name(Name)).

%%--------------------------------------------------------------------
%% @spec get_state_access(StateVariable) -> string()
%%--------------------------------------------------------------------
get_state_access(StateVariable)->
    Name = get_state_variable_name(StateVariable),
    eog_common_erlang:to_record_field_access(get_state_name(),
                                             get_testing_state_record_name(),
                                             eog_common_erlang:to_field_name(Name)).

%%--------------------------------------------------------------------
%% @spec get_after_state_access(StateVariable) -> string()
%%--------------------------------------------------------------------
get_after_state_access(StateVariable)->
    Name = get_state_variable_name(StateVariable),
    eog_common_erlang:to_record_field_access(get_after_state_name(),
                                             get_testing_state_record_name(),
                                             eog_common_erlang:to_field_name(Name)).

%%--------------------------------------------------------------------
%% @spec get_state_variables([Operation], [Class]) -> [StateVariable]
%%
%% @doc Returns the list of state variables involved in the operations.
%%--------------------------------------------------------------------
get_state_variables(Operations, Classes)->
    eog_datatype_setlists:append(
      lists:map(
        fun({operation, OperationData}) ->
                eog_datatype_setlists:append(
                  eog_datatype_setlists:append(
                    get_state_variables(OperationData, Classes, preconditions),
                    get_state_variables(OperationData, Classes, postconditions)),
                  get_state_variables(OperationData, Classes, invariants))
        end,
        Operations)).

%%--------------------------------------------------------------------
%% @spec get_state_variables_pre([Operation], [Class]) -> [StateVariable]
%%
%% @doc Returns the list of state variables in the preconditions.
%%--------------------------------------------------------------------
get_state_variables_pre(Operations, Classes)->
    eog_datatype_setlists:append(
      lists:map(
        fun({operation, OperationData}) ->
                get_state_variables(OperationData, Classes, preconditions)
        end,
        Operations)).

%%--------------------------------------------------------------------
%% @spec get_state_variables_post([Operation], [Class]) -> [StateVariable]
%%
%% @doc Returns the list of state variables in the postconditions.
%%--------------------------------------------------------------------
get_state_variables_post(Operations, Classes)->
    eog_datatype_setlists:append(
      lists:map(
        fun({operation, OperationData}) ->
                get_state_variables(OperationData, Classes, postconditions)
        end,
        Operations)).

%%--------------------------------------------------------------------
%% @spec get_state_variables_post_after([Operation], [Class]) -> [StateVariable]
%%
%% @doc Returns the list of state variables composed by invocations to
%% operations without the pre operator in the postconditions.
%%--------------------------------------------------------------------
get_state_variables_post_after(Operations, Classes)->
    eog_datatype_setlists:append(
      lists:map(
        fun({operation, OperationData}) ->
                get_state_variables_after(OperationData, Classes, postconditions)
        end,
        Operations)).

%%--------------------------------------------------------------------
%% @spec get_state_variables_post_prev([Operation], [Class]) -> [StateVariable]
%%
%% @doc Returns the list of state variables whose previous value is required in
%% any postcondition.
%%--------------------------------------------------------------------
get_state_variables_post_prev(Operations, Classes)->
    eog_datatype_setlists:append(
      lists:map(
        fun({operation, OperationData}) ->
                get_state_variables_prev(OperationData, Classes, postconditions)
        end,
        Operations)).

%%--------------------------------------------------------------------
%% @spec get_dyn_state_variables_post(Operation, [StateVariable], [StateVariableType],
%%      [Class]) -> [StateVariable]
%%
%% @doc Returns the list of state variables whose next value is not known after
%% executing the given operation.
%%--------------------------------------------------------------------
get_dyn_state_variables_post([{operation, OperationData}], StateVariables,
                             StateVariablesTypes, Classes)->
    Constraints = proplists:get_value(constraints, OperationData),
    case proplists:get_value(postconditions, Constraints) of
        [] ->
            StateVariables;
        Postconditions ->
            [{constraint, ConstraintData} | []] =
                eog_common_constraints:group_constraints(Postconditions),
            Body = proplists:get_value(body, ConstraintData),
            {_Result, _VariableNumberReturn, KnownStateVariables} =
                get_standard_next_state(filter(Body, StateVariablesTypes),
                                        undefined, Classes),
            lists:filter(fun(StateVariable) ->
                                 not lists:member(StateVariable, KnownStateVariables)
                         end, StateVariables)
    end.

%%--------------------------------------------------------------------
%% @spec get_op_state_variables([StateVariable]) -> [StateVariable]
%%
%% @doc Returns the state variables that are invocations to operations.
%%--------------------------------------------------------------------
get_op_state_variables([]) ->
    [];

get_op_state_variables([StateVariable | MoreStateVariables]) ->
    case is_state_property(StateVariable) of
        true ->
            get_op_state_variables(MoreStateVariables);
        false ->
            [StateVariable | get_op_state_variables(MoreStateVariables)]
    end.

%%--------------------------------------------------------------------
%% @spec use_dynamic_preconditions([Operation], [StateVariable], [Class]) -> string()
%%
%% @doc Indicates if dynamic preconditions must be used to generate the
%% QuickCheck State Machine.
%%--------------------------------------------------------------------
use_dynamic_preconditions(Operations, StateVariables, Classes) ->
    StateVariablesTypes = get_state_variables_types(Operations, StateVariables,
                                                    Classes),
    use_dynamic_preconditions(Operations, StateVariables, StateVariablesTypes,
                              Classes).

%%--------------------------------------------------------------------
%% @spec get_next_states([Operation], [StateVariable], [Class]) -> string()
%%--------------------------------------------------------------------
get_next_states(Operations, StateVariables, Classes) ->
    StateVariablesTypes = get_state_variables_types(
                            Operations, StateVariables, Classes),
    get_next_states(Operations, StateVariables, StateVariablesTypes, Classes).

%%--------------------------------------------------------------------
%% @spec get_next_state_default() -> string()
%%--------------------------------------------------------------------
get_next_state_default()->
    eog_common_comment:get_function_header(get_next_state_function_name(),
                                           "default") ++
        eog_util:line_break() ++ eog_util_indenter:set_value(0) ++
        eog_common_erlang:to_function_def(get_next_state_function_name(),
                                          [get_state_name(),
                                           get_next_state_result_param_name(),
                                           get_next_state_command_param_name()]
                                         ) ++
        eog_util:line_break() ++ eog_util_indenter:set_value(4) ++ 
        get_state_name() ++ "." ++ eog_util:line_break().

%%--------------------------------------------------------------------
%% @spec get_state([StateVariable]) -> string()
%%
%% @doc Return the state definition as a record
%%--------------------------------------------------------------------
get_state(StateVariables) ->
    Fields = lists:map(
               fun(StateVariable) ->
                       {get_state_variable_name(StateVariable), "undefined"}          
               end, StateVariables),
    eog_common_comment:get_record_header("State") ++
        eog_common_erlang:to_record_def(get_testing_state_record_name(),
                                        Fields).

%%--------------------------------------------------------------------
%% @spec get_initial_state([StateVariable], [Class]) -> string()
%%
%% @doc Return the initialization of the state
%%--------------------------------------------------------------------
get_initial_state(StateVariables, Classes) ->
    Fields = lists:map(
               fun(StateVariable) ->
                       Name = get_state_variable_name(StateVariable),
                       case is_state_property(StateVariable) of
                           true ->
                               {FunCallResult, function} = eog_common_operation:get_operation(
                                                             Name, [StateVariable], Classes),
                               {eog_common_erlang:to_field_name(Name),
                                eog_common_erlang:to_function_call(FunCallResult, [])};
                           false ->
                               {eog_common_erlang:to_field_name(Name),
                                "undefined"}
                       end
               end,
               StateVariables),
    eog_common_comment:get_function_header(get_initial_state_function_name(), "") ++
        eog_util:line_break() ++ 
        eog_common_erlang:to_function_def(get_initial_state_function_name(), []) ++
        eog_util:line_break() ++ eog_util_indenter:set_value(4) ++ 
        eog_common_erlang:to_new_record(get_testing_state_record_name(), Fields) ++
        "." ++ eog_util:line_break() ++ eog_util_indenter:indent(-4).

%%--------------------------------------------------------------------
%% @spec get_dyn_prev_state([StateVariable], [Class]) -> string()
%%--------------------------------------------------------------------
get_dyn_prev_state(StateVariables, Classes) ->
    Fields = lists:map(
               fun(StateVariable) ->
                       Name = get_state_variable_name(StateVariable),
                       {FunCallResult, function} = eog_common_operation:get_operation(
                                                     Name, [StateVariable], Classes),
                       {eog_common_erlang:to_field_name(Name),
                        eog_common_erlang:to_function_call(FunCallResult, [])}
               end,
               StateVariables),
    get_dyn_state_name() ++ " = " ++ eog_common_erlang:to_new_record(
      get_testing_state_record_name(), Fields) ++ ", " ++
        eog_util:line_break() ++ eog_util_indenter:indent().

%%--------------------------------------------------------------------
%% @spec get_dyn_after_state([StateVariable], [Class]) -> string()
%%--------------------------------------------------------------------
get_dyn_after_state(StateVariables, Classes) ->
    Fields = lists:map(
               fun(StateVariable) ->
                       Name = get_state_variable_name(StateVariable),
                       {FunCallResult, function} = eog_common_operation:get_operation(
                                                     Name, [StateVariable], Classes),
                       {eog_common_erlang:to_field_name(Name),
                        eog_common_erlang:to_function_call(FunCallResult, [])}
               end,
               StateVariables),
    get_dyn_after_state_name() ++ " = " ++ eog_common_erlang:to_new_record(
      get_testing_state_record_name(), Fields) ++ ", " ++
        eog_util:line_break() ++ eog_util_indenter:indent().

%%--------------------------------------------------------------------
%% @spec get_state_variables_types([Operation], [StateVariable], [Class]) ->
%%      [{StateVariable, StateVariableType}]
%%--------------------------------------------------------------------
get_state_variables_types(Operations, StateVariables, Classes) ->
    get_state_variables_types_rec(Operations, StateVariables, Classes,
                                  undefined).

%%====================================================================
%% Internal functions
%%====================================================================
%%--------------------------------------------------------------------
%% @spec get_initial_state_function_name() -> string()
%%--------------------------------------------------------------------
get_initial_state_function_name()->
    "initial_state".

%%--------------------------------------------------------------------
%% @spec get_next_state_function_name() -> string()
%%--------------------------------------------------------------------
get_next_state_function_name()->
    "next_state".

%%--------------------------------------------------------------------
%% @spec get_next_state_result_param_name() -> string()
%%--------------------------------------------------------------------
get_next_state_result_param_name()->
    "Result".

%%--------------------------------------------------------------------
%% @spec get_next_state_command_param_name() -> string()
%%--------------------------------------------------------------------
get_next_state_command_param_name()->
    "C".

%%--------------------------------------------------------------------
%% @spec use_dynamic_preconditions([Operation], [StateVariable],
%%      [StateVariableType], [Class]) -> string()
%%
%% @doc Indicates if dynamic preconditions must be used to generate the
%% QuickCheck State Machine.
%%--------------------------------------------------------------------
use_dynamic_preconditions([{operation, OperationData} | MoreOperations],
                          StateVariables, StateVariablesTypes, Classes) ->
    
    Constraints = proplists:get_value(constraints, OperationData),
    Postconditions = eog_common_constraints:group_constraints(
                       proplists:get_value(postconditions, Constraints)),
    case Postconditions of
        [] ->
            case StateVariables of
                [] ->
                    use_dynamic_preconditions(MoreOperations, StateVariables,
                                              Classes);
                _ ->
                    true
            end;
        [{constraint, ConstraintData}] ->
            Body = proplists:get_value(body, ConstraintData),
            {_Result, _VariableNumberReturn, KnownStateVariables} =
                get_standard_next_state(filter(Body, StateVariablesTypes),
                                        undefined, Classes),
            
            AllKnownStateVariables =
                lists:all(
                  fun(StateVariable) ->
                          lists:member(StateVariable, KnownStateVariables)
                  end,
                  StateVariables), 
            
            case AllKnownStateVariables of
                true ->
                    use_dynamic_preconditions(MoreOperations, StateVariables,
                                              Classes);
                false ->
                    true
            end
    end;

use_dynamic_preconditions([], _StateVariables, _StateVariablesTypes, _Classes) ->
    false.

%%--------------------------------------------------------------------
%% @spec get_next_state_command(OperationData) -> string()
%%--------------------------------------------------------------------
get_next_state_command(OperationData)->
    Name = proplists:get_value(name, OperationData),
    Params = proplists:get_value(params, OperationData),
    "{call, ?MODULE, " ++ eog_common_erlang:to_function_name(Name) ++ ", [" ++
        get_next_state_command_params(Params) ++ "]}".

%%--------------------------------------------------------------------
%% @spec get_next_state_command_params([Param]) -> string()
%%--------------------------------------------------------------------
get_next_state_command_params(Params)->
    eog_util:foldl("", Params,
                   fun({param, ParamData}) ->
                           Name = proplists:get_value(name, ParamData),
                           eog_common_erlang:to_variable_name(Name)
                   end,
                   fun() ->
                           ", "   
                   end).

%%--------------------------------------------------------------------
%% @spec get_next_state_function_definition(OperationData) -> string()
%%--------------------------------------------------------------------
get_next_state_function_definition(OperationData) ->
    eog_util:line_break() ++ eog_util_indenter:set_value(0) ++
        eog_common_erlang:to_function_def(get_next_state_function_name(),
                                          [get_state_name(),
                                           get_next_state_result_param_name(),
                                           get_next_state_command(OperationData)]
                                         ).

%%--------------------------------------------------------------------
%% @spec is_state_property(StateVariable) -> boolean()
%%--------------------------------------------------------------------
is_state_property(StateVariable) ->
    ReferredOperation = proplists:get_value(referredOperation,
                                            StateVariable),
    case ReferredOperation of
        undefined ->
            true;
        _ ->
            false
    end.

%%--------------------------------------------------------------------
%% @spec get_state_variable_name_from_number(integer() | undefined) -> string()
%%--------------------------------------------------------------------
get_state_variable_name_from_number(undefined)->
    get_state_name();
get_state_variable_name_from_number(VariableNumber)->
    get_state_name() ++ erlang:integer_to_list(VariableNumber).

%%--------------------------------------------------------------------
%% @spec get_state_variable_name
%%--------------------------------------------------------------------
get_state_variable_name(StateVariable)->
    case is_state_property(StateVariable) of
        true ->
            ReferredProperty = proplists:get_value(referredProperty,
                                                   StateVariable),
            eog_common_erlang:to_field_name(ReferredProperty);
        false ->
            [{content, ContentData} | _] = proplists:get_value(contents, StateVariable),
            case proplists:get_value(referredOperation, ContentData) of
                undefined ->
                    ReferredOperation = proplists:get_value(referredOperation,
                                                            StateVariable),
                    OperationName = proplists:get_value(
                                      name, ReferredOperation), 
                    eog_common_erlang:to_field_name(OperationName);
                ReferredOperationContent ->
                    OperationName = proplists:get_value(
                                      name, ReferredOperationContent), 
                    eog_common_erlang:to_field_name(OperationName)
            end
    end.

%%--------------------------------------------------------------------
%% @spec get_state_variables_types_rec([Operation], [StateVariable], [Class],
%%      [{StateVariable, StateVariableType}]) -> [{StateVariable, StateVariableType}]
%%--------------------------------------------------------------------
get_state_variables_types_rec(Operations, StateVariables, Classes,
                          StateVariablesTypes) ->
    StateVariablesTypesBase =
        get_state_variables_types(Operations, StateVariables, Classes,
                              StateVariablesTypes),

    NewStateVariablesTypes = get_state_variables_types(
                               Operations,StateVariables, Classes,
                               StateVariablesTypesBase),
    
    case (sets:from_list(StateVariablesTypesBase) ==
              sets:from_list(NewStateVariablesTypes)) of
        true ->
            NewStateVariablesTypes;
        false ->
            get_state_variables_types_rec(Operations, StateVariables, Classes,
                                          NewStateVariablesTypes)
    end.

%%--------------------------------------------------------------------
%% @spec get_state_variables_types([Operation], [StateVariable], [Class],
%%      [{StateVariable, StateVariableType}]) -> [{StateVariable, StateVariableType}]
%%--------------------------------------------------------------------
get_state_variables_types([Operation | MoreOperations], StateVariables,
                          Classes, StateVariablesTypes) ->
    
    join_state_variable_types(
      get_state_variables_types(MoreOperations, StateVariables, Classes,
                                StateVariablesTypes),
      get_state_variables_types_from_operation(Operation, StateVariables,
                                               Classes, StateVariablesTypes));

get_state_variables_types([], _StateVariables, _Classes, _StateVariablesTypes) ->
    [].

%%--------------------------------------------------------------------
%% @spec get_state_variables(OperationData, [Class], ContraintType) -> [StateVariable]
%%--------------------------------------------------------------------
get_state_variables(OperationData, Classes, ConstraintType)->
    Constraints = proplists:get_value(constraints, OperationData),
    SpecificConstraints = proplists:get_value(ConstraintType, Constraints),
    eog_datatype_setlists:append(
      lists:map(
        fun({constraint, ConstraintData}) ->
                Body = proplists:get_value(body, ConstraintData),
                {_ResultCode, StateVariablesPrev, StateVariablesAfter, _Variables} =
                    eog_statem_constraints:get_constraint(Body, Classes,
                                                          ConstraintType),
                eog_datatype_setlists:append(StateVariablesPrev,
                                             StateVariablesAfter)
        end,
        SpecificConstraints)).

%%--------------------------------------------------------------------
%% @spec get_state_variables_prev(OperationData, [Class], ContraintType) -> [StateVariable]
%%--------------------------------------------------------------------
get_state_variables_prev(OperationData, Classes, ConstraintType)->
    Constraints = proplists:get_value(constraints, OperationData),
    SpecificConstraints = proplists:get_value(ConstraintType, Constraints),
    eog_datatype_setlists:append(
      lists:map(
        fun({constraint, ConstraintData}) ->
                Body = proplists:get_value(body, ConstraintData),
                {_ResultCode, StateVariablesPrev, _StateVariablesAfter, _Variables} =
                    eog_statem_constraints:get_constraint(Body, Classes,
                                                          ConstraintType),
                StateVariablesPrev
        end,
        SpecificConstraints)).

%%--------------------------------------------------------------------
%% @spec get_state_variables_after(OperationData, [Class], ContraintType) -> [StateVariable]
%%--------------------------------------------------------------------
get_state_variables_after(OperationData, Classes, ConstraintType)->
    Constraints = proplists:get_value(constraints, OperationData),
    SpecificConstraints = proplists:get_value(ConstraintType, Constraints),
    eog_datatype_setlists:append(
      lists:map(
        fun({constraint, ConstraintData}) ->
                Body = proplists:get_value(body, ConstraintData),
                {_ResultCode, _StateVariablesPrev, StateVariablesAfter, _Variables} =
                    eog_statem_constraints:get_constraint(Body, Classes,
                                                          ConstraintType),
                StateVariablesAfter
        end,
        SpecificConstraints)).

%%--------------------------------------------------------------------
%% @spec get_next_states([Operation], [StateVariable],
%%      [{StateVariable, StateVariableType}], [Class]) -> string()
%%--------------------------------------------------------------------
get_next_states([{operation, OperationData} | MoreOperations], StateVariables,
                StateVariablesTypes, Classes) ->
    Name = proplists:get_value(name, OperationData),
    Constraints = proplists:get_value(constraints, OperationData),
    Postconditions = eog_common_constraints:group_constraints(
                       proplists:get_value(postconditions, Constraints)),
    
    eog_common_comment:get_function_header(get_next_state_function_name(), Name) ++
        case Postconditions of
            [] ->
                get_next_state_empty(OperationData, StateVariables,
                                     StateVariablesTypes, Classes);
            _ ->
                get_next_state_operation(OperationData, Postconditions,
                                         StateVariables, StateVariablesTypes,
                                         Classes)
        end ++
        get_next_states(MoreOperations, StateVariables, StateVariablesTypes,
                        Classes);  

get_next_states([], _StateVariables, _UnknownStateVariables, _Classes) ->
    "".

%%--------------------------------------------------------------------
%% @spec get_next_state([Constraint], [StateVariable], [StateVariableType], [Class]) ->
%%      string()
%%--------------------------------------------------------------------
get_next_state(Constraints, StateVariables, StateVariablesTypes, Classes) -> 
    
    {Result, VariableNumberReturn, KnownStateVariables} =
        get_standard_next_state(filter(Constraints, StateVariablesTypes),
                                undefined, Classes),
    
    AllKnownStateVariables =
        lists:all(
          fun(StateVariable) ->
                  lists:member(StateVariable, KnownStateVariables)
          end,
          StateVariables),  
    
    UnknownStateVariables =
        case AllKnownStateVariables of
            true ->
                get_state_variable_name_from_number(VariableNumberReturn);
            false ->
                get_default_next_state(VariableNumberReturn, KnownStateVariables,
                                       StateVariables)
        end,
    
    case Result ++ UnknownStateVariables of
        "" ->
            get_state_name();
        _ ->
            case Result of
                "" ->
                    UnknownStateVariables;
                _ ->
                    Result ++ ", " ++
                        eog_util:line_break() ++ eog_util_indenter:indent() ++
                        UnknownStateVariables
            end
    end.

%%--------------------------------------------------------------------
%% @spec get_next_state_empty(OperationData, [StateVariable], [StateVariablesType], [Class]) ->
%%      string()
%%--------------------------------------------------------------------
get_next_state_empty(OperationData, StateVariables, StateVariablesTypes, Classes) ->
    get_next_state_function_definition(OperationData) ++
        eog_util:line_break() ++ eog_util_indenter:set_value(4) ++
        get_dyn_after_state_name() ++ " = {call, erlang, element, [3, " ++
        get_next_state_result_param_name() ++ "]}," ++
        eog_util:line_break() ++ eog_util_indenter:indent() ++
        get_next_state([], StateVariables, StateVariablesTypes, Classes) ++ ";" ++
        eog_util:line_break().

%%--------------------------------------------------------------------
%% @spec get_next_state_operation(OperationData, [Constraint], [StateVariable],
%%      [StateVariablesType], [Class]) -> string()
%%--------------------------------------------------------------------
get_next_state_operation(OperationData,
                         [{constraint, ConstraintData} | MoreConstraints],
                         StateVariables, StateVariablesTypes, Classes) ->
    
    Body = proplists:get_value(body, ConstraintData),
    
    get_next_state_function_definition(OperationData) ++
        eog_util:line_break() ++ eog_util_indenter:set_value(4) ++
        get_dyn_after_state_name() ++ " = {call, erlang, element, [3, " ++
        get_next_state_result_param_name() ++ "]}," ++
        eog_util:line_break() ++ eog_util_indenter:indent() ++
        get_next_state(Body, StateVariables, StateVariablesTypes, Classes) ++ ";" ++
        eog_util:line_break() ++
        get_next_state_operation(OperationData, MoreConstraints,
                                 StateVariables, StateVariablesTypes, Classes);

get_next_state_operation(_OperationData, [], _StateVariables,
                         _StateVariablesTypes, _Classes) ->
    "".

%%--------------------------------------------------------------------
%% @spec valid_eq([Content], [StateVariablesType]) -> boolean()
%%--------------------------------------------------------------------
valid_eq([Content1, Content2], StateVariablesTypes)->
    IsResultVariable1 = is_result_variable(Content1),
    IsResultVariable2 = is_result_variable(Content2),
    case {IsResultVariable1, IsResultVariable2} of
        {false, false} ->
            StateVariable1 = contains_state_variable(Content1),
            StateVariable2 = contains_state_variable(Content2),
            case {StateVariable1, StateVariable2} of
                {true, false} ->
                    valid_dependences(Content2, StateVariablesTypes);
                {false, true} ->
                    valid_dependences(Content1, StateVariablesTypes);
                _ ->
                    false
            end;
        _ ->
            false
    end.

%%--------------------------------------------------------------------
%% @spec valid_dependences(Content, [StateVariablesType]) -> boolean()
%%--------------------------------------------------------------------
valid_dependences({content, Content}, StateVariablesTypes)->
    valid_dependences(Content, StateVariablesTypes);

valid_dependences([{expression, "OperationCallExp"} | Data],
                  StateVariablesTypes = undefined)->
    Operation = proplists:get_value(referredOperation, Data),
    Name = proplists:get_value(name, Operation),
    Contents = proplists:get_value(contents, Data),
    
    case Name of
        "atPre" ->
            true;
        _ ->
            lists:all(
              fun(Content) ->
                       valid_dependences(Content, StateVariablesTypes)
              end,
              Contents)
    end;

valid_dependences([{expression, "OperationCallExp"} | Data], StateVariablesTypes)->
    Operation = proplists:get_value(referredOperation, Data),
    Name = proplists:get_value(name, Operation),
    Contents = proplists:get_value(contents, Data),
    
    case Name of
        "atPre" ->
            [{content, Content}| _] = Contents,
            case lists:keyfind(Content, 1, StateVariablesTypes) of
                {Content, real} ->
                    true;
                _ ->
                    false
            end;
        _ ->
            lists:all(
              fun(Content) ->
                      case lists:keyfind(Content, 1, StateVariablesTypes) of
                          {Content, symbolic} ->
                              false;
                          _ ->
                              valid_dependences(Content, StateVariablesTypes)
                      end    
              end,
              Contents)
    end;
%% 
%% valid_dependences([{expression, "UndefinedLiteralExp"} | _Data],
%%                   _StateVariablesTypes)->
%%     true;
%% 
%% valid_dependences([{expression, "CollectionLiteralExp"} | _Data],
%%                   _StateVariablesTypes)->
%%     true;
%% 
%% valid_dependences([{expression, "VariableExp"} | _Data], _StateVariablesTypes)->
%%     true;
%% 
%% valid_dependences([{expression, "PropertyCallExp"} | _Data], _StateVariablesTypes)->
%%     true;
%% 
%% valid_dependences([{expression, "IteratorExpImpl"} | _Data], _StateVariablesTypes)->
%%     true;

valid_dependences(_Content, _StateVariablesTypes)->
    true.

%%--------------------------------------------------------------------
%% @spec contains_state_variable(Content) -> boolean()
%%--------------------------------------------------------------------
contains_state_variable({content, Content})->
    contains_state_variable(Content);

contains_state_variable([{expression, "PropertyCallExp"} | _Data])->
    true;

contains_state_variable(_Content)->
    false.

%%--------------------------------------------------------------------
%% @spec is_result_variable(Content) -> boolean()
%%--------------------------------------------------------------------
is_result_variable({content, Content})->
    is_result_variable(Content);

is_result_variable([{expression, "VariableExp"} | Data])->
    ReferredVariable = proplists:get_value(referredVariable, Data),
    Name = proplists:get_value(name, ReferredVariable),
    case Name of
        "result" ->
            true;
        _ ->
            false
    end;

is_result_variable(_Content)->
    false.

%%--------------------------------------------------------------------
%% @spec filter([Content], [StateVariableType]) -> [Constraint]
%%--------------------------------------------------------------------
filter([{content, Content} | MoreContents], StateVariablesTypes) ->
    [{content, filter(Content, StateVariablesTypes)} |
         filter(MoreContents, StateVariablesTypes)];

filter([{expression, "CollectionLiteralExp"} | _Data], _StateVariablesTypes) -> 
    [];

filter([{expression, "IfExp"} | Data], StateVariablesTypes) ->
    Condition = proplists:get_value(condition, Data),
    Then = proplists:get_value(then, Data),
    Else = proplists:get_value(else, Data),
    
    %case valid_dependences(Condition, StateVariablesTypes) of
    %    true ->
            [{expression, "IfExp"},
             {condition, Condition},
             {then, filter(Then, StateVariablesTypes)},
             {else, filter(Else, StateVariablesTypes)}];
    %    false ->
    %        []
    %end;

filter([{expression, "OperationCallExp"} | Data], StateVariablesTypes) ->
    Name = proplists:get_value(name, Data),
    Operation = proplists:get_value(referredOperation, Data),
    OperationName = proplists:get_value(name, Operation),
    Type = proplists:get_value(type, Operation),
    Contents = proplists:get_value(contents, Data),
    
    case OperationName of
        "and" ->
            [{expression, "OperationCallExp"},
             {name, Name},
             {referredOperation, [{name, "seq"}, {type, Type}]},
             {contents, filter(Contents, StateVariablesTypes)}];
        "=" ->
            case valid_eq(Contents, StateVariablesTypes) of
                true ->
                    [{content, Content1}, Content2] = Contents,
                    Source1 = proplists:get_value(source, Content1),
                    ReferredProperty1 = proplists:get_value(referredProperty,
                                                            Content1),
                    [{expression, "OperationCallExp"},
                     {name, Name},
                     {referredOperation,
                      [{name, "assign"},
                       {type, Type}]},
                     {contents,
                      [{content,
                        [{expression, "PropertyCallExpAssign"},
                         {source, Source1},
                         {referredProperty, ReferredProperty1}
                        ]},
                       Content2]}];                
                false ->
                    []
            end;
        _ ->
            []
    end;

filter([{expression, "PropertyCallExp"} | _Data], _StateVariablesTypes) -> 
    [];

filter([{expression, "VariableExp"} | _Data], _StateVariablesTypes) -> 
    [];

filter([{expression, "UndefinedLiteralExp"} | _Data], _StateVariablesTypes) -> 
    [];

filter([], _StateVariablesTypes) ->
    [].

%%--------------------------------------------------------------------
%% @spec get_state_variables_types_from_operation([Operation], [StateVariable],
%%      [Class], [{StateVariable, StateVariableType}]) -> [{StateVariable, StateVariableType}]
%%--------------------------------------------------------------------
get_state_variables_types_from_operation({operation, OperationData},
                                         StateVariables, Classes,
                                         StateVariablesTypes) ->
    
    Constraints = proplists:get_value(constraints, OperationData),
    Postconditions = eog_common_constraints:group_constraints(
                       proplists:get_value(postconditions, Constraints)),
    
    StateVariablesWithType =
        case Postconditions of
            [] ->
                [];
            [{constraint, ConstraintData}] ->
                Body = proplists:get_value(body, ConstraintData),
                get_state_variable_types_from_constraints(
                  filter(Body, StateVariablesTypes), StateVariables, Classes, [])
        end,
   
    lists:foldl(
      fun(StateVariable, R) ->
              case lists:keyfind(StateVariable, 1, StateVariablesWithType) of
                  false ->
                      [{StateVariable, symbolic} | R];
                  {StateVariable, unknown} ->
                      [{StateVariable, symbolic} |
                           lists:keydelete(StateVariable, 1, R)];
                  _ ->
                      R
              end
      end, StateVariablesWithType, StateVariables).

%%--------------------------------------------------------------------
%% @spec get_state_variable_types_from_constraints([Expression], [StateVariable],
%%      [Class], [{StateVariable, StateVariableType}]) -> [{StateVariable, StateVariableType}]
%%--------------------------------------------------------------------
get_state_variable_types_from_constraints([{expression, "CollectionLiteralExp"} | _Data],
                                          _StateVariables, _Classes, Result) ->
    Result;   

get_state_variable_types_from_constraints([{expression, "IfExp"} | Data],
                                          StateVariables, Classes, Result) ->
    
    Then = proplists:get_value(then, Data),
    Else = proplists:get_value(else, Data),
    
    StateVariablesTypesThen = get_state_variable_types_from_constraints(
                                Then, StateVariables, Classes, []),
    StateVariablesTypesElse = get_state_variable_types_from_constraints(
                                Else, StateVariables, Classes, []),

    StateVariablesTypesThenMix = lists:foldl(
            fun({StateVariable, StateVariableType}, R) ->
                    case lists:keyfind(StateVariable, 1, R) of
                        false ->
                            [{StateVariable, symbolic} | R];
                        _ ->
                            join_state_variable_type(
                              {StateVariable, StateVariableType}, R)
                    end
            end, StateVariablesTypesThen, StateVariablesTypesElse),
    
    append_state_variable_types(
      join_state_variable_types(StateVariablesTypesThenMix,
                                StateVariablesTypesElse),
      Result);

get_state_variable_types_from_constraints([{expression, "OperationCallExp"} | Data],
                                          StateVariables, Classes, Result) ->
    Operation = proplists:get_value(referredOperation, Data),
    OperationName = proplists:get_value(name, Operation),
    
    Contents = proplists:get_value(contents, Data),
    
    case OperationName of
        "atPre" ->
            [{content, ExpPre}] = Contents,
            get_state_variable_types_from_constraints(
              ExpPre, StateVariables, Classes, Result);
        "assign" ->
            [{content, Exp1}, {content, Exp2}] = Contents,
            StateVariablesTypes1 = get_state_variable_types_from_constraints(
                                     Exp1, StateVariables, Classes, []),
            StateVariablesTypes2 = get_state_variable_types_from_constraints(
                                     Exp2, StateVariables, Classes,  []),
            
            append_state_variable_types(
              append_state_variable_types(StateVariablesTypes1, StateVariablesTypes2),
              Result);
        _ ->
            case Contents of
                [{content, Exp}] ->
                    get_state_variable_types_from_constraints(
                      Exp, StateVariables, Classes, Result);
                _ ->
                    lists:foldl(
                      fun({content, Exp}, StateVariablesWithType) ->
                              append_state_variable_types(
                                get_state_variable_types_from_constraints(
                                  Exp, StateVariables, Classes, Result),
                                StateVariablesWithType)
                      end, Result, Contents)
            end
    end;

get_state_variable_types_from_constraints([{expression, "PropertyCallExp"} | Data],
                                          StateVariables, Classes, Result) ->
    
    Source = proplists:get_value(source, Data),
    get_state_variable_types_from_constraints(Source, StateVariables, Classes,
                                              Result);

get_state_variable_types_from_constraints([{expression, "PropertyCallExpAssign"} | Data],
                                          StateVariables, Classes, Result) ->
    Source = proplists:get_value(source, Data),
    add_state_variable_type({[{expression, "PropertyCallExp"} | Data], real},
                            get_state_variable_types_from_constraints(
                              Source, StateVariables, Classes, Result));

get_state_variable_types_from_constraints([{expression, "VariableExp"} | _Data],
                                          _StateVariables, _Classes, Result) ->
    Result;

get_state_variable_types_from_constraints([{expression, "UndefinedLiteralExp"} | _Data],
                                          _StateVariables, _Classes, Result) ->
    Result;

get_state_variable_types_from_constraints([{expression, "IteratorExpImpl"} | _Data],
                                          _StateVariables, _Classes, Result) ->
    Result;

get_state_variable_types_from_constraints([], _StateVariables, _Classes, Result)->
    Result.

%%--------------------------------------------------------------------
%% @spec append_state_variable_types([{StateVariable, StateVariableType}],
%%      [{StateVariable, StateVariableType}]) -> [{StateVariable, StateVariableType}]
%%--------------------------------------------------------------------
append_state_variable_types(StateVariablesWithType1, StateVariablesWithType2) ->
    lists:foldl(
      fun(StateVariableWithType, Result) ->
              add_state_variable_type(StateVariableWithType, Result)
      end, StateVariablesWithType2, StateVariablesWithType1).

%%--------------------------------------------------------------------
%% @spec add_state_variable_type({StateVariable, StateVariableType},
%%    [{StateVariable, StateVariableType}]) -> [{StateVariable, StateVariableType}]
%%--------------------------------------------------------------------
add_state_variable_type({StateVariable, StateVariableType}, StateVariablesWithType) ->
    case lists:keyfind(StateVariable, 1, StateVariablesWithType) of
        false ->
            [{StateVariable, StateVariableType} | StateVariablesWithType];
        {StateVariable, unknown} ->
            [{StateVariable, StateVariableType} |
                 lists:keydelete(StateVariable, 1, StateVariablesWithType)];
        {StateVariable, real} ->
            [{StateVariable, StateVariableType} |
                 lists:keydelete(StateVariable, 1, StateVariablesWithType)];
        {StateVariable, symbolic} ->
            StateVariablesWithType;
        {StateVariable, mix} ->
            StateVariablesWithType;
        {StateVariable, {list, real}} ->
            [{StateVariable, StateVariableType} |
                 lists:keydelete(StateVariable, 1, StateVariablesWithType)];
        {StateVariable, {list, symbolic}} ->
            StateVariablesWithType;
        {StateVariable, {list, mix}} ->
            StateVariablesWithType
    end.

%%--------------------------------------------------------------------
%% @spec join_state_variable_types([{StateVariable, StateVariableType}],
%%      [{StateVariable, StateVariableType}]) -> [{StateVariable, StateVariableType}]
%%--------------------------------------------------------------------
join_state_variable_types(StateVariablesWithType1, StateVariablesWithType2) ->
    lists:foldl(
      fun(StateVariablesWithType, Result) ->
              join_state_variable_type(StateVariablesWithType, Result)
      end, StateVariablesWithType2, StateVariablesWithType1).

%%--------------------------------------------------------------------
%% @spec join_state_variable_type({StateVariable, StateVariableType},
%%    [{StateVariable, StateVariableType}]) -> [{StateVariable, StateVariableType}]
%%--------------------------------------------------------------------
join_state_variable_type({StateVariable, StateVariableType}, StateVariablesWithType) ->
    case lists:keyfind(StateVariable, 1, StateVariablesWithType) of
        false ->
            [{StateVariable, unknown} | StateVariablesWithType];
        {StateVariable, unknown} ->
            StateVariablesWithType;
        {StateVariable, real} ->
            [{StateVariable, StateVariableType} |
                 lists:keydelete(StateVariable, 1, StateVariablesWithType)];
        {StateVariable, symbolic} ->
            StateVariablesWithType;
        {StateVariable, mix} ->
            StateVariablesWithType;
        {StateVariable, {list, real}} ->
            [{StateVariable, StateVariableType} |
                 lists:keydelete(StateVariable, 1, StateVariablesWithType)];
        {StateVariable, {list, symbolic}} ->
            StateVariablesWithType;
        {StateVariable, {list, mix}} ->
            StateVariablesWithType
    end.

%%--------------------------------------------------------------------
%% @spec assign_unknown_state_variables([StateVariable], [StateVariable]) ->
%%      string()
%%--------------------------------------------------------------------
assign_unknown_state_variables(KnownVariables, StateVariables)->
    do_assign_unknown_state_variables(KnownVariables, StateVariables, 2).

%%--------------------------------------------------------------------
%% @spec do_assign_unknown_state_variables([StateVariable], [StateVariable],
%%      integer() | undefined) -> string()
%%--------------------------------------------------------------------
do_assign_unknown_state_variables(KnownVariables, [StateVariable | []], Number)->
    case lists:member(StateVariable, KnownVariables) of
        true ->
            do_assign_unknown_state_variables(KnownVariables, [], Number + 1);
        false ->
            assign_unknown_state_variable(StateVariable, Number)
    end;

do_assign_unknown_state_variables(KnownVariables,
                                  [StateVariable | MoreStateVariables],
                                  Number)->
    case lists:member(StateVariable, KnownVariables) of
        true ->
            do_assign_unknown_state_variables(KnownVariables, MoreStateVariables,
                                              Number + 1);
        false ->
            Next = do_assign_unknown_state_variables(KnownVariables, MoreStateVariables,
                                                     Number + 1),
            assign_unknown_state_variable(StateVariable, Number) ++
                (case Next of
                     "" ->
                         Next;
                     _ ->
                         "," ++ eog_util:line_break() ++
                             eog_util_indenter:indent() ++ Next
                 end)
    end;

do_assign_unknown_state_variables(_KnownVariables, [], _Number)->
    "".

%%--------------------------------------------------------------------
%% @spec assign_unknown_state_variable(StateVariable, integer() | undefined) -> string()
%%--------------------------------------------------------------------
assign_unknown_state_variable(StateVariable, Number)->
    get_state_variable_name(StateVariable) ++ " = " ++
        "{call, erlang, element, [" ++ erlang:integer_to_list(Number) ++
        ", " ++ get_dyn_after_state_name() ++ "]}".

%%--------------------------------------------------------------------
%% @spec get_default_next_state(integer() | undefined, [StateVariable],
%%      [StateVariable]) -> string()
%%--------------------------------------------------------------------
get_default_next_state(VariableNumberReturn, KnownStateVariables,
                       StateVariables)->
    case assign_unknown_state_variables(KnownStateVariables, StateVariables) of
        "" ->
            "";
        _UnknownVariablesState ->
            eog_common_erlang:to_record_access(
              get_state_variable_name_from_number(VariableNumberReturn),
              get_testing_state_record_name()) ++
                " { " ++ eog_util:line_break() ++ eog_util_indenter:indent(4) ++
                assign_unknown_state_variables(KnownStateVariables, StateVariables) ++
                eog_util:line_break() ++ eog_util_indenter:indent(-4) ++ "}"
    end.

%%--------------------------------------------------------------------
%% get_standard_next_state([Contraint], integer(), [Class]) -> {string(), integer(), [StateVariable]}
%%--------------------------------------------------------------------
get_standard_next_state([{expression, "CollectionLiteralExp"} | Data],
                        VariableNumberReturn, _Classes) -> 
    Kind = proplists:get_value(kind, Data),
    case Kind of
        "Set" ->
            {"ocl_set:new_set()", VariableNumberReturn, []}
    end;    

get_standard_next_state([{expression, "IfExp"} | Data], VariableNumberReturn,
                        Classes) ->
    Condition = proplists:get_value(condition, Data),
    Then = proplists:get_value(then, Data),
    Else = proplists:get_value(else, Data),
    
    VariableNumber = eog_util_unique:next(),
    VariableName = get_state_variable_name_from_number(VariableNumber),
    
    ResultCodeIf = VariableName ++ " = case ",
    {ResultCodeCondition, _, _} = get_standard_next_state(
                                    Condition, VariableNumberReturn, Classes),
    ResultCodeSep1 = " of " ++ eog_util:line_break() ++ eog_util_indenter:indent(4) ++
                         "true -> " ++ eog_util:line_break() ++ eog_util_indenter:indent(4),
    {ResultCodeThen, VariableNumberReturnThen, StateVariablesThen} =
        get_standard_next_state(Then, VariableNumberReturn, Classes),
    {ResultCodeElse, VariableNumberReturnElse, StateVariablesElse} =
        get_standard_next_state(Else, VariableNumberReturn, Classes),
    
    ResultCodeSep2 =
        (case ResultCodeThen of
             "" ->
                 "";
             _ ->
                 ", " ++ eog_util:line_break() ++ eog_util_indenter:indent()
         end) ++ 
            (case get_default_next_state(VariableNumberReturnThen, StateVariablesThen,
                                         eog_datatype_setlists:append(StateVariablesThen,
                                                                      StateVariablesElse)) of
                 "" ->
                     get_state_variable_name_from_number(VariableNumberReturnThen) ++ ";" ++
                         eog_util:line_break() ++ eog_util_indenter:indent(-4) ++
                         "false -> " ++
                         eog_util:line_break() ++ eog_util_indenter:indent(4);
                 _DefaultNextStateThen ->
                     get_default_next_state(VariableNumberReturnThen,
                                            StateVariablesThen,
                                            eog_datatype_setlists:append(StateVariablesThen,
                                                                         StateVariablesElse)) ++ 
                         ";" ++ eog_util:line_break() ++ eog_util_indenter:indent(-4) ++
                         "false -> " ++
                         eog_util:line_break() ++ eog_util_indenter:indent(4)
             end),
    
    ResultCodeEndIf =
        (case ResultCodeElse of
             "" ->
                 "";
             _ ->
                 ", " ++ eog_util:line_break() ++ eog_util_indenter:indent()
         end)  ++
            (case get_default_next_state(VariableNumberReturnElse,
                                         StateVariablesElse,
                                         eog_datatype_setlists:append(StateVariablesThen,
                                                                      StateVariablesElse)) of
                 "" ->
                     get_state_variable_name_from_number(VariableNumberReturnElse) ++ 
                         eog_util:line_break() ++ eog_util_indenter:indent(-8) ++
                         "end";
                 _DefaultNextStateElse ->
                     get_default_next_state(VariableNumberReturnElse,
                                            StateVariablesElse,
                                            eog_datatype_setlists:append(StateVariablesThen,
                                                                         StateVariablesElse)) ++ 
                         eog_util:line_break() ++ eog_util_indenter:indent(-8) ++
                         "end"
             end),
    
    {ResultCodeIf ++ ResultCodeCondition ++ ResultCodeSep1 ++
         ResultCodeThen ++ ResultCodeSep2 ++
         ResultCodeElse ++ ResultCodeEndIf,
     VariableNumber,
     eog_datatype_setlists:union(StateVariablesThen, StateVariablesElse)};

get_standard_next_state([{expression, "OperationCallExp"} | Data],
                        VariableNumberReturn, Classes) ->    
    
    Operation = proplists:get_value(referredOperation, Data),
    OperationName = proplists:get_value(name, Operation),
    
    Contents = proplists:get_value(contents, Data),
    
    case OperationName of
        "atPre" ->
            [{content, ExpPre}] = Contents,
            get_standard_next_state(ExpPre, VariableNumberReturn, Classes);
        
        "assign" ->
            [{content, Exp1}, {content, Exp2}] = Contents,
            {Result1, _NextVariableNumberReturn1, StateVariables1} =
                get_standard_next_state(Exp1, VariableNumberReturn, Classes),
            {Result2, _NextVariableNumberReturn2, StateVariables2} =
                get_standard_next_state(Exp2, VariableNumberReturn, Classes),
            {OpResult, _Type} = eog_common_operation:get_operation(
                                  OperationName, Contents, Classes),
            
            VariableNumber = eog_util_unique:next(),
            
            {get_state_variable_name_from_number(VariableNumber) ++ " = " ++
                 Result1 ++ " " ++ OpResult ++ " " ++
                 Result2 ++ eog_util:line_break() ++ eog_util_indenter:indent(-4) ++ "}",
             VariableNumber, eog_datatype_setlists:append(
               StateVariables1, StateVariables2)};
        
        _ ->
            case Contents of
                [{content, Exp}] ->
                    case eog_common_operation:get_operation(OperationName,
                                                            Contents, Classes) of
                        {OpResult, function} ->
                            {ResultCode, NextVariableNumberReturn, _} =
                                get_standard_next_state(Exp,
                                                        VariableNumberReturn,
                                                        Classes),
                            {OpResult ++ "(" ++ ResultCode ++ ")",
                             NextVariableNumberReturn, []}
                    end;
                _ ->
                    lists:foldl(
                      fun({content, Exp}, {R, V, SV}) ->
                              case R of
                                  "" ->
                                      get_standard_next_state(
                                        Exp, VariableNumberReturn, Classes);
                                  _ ->
                                      {ResultCode, NextVariableNumberReturn, StateVariables} =
                                          get_standard_next_state(Exp, V, Classes),
                                      case eog_common_operation:get_operation(
                                             OperationName, Contents, Classes) of
                                          
                                          {OpResult, in} ->
                                              {"(" ++ R ++ " " ++ OpResult ++ " " ++
                                                   ResultCode ++ ")",
                                               NextVariableNumberReturn,
                                               eog_datatype_setlists:append(SV, StateVariables)};
                                          
                                          {OpResult, in_nospaces} ->
                                              case ResultCode of
                                                  "" ->
                                                      {R, NextVariableNumberReturn,
                                                       eog_datatype_setlists:append(
                                                         SV, StateVariables)};
                                                  _ ->
                                                      {R ++ OpResult ++
                                                           eog_util:line_break() ++
                                                           eog_util_indenter:indent() ++
                                                           ResultCode,
                                                       NextVariableNumberReturn,
                                                       eog_datatype_setlists:append(SV, StateVariables)}
                                              end;
                                          
                                          {OpResult, function} ->
                                              {OpResult ++ "(" ++ R ++ ", " ++
                                                   ResultCode ++ ")",
                                               NextVariableNumberReturn, []};
                                          
                                          {OpResult, function_revert} ->
                                              {OpResult ++ "(" ++
                                                   ResultCode ++ ", " ++  R ++ ")",
                                               NextVariableNumberReturn,
                                               eog_datatype_setlists:append(
                                                 SV, StateVariables)}
                                      
                                      end
                              end
                      end,
                      {"", VariableNumberReturn, []}, Contents)
            end
    end;

get_standard_next_state([{expression, "IteratorExpImpl"} | Data],
                        VariableNumberReturn, Classes) ->
    Name = proplists:get_value(name, Data),

    case Name of
        "select" ->

            Iterator = proplists:get_value(iterator, Data),
            Variable = proplists:get_value(variable, Iterator),

            {ResultCodeVariable, _, _} = get_standard_next_state(
                                      Variable, VariableNumberReturn, Classes),
            Source = proplists:get_value(source, Data),
            SourceOperationType = eog_common_types:get_type(Source),
            {ResultCodeSource, _, _} = get_standard_next_state(
                                      Source, VariableNumberReturn, Classes),
            Body = proplists:get_value(body, Data),
            {ResultCodeBody, _, _} = get_standard_next_state(
                                      Body, VariableNumberReturn, Classes),
            
            Result = case SourceOperationType of
                    "Sequence" ->
                        "ocl_seq:";
                    _ ->
                        io:format("WARNING: Using default forAll for type: ~p ~n",
                                  [SourceOperationType]),
                        "ocl:"
                end ++ "filter(fun(" ++ ResultCodeVariable ++ ") -> " ++
                         ResultCodeBody ++ " end, " ++ ResultCodeSource ++ ")",
            {Result, VariableNumberReturn, []};
        
        _ ->
            io:format("WARNING: Unknown IteratorExpImpl: ~p ~n",
                      [Name]),
            {"<<IteratorExpImpl>>", VariableNumberReturn, []}
    end;

get_standard_next_state([{expression, "PropertyCallExp"} | Data],
                        VariableNumberReturn, Classes) -> 
    
    Source = proplists:get_value(source, Data),
    Property = proplists:get_value(referredProperty, Data),
    PropertyName = proplists:get_value(name, Property),
    {ResultCode, _, StateVariables} = get_standard_next_state(
                                        Source, VariableNumberReturn, Classes),
    case ResultCode of
        "" ->
            {ResultCode ++ eog_common_erlang:to_variable_state(PropertyName),
             VariableNumberReturn, StateVariables};
        _ ->
            {eog_common_erlang:to_field_name(ResultCode, PropertyName),
             VariableNumberReturn, StateVariables}
    end;

get_standard_next_state([{expression, "PropertyCallExpAssign"} | Data],
                        VariableNumberReturn, Classes) -> 
    Source = proplists:get_value(source, Data),
    Property = proplists:get_value(referredProperty, Data),
    PropertyName = proplists:get_value(name, Property),
    
    {ResultCode, _, StateVariables} = get_standard_next_state(Source,
                                                              VariableNumberReturn,
                                                              Classes),
    
    {ResultCode ++ eog_common_erlang:to_record_access(get_state_variable_name_from_number(VariableNumberReturn),
                                                      get_testing_state_record_name()) ++ " {" ++
         eog_util:line_break() ++ eog_util_indenter:indent(4) ++ 
         eog_common_erlang:to_field_name(PropertyName), VariableNumberReturn,
     eog_datatype_setlists:add_element([{expression,"PropertyCallExp"} | Data], StateVariables)};

get_standard_next_state([{expression, "VariableExp"} | Data],
                        VariableNumberReturn, Classes) -> 
    Variable = proplists:get_value(referredVariable, Data),
    get_standard_next_state(Variable, VariableNumberReturn, Classes);

get_standard_next_state([{expression, "IntegerLiteralExp"} | Data],
                        VariableNumberReturn, _Classes) -> 
    Value = proplists:get_value(value, Data),
    {eog_common_erlang:to_integer_value(Value), VariableNumberReturn, []};

get_standard_next_state([{expression, "BooleanLiteralExp"} | Data],
                        VariableNumberReturn, _Classes) -> 
    Value = proplists:get_value(value, Data),
    {eog_common_erlang:to_boolean_value(Value), VariableNumberReturn, []};

get_standard_next_state([{expression, "UndefinedLiteralExp"} | _Data],
                        VariableNumberReturn, _Classes) -> 
    {"undefined", VariableNumberReturn, []};

get_standard_next_state([{expression, "Variable"} | Data],
                        VariableNumberReturn, _Classes) -> 
    Name = proplists:get_value(name, Data),
    case Name of
        "self" ->
            {"", VariableNumberReturn, []};
        _ ->
            {eog_common_erlang:to_variable_name(Name), VariableNumberReturn, []}
    end;

get_standard_next_state([], VariableNumberReturn, _Classes)->
    {"", VariableNumberReturn, []}.
