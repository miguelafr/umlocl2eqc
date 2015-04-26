-module(eog_statem_constraints).

%% API
-export([get_constraint/3, get_constraints/4, get_constraint_default/4]).

%%====================================================================
%% API
%% - Operation
%% - OperationData
%% - Constraint
%% - Class
%% - Variable
%% - ConstraintType
%%====================================================================
%%--------------------------------------------------------------------
%% get_constraint(Expression, [Class], ConstraintType) ->
%%      {string(), [StateVariable], [StateVariable], [Variable]}
%%--------------------------------------------------------------------
get_constraint([{expression, "CollectionLiteralExp"} | Data],
               Classes, ConstraintType) -> 
    
    Kind = proplists:get_value(kind, Data),
    Range = proplists:get_value(range, Data),
    RangeCode = get_collection_range(Range, Classes, ConstraintType),
    
    case Kind of
        "Set" ->
            {"ocl_set:new_set(" ++ RangeCode ++ ")", [], [], []};
        "Sequence"->
            {"ocl_seq:new_seq(" ++ RangeCode ++ ")", [], [], []};
        _ ->
            io:format("WARNING: Using default collection constructor for type: ~p ~n",
                      [Kind]),
            {"xxxxx()", [], [], []}
    end;    

get_constraint([{expression, "IteratorExpImpl"} | Data],
               Classes, ConstraintType) ->
    
    Name = proplists:get_value(name, Data),

    case Name of
        "forAll" ->
            
            Source = proplists:get_value(source, Data),
            SourceVariable = proplists:get_value(referredVariable, Source),
            SourceVariableName = proplists:get_value(name, SourceVariable),
            SourceVariableType = eog_common_types:get_type(
                                   proplists:get_value(type, SourceVariable)),
            
            Iterator = proplists:get_value(iterator, Data),
            IteratorVariable = proplists:get_value(variable, Iterator),
            IteratorVariableName = proplists:get_value(name, IteratorVariable),
            
            ResultCodeFun =
                case SourceVariableType of
                    "Sequence" ->
                        "ocl_seq:forAll";
                    _ ->
                        io:format("WARNING: Using default forAll for type: ~p ~n",
                                  [SourceVariableType]),
                        "ocl:forAll"
                end ++
                    "(" ++ eog_util:line_break() ++ eog_util_indenter:indent(4) ++
                    "fun(" ++ eog_common_erlang:to_variable_name(IteratorVariableName) ++ ") -> " ++
                    eog_util:line_break() ++ eog_util_indenter:indent(4),
            
            Body = proplists:get_value(body, Data),
            {ResultCodeBody, _, _, _} = get_constraint(Body, Classes,
                                                       ConstraintType),
            
            ResultCodeEnd = eog_util:line_break() ++ eog_util_indenter:indent(-4) ++
                                "end, " ++ eog_util:line_break() ++ eog_util_indenter:indent(0) ++
                                eog_common_erlang:to_variable_name(SourceVariableName) ++
                                eog_util:line_break() ++ eog_util_indenter:indent(-4) ++ ")",
            
            {ResultCodeFun ++ ResultCodeBody ++ ResultCodeEnd, [], [], []};
        
        "select" ->

            Iterator = proplists:get_value(iterator, Data),
            Variable = proplists:get_value(variable, Iterator),

            {ResultCodeVariable, _, _, _} = get_constraint(
                                      Variable, Classes, ConstraintType),
            Source = proplists:get_value(source, Data),
            SourceOperationType = eog_common_types:get_type(Source),

            {ResultCodeSource, _, _, _} = get_constraint(
                                      Source, Classes, ConstraintType),
            Body = proplists:get_value(body, Data),
            {ResultCodeBody, _, _, _} = get_constraint(
                                      Body, Classes, ConstraintType),
            
            Result = case SourceOperationType of
                    "Sequence" ->
                        "ocl_seq:";
                    _ ->
                        io:format("WARNING: Using default forAll for type: ~p ~n",
                                  [SourceOperationType]),
                        "ocl:"
                end ++
                         "filter(fun(" ++ ResultCodeVariable ++ ") -> " ++
                         ResultCodeBody ++ " end, " ++ ResultCodeSource ++ ")",
            {Result, [], [], []};
        
        _ ->
            io:format("WARNING: Unknown IteratorExpImpl: ~p ~n",
                      [Name]),
            {"<<IteratorExpImpl>>", [], [], []}
    end;

get_constraint([{expression, "IterateExpImpl"} | Data], Classes, ConstraintType) -> 
    
    Source = proplists:get_value(source, Data),
    {SourceVariableName, _, _, _} = get_constraint(Source, Classes, ConstraintType),
    SourceVariableType = eog_common_types:get_type(Source),
    
    Iterator = proplists:get_value(iterator, Data),
    IteratorVariable = proplists:get_value(variable, Iterator),
    IteratorVariableName = proplists:get_value(name, IteratorVariable),
    
    Acc = proplists:get_value(result, Data),
    AccName = proplists:get_value(name, Acc),
    AccType = proplists:get_value(type, Acc),
    {AccInitExpression, _, _, _} = get_constraint(
                                     proplists:get_value(initExpression, AccType),
                                     Classes, ConstraintType),
    
    ResultCodeFun =
        case SourceVariableType of
            "Sequence" ->
                "ocl_seq:iterate";
            _ ->
                io:format("WARNING: Using default IterateExpImpl for type: ~p ~n",
                          [SourceVariableType]),
                "ocl:iterate"
        end ++
            "(" ++ eog_util:line_break() ++ eog_util_indenter:indent(4) ++
            "fun(" ++ eog_common_erlang:to_variable_name(IteratorVariableName) ++ ", " ++ 
            eog_common_erlang:to_variable_name(AccName) ++ ") -> " ++
            eog_util:line_break() ++ eog_util_indenter:indent(4),
    
    {ResultCodeBody, _, _, _} = get_constraint(
                                  proplists:get_value(body, Data),
                                  Classes, ConstraintType),
    
    ResultCodeEnd = eog_util:line_break() ++ eog_util_indenter:indent(-4) ++
                        "end, " ++ eog_util:line_break() ++ eog_util_indenter:indent(0) ++
                        AccInitExpression ++ ", " ++ SourceVariableName ++
                        eog_util:line_break() ++ eog_util_indenter:indent(-4) ++ ")",
    
    {ResultCodeFun ++ ResultCodeBody ++ ResultCodeEnd, [], [], []};

get_constraint([{expression, "IfExp"} | Data], Classes, ConstraintType) ->
    
    Condition = proplists:get_value(condition, Data),
    Then = proplists:get_value(then, Data),
    Else = proplists:get_value(else, Data),
    
    ResultCodeIf = "case ",
    
    {ResultCodeCondition, StateVariablesConditionPrev,
     StateVariablesConditionAfter, VariablesCondition} =
        get_constraint(Condition, Classes, ConstraintType),
    
    ResultCodeSep1 = " of " ++ eog_util:line_break() ++ eog_util_indenter:indent(4) ++
                         "true -> " ++ eog_util:line_break() ++ eog_util_indenter:indent(4),
    
    {ResultCodeThen, StateVariablesThenPrev, StateVariablesThenAfter,
     VariablesThen} = get_constraint(Then, Classes, ConstraintType),
    
    ResultCodeSep2 = ";" ++ eog_util:line_break() ++ eog_util_indenter:indent(-4) ++
                         "false -> " ++ eog_util:line_break() ++ eog_util_indenter:indent(4),
    
    {ResultCodeElse, StateVariablesElsePrev, StateVariablesElseAfter,
     VariablesElse} = get_constraint(Else, Classes, ConstraintType),
    
    ResultCodeEndIf = eog_util:line_break() ++ eog_util_indenter:indent(-8) ++
                          "end",
    
    {ResultCodeIf ++ ResultCodeCondition ++ ResultCodeSep1 ++
         ResultCodeThen ++ ResultCodeSep2 ++ ResultCodeElse ++ ResultCodeEndIf,
     eog_datatype_setlists:append(
       eog_datatype_setlists:append(StateVariablesConditionPrev, StateVariablesThenPrev),
       StateVariablesElsePrev),
     eog_datatype_setlists:append(
       eog_datatype_setlists:append(StateVariablesConditionAfter, StateVariablesThenAfter),
       StateVariablesElseAfter),
     eog_datatype_setlists:append(
       eog_datatype_setlists:append(VariablesCondition, VariablesThen),
       VariablesElse)};

get_constraint(Expression = [{expression, "OperationCallExp"} | Data],
               Classes, ConstraintType) ->    
    
    Operation = proplists:get_value(referredOperation, Data),
    OperationName = proplists:get_value(name, Operation),
    
    Contents = proplists:get_value(contents, Data),
    
    case OperationName of
        "atPre" ->
            [{content, ContentPre}] = Contents,
            case ContentPre of
                [{expression, "PropertyCallExp"} | PropertyDataPre] ->
                    ResultCode = eog_statem_state:get_state_access(
                                   PropertyDataPre),
                    {ResultCode, add_state_variable(ContentPre, []), [], []};
                [{expression, "OperationCallExp"} | _OpDataPre] ->
                    ResultCode = eog_statem_state:get_dyn_state_access(
                                   Expression),
                    {ResultCode, add_state_variable(ContentPre, []), [], []}
            end;
        _ ->
            get_constraint_operation(Expression, Classes, ConstraintType)
    end;

get_constraint(Expression = [{expression, "PropertyCallExp"} | Data],
               Classes, ConstraintType = preconditions) -> 
    
    Source = proplists:get_value(source, Data),
    Property = proplists:get_value(referredProperty, Data),
    PropertyName = proplists:get_value(name, Property),
    
    {ResultCode, StateVariablesPrev, StateVariablesAfter, Variables} =
        get_constraint(Source, Classes, ConstraintType),

    {ResultCode ++ eog_common_erlang:to_variable_state(PropertyName),
     StateVariablesPrev, add_state_variable(Expression, StateVariablesAfter),
     Variables};

get_constraint(Expression = [{expression, "PropertyCallExp"} | Data],
               Classes, ConstraintType) -> 
    
    Source = proplists:get_value(source, Data),
    Property = proplists:get_value(referredProperty, Data),
    PropertyName = proplists:get_value(name, Property),
    {ResultCode, _, _, _} = get_constraint(
                                        Source, Classes, ConstraintType),
    case ResultCode of
        "" ->
            {eog_statem_state:get_after_state_access(Expression), [],
             add_state_variable(Expression, []), []};
        _ ->
            {eog_common_erlang:to_field_name(ResultCode, PropertyName),
             [], [], []}
    end;

get_constraint([{expression, "VariableExp"} | Data],
               Classes, ConstraintType) -> 
    
    Variable = proplists:get_value(referredVariable, Data),
    get_constraint(Variable, Classes, ConstraintType);

get_constraint([{expression, "TypeLiteralExp"} | Data],
               _Classes, _ConstraintType) ->
    
    Value = proplists:get_value(value, Data),
    {eog_common_erlang:to_module_name(Value), [], [], []};

get_constraint([{expression, "BooleanLiteralExp"} | Data],
               _Classes, _ConstraintType) ->
    
    Value = proplists:get_value(value, Data),
    {erlang:atom_to_list(Value), [], [], []};

get_constraint([{expression, "IntegerLiteralExp"} | Data],
               _Classes, _ConstraintType) ->
    
    Value = proplists:get_value(value, Data),
    {erlang:integer_to_list(Value), [], [], []};

get_constraint([{expression, "UndefinedLiteralExp"} | _Data],
               _Classes, _ConstraintType) -> 
    
    {"undefined", [], [], []};

get_constraint([{expression, "Variable"} | Data],
               _Classes, _ConstraintType) -> 
    Name = proplists:get_value(name, Data),
    case Name of
        "self" ->
            {"", [], [], []};
        _ ->
            {eog_common_erlang:to_variable_name(Name), [], [], []}
    end;

get_constraint([], _Classes, _ConstraintType)->
    {"", [], [], []}.


%%--------------------------------------------------------------------
%% get_constraints([Operation], [Class], ConstraintType) -> string()
%%--------------------------------------------------------------------
get_constraints(Operations, _StateVariables, Classes,
                ConstraintType = postconditions) ->
    eog_util:line_break() ++ eog_util_indenter:set_value(0) ++ eog_util_indenter:set_value(0) ++
        "postcondition(" ++ eog_statem_state:get_state_name() ++ ", C, R)->" ++
        eog_util:line_break() ++ eog_util_indenter:indent(4) ++
        eog_statem_state:get_after_state_name() ++ " = " ++
        "eqc_symbolic:eval(next_state(" ++ eog_statem_state:get_state_name() ++ ", R, C)), " ++
        eog_util:line_break() ++ eog_util_indenter:indent() ++
        "postcondition(" ++ eog_statem_state:get_state_name() ++ ", " ++
        eog_statem_state:get_after_state_name() ++ ", C, R)." ++
        eog_util:line_break() ++ eog_util_indenter:indent(-4) ++
        get_specific_constraints(Operations, Classes, ConstraintType,
                                 standard);

get_constraints(Operations, StateVariables, Classes,
                ConstraintType = preconditions) ->
    case eog_statem_state:use_dynamic_preconditions(Operations, StateVariables, Classes) of
        true ->
            eog_util:line_break() ++ eog_util_indenter:set_value(0) ++ eog_util_indenter:set_value(0) ++
                "precondition(_" ++ eog_statem_state:get_state_name() ++ ", _C) ->" ++
                eog_util:line_break() ++ eog_util_indenter:indent(4) ++
                "true." ++
                eog_util:line_break() ++ eog_util_indenter:indent(-4) ++
                get_specific_constraints(Operations, Classes, ConstraintType,
                                         dynamic);
        false ->
            get_specific_constraints(Operations, Classes, ConstraintType,
                                     standard)
    end.

%%--------------------------------------------------------------------
%% get_constraint_default(ConstraintType) -> string()
%%--------------------------------------------------------------------
get_constraint_default(_Operations, _StateVariables, _Classes,
                       ConstraintType = postconditions) ->
    eog_common_comment:get_function_header(erlang:atom_to_list(ConstraintType), "default") ++
        get_constraints_function_definition_default(ConstraintType, standard) ++
        eog_util:line_break() ++ eog_util_indenter:set_value(4) ++
        "false." ++ eog_util:line_break();

get_constraint_default(Operations, StateVariables, Classes,
                       ConstraintType = preconditions) ->
    eog_common_comment:get_function_header(erlang:atom_to_list(ConstraintType), "default") ++
        case eog_statem_state:use_dynamic_preconditions(Operations, StateVariables, Classes) of
            true ->
                get_constraints_function_definition_default(ConstraintType, dynamic);
            false ->
                get_constraints_function_definition_default(ConstraintType, standard)
        end ++
        eog_util:line_break() ++ eog_util_indenter:set_value(4) ++
        "false." ++ eog_util:line_break().

%%====================================================================
%% Internal functions
%%====================================================================
%%--------------------------------------------------------------------
%% exist_class
%%--------------------------------------------------------------------
exist_class(ClassName, [{class, ClassData} | MoreClasses])->
    case proplists:get_value(name, ClassData) of
        ClassName ->
            true;
        _ ->
            exist_class(ClassName, MoreClasses)
    end;
exist_class(_ClassName, [])->
    false.

%%--------------------------------------------------------------------
%% add_state_variable(StateVariable, [StateVariable]) -> [StateVariable]
%%--------------------------------------------------------------------
add_state_variable(StateVariable, StateVariables) ->
    eog_datatype_setlists:add_element(StateVariable, StateVariables).

%%--------------------------------------------------------------------
%% get_collection_range(Range, [Class], ConstraintType) -> string()
%%--------------------------------------------------------------------
get_collection_range(Range, Classes, ConstraintType) ->
    case Range of
        undefined ->
            "";
        _ ->
            {First, _, _, _} = get_constraint(proplists:get_value(first, Range),
                                              Classes, ConstraintType),
            {Last, _, _, _} = get_constraint(proplists:get_value(last, Range),
                                             Classes, ConstraintType),
            First ++ ", " ++ Last
    end.

%%--------------------------------------------------------------------
%% get_constraint_operation(Constraint, [Class], ConstraintType) ->
%%      {string(), [StateVariable], [Variable]}
%%--------------------------------------------------------------------
get_constraint_operation(Constraint = [{expression, "OperationCallExp"} | Data],
                         Classes, ConstraintType) ->  
    
    Operation = proplists:get_value(referredOperation, Data),
    OperationName = proplists:get_value(name, Operation),
    Contents = proplists:get_value(contents, Data),
    
    get_constraint_operation(Constraint, Classes, ConstraintType, OperationName,
                             Contents).

%%--------------------------------------------------------------------
%% get_constraint_operation(Constraint, [Class], ConstraintType, string(), ConstraintData) ->
%%      {string(), [StateVariable], [Variable]}
%%--------------------------------------------------------------------
get_constraint_operation(Constraint = [{expression, "OperationCallExp"} | _Data],
                         Classes, ConstraintType = preconditions, OperationName,
                         [Content] = [{content, Exp}]) ->  
    
    ContentType = eog_common_types:get_type(Content),
    case exist_class(ContentType, Classes) of
        true ->
            {eog_statem_state:get_state_access(Constraint),
             [], add_state_variable(Constraint, []), []};
        false ->
            case eog_common_operation:get_operation(OperationName, [Content], Classes) of
                {OpResult, function} ->
                    {ResultCode, StateVariablesPrev, StateVariablesAfter, Variables} =
                        get_constraint(Exp, Classes, ConstraintType),
                    {OpResult ++ "(" ++ ResultCode ++ ")",
                     StateVariablesPrev, StateVariablesAfter, Variables}
            end
    end;

get_constraint_operation(Constraint = [{expression, "OperationCallExp"} | _Data],
                         Classes, ConstraintType = postconditions, OperationName,
                         [Content] = [{content, Exp}]) ->  
    
    ContentType = eog_common_types:get_type(Content),
    case exist_class(ContentType, Classes) of
        true ->
            {eog_statem_state:get_after_state_access(Constraint),
             [], add_state_variable(Constraint, []), []};
        false ->
            case eog_common_operation:get_operation(OperationName, [Content], Classes) of
                {OpResult, function} ->
                    {ResultCode, StateVariablesPrev, StateVariablesAfter, Variables} =
                        get_constraint(Exp, Classes, ConstraintType),
                    {OpResult ++ "(" ++ ResultCode ++ ")",
                     StateVariablesPrev, StateVariablesAfter, Variables}
            end
    end;

get_constraint_operation(_Constraint = [{expression, "OperationCallExp"} | _Data],
                         Classes, ConstraintType, OperationName, Contents) ->  
    
    lists:foldl(
      fun({content, Exp}, {R, StateVariablePrev, StateVariableAfter, Variable}) ->
              case R of
                  "" ->
                      {ResultCode, StateVariablesPrev, StateVariablesAfter,
                       Variables} = get_constraint(Exp, Classes, ConstraintType),
                      {ResultCode,
                       eog_datatype_setlists:append(StateVariablePrev, StateVariablesPrev),
                       eog_datatype_setlists:append(StateVariableAfter, StateVariablesAfter),
                       eog_datatype_setlists:append(Variable, Variables)};
                  _ ->
                      {ResultCode, StateVariablesPrev, StateVariablesAfter,
                       Variables} = get_constraint(Exp, Classes, ConstraintType),
                      case eog_common_operation:get_operation(OperationName, Contents, Classes) of
                          {OpResult, in} ->
                              {"(" ++ R ++ " " ++ OpResult ++ " " ++
                                   ResultCode ++ ")",
                               eog_datatype_setlists:append(StateVariablePrev, StateVariablesPrev),
                               eog_datatype_setlists:append(StateVariableAfter, StateVariablesAfter),
                               eog_datatype_setlists:append(Variable, Variables)};
                          {OpResult, in_not} ->
                              {"(not (" ++ R ++ ") " ++ OpResult ++
                                   eog_util:line_break() ++ eog_util_indenter:indent() ++
                                   ResultCode ++ ")",
                               eog_datatype_setlists:append(StateVariablePrev, StateVariablesPrev),
                               eog_datatype_setlists:append(StateVariableAfter, StateVariablesAfter),
                               eog_datatype_setlists:append(Variable, Variables)};
                          {OpResult, function} ->
                              {OpResult ++ "(" ++ R ++ ", " ++
                                   ResultCode ++ ")",
                               eog_datatype_setlists:append(StateVariablePrev, StateVariablesPrev),
                               eog_datatype_setlists:append(StateVariableAfter, StateVariablesAfter),
                               eog_datatype_setlists:append(Variable, Variables)};
                          {OpResult, function_revert} ->
                              {OpResult ++ "(" ++
                                   ResultCode ++ ", " ++  R ++ ")",
                               eog_datatype_setlists:append(StateVariablePrev, StateVariablesPrev),
                               eog_datatype_setlists:append(StateVariableAfter, StateVariablesAfter),
                               eog_datatype_setlists:append(Variable, Variables)}
                      end
              end
      end,
      {"", [], [], []}, Contents).

%%--------------------------------------------------------------------
%% get_specific_constraints([Operation], [Class], ConstraintType) -> string()
%%--------------------------------------------------------------------
get_specific_constraints([{operation, OperationData} | MoreOperations], Classes,
                         ConstraintType, Subtype) ->
    
    Name = proplists:get_value(name, OperationData),
    Constraints = proplists:get_value(constraints, OperationData),
    SpecificConstraints = eog_common_constraints:group_constraints(
                            proplists:get_value(
                              erlang:list_to_atom(erlang:atom_to_list(ConstraintType)),
                              Constraints)),
    
    eog_common_comment:get_function_header(Name, erlang:atom_to_list(ConstraintType)) ++
        case SpecificConstraints of
            [] ->
                get_constraints_empty(OperationData, ConstraintType, Subtype);
            _ ->
                get_constraints_constraint(OperationData, SpecificConstraints,
                                           Classes, ConstraintType, Subtype)
        end ++
        get_specific_constraints(MoreOperations, Classes, ConstraintType, Subtype);  

get_specific_constraints([], _Classes, _ConstraintType, _Subtype) ->
    "".

%%--------------------------------------------------------------------
%% get_constraints_constraint(OperationData, [Constraint], [Class], ConstraintType) -> string()
%%--------------------------------------------------------------------
get_constraints_constraint(OperationData,
                           [{constraint, ConstraintData} | MoreConstraints],
                           Classes, ConstraintType, Subtype) ->
    
    Body = proplists:get_value(body, ConstraintData),
    
    ResultTmp = eog_util_indenter:set_value(4),
    
    {ResultCode, _StateVariablesPrev, _StateVariablesAfter, Variables} =
        get_constraint(Body, Classes, ConstraintType),
    
    ResultTmp ++
        get_constraints_function_definition(OperationData, Variables,
                                            ConstraintType, Subtype) ++
        eog_util:line_break() ++ eog_util_indenter:set_value(4) ++
        ResultCode ++ ";" ++ eog_util:line_break() ++
        get_constraints_constraint(OperationData, MoreConstraints,
                                   Classes, ConstraintType, Subtype);

get_constraints_constraint(_OperationData, [], _Classes, _ConstraintType,
                           _Subtype) ->
    "".

%%--------------------------------------------------------------------
%% get_constraints_empty
%%--------------------------------------------------------------------
get_constraints_empty(OperationData, ConstraintType, Subtype) ->
    get_constraints_function_definition(OperationData, [], ConstraintType, Subtype) ++
        eog_util:line_break() ++ eog_util_indenter:set_value(4) ++
        "true;" ++ eog_util:line_break().

%%--------------------------------------------------------------------
%% get_constraints_function_definition_default
%%--------------------------------------------------------------------
get_constraints_function_definition_default(preconditions, dynamic) ->
    eog_util:line_break() ++ eog_util_indenter:set_value(0) ++
        "dynamic_precondition(_" ++ eog_statem_state:get_state_name() ++ ", _C) ->";

get_constraints_function_definition_default(preconditions, _Subtype) ->
    eog_util:line_break() ++ eog_util_indenter:set_value(0) ++
        "precondition(_" ++ eog_statem_state:get_state_name() ++ ", _C) ->";

get_constraints_function_definition_default(postconditions, _Subtype) ->
    eog_util:line_break() ++ eog_util_indenter:set_value(0) ++
        "postcondition(_" ++ eog_statem_state:get_state_name() ++ ", _" ++
        eog_statem_state:get_after_state_name() ++ ", _C, _R) ->".

%%--------------------------------------------------------------------
%% get_constraints_function_definition(OperationData, [Variable], ConstraintType) -> string()
%%--------------------------------------------------------------------
get_constraints_function_definition(OperationData, _Variables, preconditions,
                                    dynamic) ->
    eog_util:line_break() ++ eog_util_indenter:set_value(0) ++
        "dynamic_precondition(" ++ eog_statem_state:get_state_name() ++ ", " ++
        get_function_call(OperationData) ++ ")->";

get_constraints_function_definition(OperationData, _Variables, preconditions,
                                    _Subtype) ->
    eog_util:line_break() ++ eog_util_indenter:set_value(0) ++
        "precondition(" ++ eog_statem_state:get_state_name() ++ ", " ++
        get_function_call(OperationData) ++ ")->";

get_constraints_function_definition(OperationData, _Variables, postconditions,
                                    _Subtype) ->
    PostconditionParams = [[{name, eog_statem_state:get_dyn_state_name()}]] ++
                              [[{name, eog_statem_state:get_dyn_after_state_name()}]],
    eog_util:line_break() ++ eog_util_indenter:set_value(0) ++
        "postcondition(" ++ eog_statem_state:get_state_name() ++ ", " ++
        eog_statem_state:get_after_state_name() ++ ", " ++
        get_function_call(OperationData) ++ ", " ++
        get_function_call_result(PostconditionParams) ++ ")->".

%%--------------------------------------------------------------------
%% get_function_call
%%--------------------------------------------------------------------
get_function_call(OperationData)->
    Name = proplists:get_value(name, OperationData),
    Params = proplists:get_value(params, OperationData),
    
    "{call, ?MODULE, " ++ eog_common_erlang:to_function_name(Name) ++ ", [" ++
        get_function_call_params(Params) ++ "]}".

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

%%--------------------------------------------------------------------
%% get_function_call_result
%%--------------------------------------------------------------------
get_function_call_result(Params)->
    "{Result" ++ lists:foldl(
      fun(Param, Result) -> 
              Name = proplists:get_value(name, Param),
              Result ++ ", " ++ eog_common_erlang:to_variable_name(Name)
      end,
      "", Params) ++ "}".