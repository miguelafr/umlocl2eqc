%% Author: miguelafr
%% Created: 26/02/2012
%% Description: eog_common_contraints
-module(eog_stateless_constraints).

%%====================================================================

%% Include files
%%====================================================================

%%====================================================================
%% Exported functions
%%====================================================================
-export([get_constraint/3]).

%%====================================================================
%% API
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
            "ocl_set:new_set(" ++ RangeCode ++ ")";
        "Sequence"->
            "ocl_seq:new_seq(" ++ RangeCode ++ ")";
        _ ->
            io:format("WARNING: Using default collection constructor for type: ~p ~n",
                      [Kind]),
            "xxxxx()"
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
            
            ResultCodeBody = get_constraint(
                               proplists:get_value(body, Data),
                               Classes, ConstraintType),
            
            ResultCodeEnd = eog_util:line_break() ++ eog_util_indenter:indent(-4) ++
                                "end, " ++ eog_util:line_break() ++ eog_util_indenter:indent(0) ++
                                eog_common_erlang:to_variable_name(SourceVariableName) ++
                                eog_util:line_break() ++ eog_util_indenter:indent(-4) ++ ")",
            
            ResultCodeFun ++ ResultCodeBody ++ ResultCodeEnd;
        _ ->
            io:format("WARNING: Unknown IteratorExpImpl: ~p ~n",
                      [Name]),
            "<<IteratorExpImpl>>"
    end;

get_constraint([{expression, "IterateExpImpl"} | Data], Classes, ConstraintType) -> 
    
    Source = proplists:get_value(source, Data),
    SourceVariableName = get_constraint(Source, Classes, ConstraintType),
    SourceVariableType = eog_common_types:get_type(Source),
    
    Iterator = proplists:get_value(iterator, Data),
    IteratorVariable = proplists:get_value(variable, Iterator),
    IteratorVariableName = proplists:get_value(name, IteratorVariable),
    
    Acc = proplists:get_value(result, Data),
    AccName = proplists:get_value(name, Acc),
    AccType = proplists:get_value(type, Acc),
    AccInitExpression = get_constraint(
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
    
    ResultCodeBody = get_constraint(
                       proplists:get_value(body, Data),
                       Classes, ConstraintType),
    
    ResultCodeEnd = eog_util:line_break() ++ eog_util_indenter:indent(-4) ++
                        "end, " ++ eog_util:line_break() ++ eog_util_indenter:indent(0) ++
                        AccInitExpression ++ ", " ++ SourceVariableName ++
                        eog_util:line_break() ++ eog_util_indenter:indent(-4) ++ ")",
    
    ResultCodeFun ++ ResultCodeBody ++ ResultCodeEnd;

get_constraint([{expression, "IfExp"} | Data], Classes, ConstraintType) ->
    
    Condition = proplists:get_value(condition, Data),
    Then = proplists:get_value(then, Data),
    Else = proplists:get_value(else, Data),
    
    ResultCodeIf = "case ",
    
    ResultCodeCondition = get_constraint(Condition, Classes, ConstraintType),
    
    ResultCodeSep1 = " of " ++ eog_util:line_break() ++ eog_util_indenter:indent(4) ++
                         "true -> " ++ eog_util:line_break() ++ eog_util_indenter:indent(4),
    
    ResultCodeThen = get_constraint(Then, Classes, ConstraintType),
    
    ResultCodeSep2 = ";" ++ eog_util:line_break() ++ eog_util_indenter:indent(-4) ++
                         "false -> " ++ eog_util:line_break() ++ eog_util_indenter:indent(4),
    
    ResultCodeElse = get_constraint(Else, Classes, ConstraintType),
    
    ResultCodeEndIf = eog_util:line_break() ++ eog_util_indenter:indent(-8) ++
                          "end",
    
    ResultCodeIf ++ ResultCodeCondition ++ ResultCodeSep1 ++
        ResultCodeThen ++ ResultCodeSep2 ++ ResultCodeElse ++ ResultCodeEndIf;

get_constraint(Expression = [{expression, "OperationCallExp"} | _Data],
               Classes, ConstraintType) ->    
    get_constraint_operation(Expression, Classes, ConstraintType);

get_constraint([{expression, "PropertyCallExp"} | Data],
               Classes, ConstraintType) -> 
    
    Source = proplists:get_value(source, Data),
    Property = proplists:get_value(referredProperty, Data),
    PropertyName = proplists:get_value(name, Property),
    
    ResultCode = get_constraint(Source, Classes, ConstraintType),
    
    eog_common_erlang:to_field_name(ResultCode, PropertyName);

get_constraint([{expression, "VariableExp"} | Data],
               _Classes, _ConstraintType) -> 
    
    Variable = proplists:get_value(referredVariable, Data),
    VariableName = proplists:get_value(name, Variable),
    case VariableName of
        "self" ->
            "";
        _ ->
            eog_common_erlang:to_variable_name(VariableName)
    end;

get_constraint([{expression, "TypeLiteralExp"} | Data],
               _Classes, _ConstraintType) ->
    
    Value = proplists:get_value(value, Data),
    eog_common_erlang:to_module_name(Value);

get_constraint([{expression, "BooleanLiteralExp"} | Data],
               _Classes, _ConstraintType) ->
    
    Value = proplists:get_value(value, Data),
    erlang:atom_to_list(Value);

get_constraint([{expression, "IntegerLiteralExp"} | Data],
               _Classes, _ConstraintType) ->
    
    Value = proplists:get_value(value, Data),
    erlang:integer_to_list(Value);

get_constraint([{expression, "UndefinedLiteralExp"} | _Data],
               _Classes, _ConstraintType) -> 
    
    "undefined";

get_constraint([], _Classes, _ConstraintType)->
    "".

%%====================================================================
%% Internal functions
%%====================================================================
%%--------------------------------------------------------------------
%% get_collection_range(Range, [Class], ConstraintType) -> string()
%%--------------------------------------------------------------------
get_collection_range(Range, Classes, ConstraintType) ->
    case Range of
        undefined ->
            "";
        _ ->
            First = get_constraint(proplists:get_value(first, Range),
                                   Classes, ConstraintType),
            Last = get_constraint(proplists:get_value(last, Range),
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
get_constraint_operation([{expression, "OperationCallExp"} | _Data],
                         Classes, ConstraintType, OperationName,
                         [Content] = [{content, Exp}]) ->  
    
    case eog_common_operation:get_operation(OperationName, [Content], Classes) of
        {OpResult, function} ->
            ResultCode = get_constraint(Exp, Classes, ConstraintType),
            OpResult ++ "(" ++ ResultCode ++ ")"
    end;

get_constraint_operation(_Constraint = [{expression, "OperationCallExp"} | _Data],
                         Classes, ConstraintType, OperationName, Contents) ->  
    
    lists:foldl(
      fun({content, Exp}, R) ->
              case R of
                  "" ->
                      ResultCode = get_constraint(Exp, Classes, ConstraintType),
                      ResultCode;
                  _ ->
                      ResultCode = get_constraint(Exp, Classes, ConstraintType),
                      case eog_common_operation:get_operation(OperationName, Contents, Classes) of
                          {OpResult, in} ->
                              "(" ++ R ++ " " ++ OpResult ++ eog_util:line_break() ++ eog_util_indenter:indent() ++ ResultCode ++ ")";
                          {OpResult, in_not} ->
                              "(not (" ++ R ++ ") " ++ OpResult ++ eog_util:line_break() ++ eog_util_indenter:indent() ++
                                  ResultCode ++ ")";
                          {OpResult, function} ->
                              OpResult ++ "(" ++ R ++ ", " ++
                                  ResultCode ++ ")";
                          {OpResult, function_revert} ->
                              OpResult ++ "(" ++
                                  ResultCode ++ ", " ++  R ++ ")"
                      end
              end
      end,
      "", Contents).