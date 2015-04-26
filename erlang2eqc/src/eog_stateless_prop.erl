%% Author: miguelafr
%% Created: 26/02/2012
%% Description: eog_stateless_prop
-module(eog_stateless_prop).

%%====================================================================
%% Include files
%%====================================================================

%%====================================================================
%% Exported functions
%%====================================================================
-export([get_property_name/2, get_property/4, get_test_function_name/0,
         get_test_function/1]).

%%====================================================================
%% API
%%====================================================================
%%--------------------------------------------------------------------
%% get_property_name
%%--------------------------------------------------------------------
get_property_name(OperationName, PostconditionName)->
    eog_common_erlang:to_function_name("prop_" ++ OperationName ++ "_" ++
                                           PostconditionName).

%%--------------------------------------------------------------------
%% get_property
%%--------------------------------------------------------------------
get_property(ClassName, OperationData, PostconditionData, Classes)->
    OperationName = proplists:get_value(name, OperationData),
    Params = proplists:get_value(params, OperationData),
    Constraints = proplists:get_value(constraints, OperationData),
    
    Preconditions = eog_common_constraints:group_constraints(
                      proplists:get_value(preconditions, Constraints)),
    PreconditionData = case Preconditions of
        [] ->
            [];
        [{constraint, ConstraintData}] ->
            ConstraintData
    end,
    PreconditionBody = proplists:get_value(body, PreconditionData),
    
    PostconditionName = proplists:get_value(name, PostconditionData),
    PostconditionBody = proplists:get_value(body, PostconditionData),

    ForAllBegin = get_property_name(OperationName, PostconditionName) ++ "()->" ++
        eog_util:line_break() ++ eog_util_indenter:indent(4) ++
        "?FORALL({" ++ get_params(Params) ++ "}, " ++
        eog_util:line_break() ++ eog_util_indenter:indent(4),
    
    ForAllFilter =
        case Preconditions of
            [] ->
                "{" ++ get_generators(Params )++ "},";
            _ ->                 
                FilterBegin =
                    "?SUCHTHAT({" ++ get_params(Params) ++ "}, " ++
                    "{" ++ get_generators(Params )++ "}," ++
                    eog_util:line_break() ++ eog_util_indenter:indent(4) ++
                    "begin" ++
                    eog_util:line_break() ++ eog_util_indenter:indent(4),

                ErlangPrecondition =
                    eog_stateless_constraints:get_constraint(PreconditionBody,
                                                          Classes,
                                                          preconditions),
                
                FilterEnd = eog_util:line_break() ++ eog_util_indenter:indent(-4) ++
                    "end"  ++
                    eog_util:line_break() ++ eog_util_indenter:indent(-4) ++ "),",
                
                FilterBegin ++ ErlangPrecondition ++ FilterEnd
        end,
                
    ForAllPropBegin =
        eog_util:line_break() ++ eog_util_indenter:indent() ++
        "begin" ++
        eog_util:line_break() ++ eog_util_indenter:indent(4) ++
        "Result = " ++ eog_common_erlang:to_module_name(ClassName) ++ ":" ++
        OperationName ++ "(" ++ get_params(Params) ++ ")," ++
        eog_util:line_break()  ++ eog_util_indenter:indent(),
        
    ErlangPostcondition =
        eog_stateless_constraints:get_constraint(PostconditionBody, Classes,
            postconditions),
    
    ForAllPropEnd = eog_util:line_break() ++ eog_util_indenter:indent(-4) ++
        "end",

    ForAllEnd = eog_util:line_break() ++ eog_util_indenter:indent(-4) ++ ")." ++
        eog_util:line_break() ++ eog_util_indenter:indent(-4) ++
        eog_util:line_break(),
    
    eog_common_comment:get_function_header("property",
            get_property_name(OperationName, PostconditionName)) ++
            eog_util:line_break() ++ eog_util_indenter:indent() ++
            ForAllBegin ++ ForAllFilter ++ ForAllPropBegin ++
            ErlangPostcondition ++ ForAllPropEnd ++
            ForAllEnd.

%%--------------------------------------------------------------------
%% get_test_function_name
%%--------------------------------------------------------------------
get_test_function_name()->
    eog_common_erlang:to_function_name("test").

%%--------------------------------------------------------------------
%% get_test_function
%%--------------------------------------------------------------------
get_test_function(Operations)->
    "test() -> " ++ eog_util:line_break() ++ eog_util_indenter:indent(4) ++
    lists:append(lists:map(fun({operation, OperationData}) ->
        OperationName = proplists:get_value(name, OperationData),
        Constraints = proplists:get_value(constraints, OperationData),
        Postconditions = proplists:get_value(postconditions, Constraints),
        lists:map(fun({constraint, ConstraintData}) ->
            PostconditionName = proplists:get_value(name, ConstraintData),
            "eqc:quickcheck(" ++
                eog_stateless_prop:get_property_name(OperationName, PostconditionName)  ++ "())," ++
                   eog_util:line_break() ++ eog_util_indenter:indent()   
        end, Postconditions)
    end, Operations)) ++
    "ok." ++ eog_util:line_break() ++ eog_util_indenter:indent(-4) ++
    eog_util:line_break().

%%====================================================================
%% Internal functions
%%====================================================================
%%--------------------------------------------------------------------
%% get_params
%%--------------------------------------------------------------------
get_params(Params)->
    lists:foldl(
        fun({param, ParamData}, Result) ->
            Name = proplists:get_value(name, ParamData),
            case Result of
                "" ->
                    eog_common_erlang:to_variable_name(Name);
                _ ->
                    Result ++ ", " ++ eog_common_erlang:to_variable_name(Name)
            end
        end,
        "",
        Params
    ).

%%--------------------------------------------------------------------
%% get_generators
%%--------------------------------------------------------------------
get_generators(Params)->
    lists:foldl(
        fun({param, ParamData}, Result) ->
            Type = proplists:get_value(type, ParamData),
            case Result of
                "" ->
                    eog_common_gen:get_gen_name(Type) ++ "()";
                _ ->
                    Result ++ ", " ++ eog_common_gen:get_gen_name(Type) ++ "()"
            end
        end,
        "",
        Params
    ).