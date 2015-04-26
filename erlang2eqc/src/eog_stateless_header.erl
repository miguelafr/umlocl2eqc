-module(eog_stateless_header).

%% API
-export([get_header/1, get_exports/1]).

%%====================================================================
%% API
%%====================================================================
%%--------------------------------------------------------------------
%% get_header
%%--------------------------------------------------------------------
get_header(ModuleName)->
    "-module(" ++ ModuleName ++ ")." ++ 
        eog_util:line_break() ++ eog_util:line_break() ++
        "-include_lib(\"eqc/include/eqc.hrl\")." ++
        eog_util:line_break() ++ eog_util:line_break().

%%--------------------------------------------------------------------
%% get_exports
%%--------------------------------------------------------------------
get_exports(Operations)->
    PropertyNames = lists:append(lists:map(fun({operation, OperationData}) ->
        OperationName = proplists:get_value(name, OperationData),
        Constraints = proplists:get_value(constraints, OperationData),
        Postconditions = proplists:get_value(postconditions, Constraints),
        lists:map(fun({constraint, ConstraintData}) ->
                PostconditionName = proplists:get_value(name, ConstraintData),
            eog_stateless_prop:get_property_name(OperationName, PostconditionName)
        end, Postconditions)
    end, Operations)),
    "%% Properties" ++ eog_util:line_break() ++
    "-export([" ++ eog_stateless_prop:get_test_function_name() ++ "/0])." ++ eog_util:line_break() ++
    "-export([" ++
    lists:foldl(
        fun(PropertyName, Result) ->
            case Result of
                "" ->
                    PropertyName ++ "/0";
                _ ->
                    Result ++ ", " ++ PropertyName ++ "/0"
            end
        end,
        "",
        PropertyNames) ++ "])." ++
    eog_util:line_break() ++ eog_util:line_break().