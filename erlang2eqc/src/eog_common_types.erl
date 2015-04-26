-module(eog_common_types).

%% API
-export([get_type/1, initial_value/1]).

%%====================================================================
%% API
%%====================================================================
%%--------------------------------------------------------------------
%% get_type
%%--------------------------------------------------------------------
get_type({content, ContentData}) ->
    get_type(ContentData);

get_type([{expression, "PropertyCallExp"} | Data])->
    ReferredProperty = proplists:get_value(referredProperty, Data),
    Type = proplists:get_value(type, ReferredProperty),
    get_type(Type);

get_type([{expression, "OperationCallExp"} | Data])->
    ReferredOperation = proplists:get_value(referredOperation, Data),
    Type = proplists:get_value(type, ReferredOperation),
    get_type(Type);

get_type([{expression, "VariableExp"} | Data])->
    ReferredVariable = proplists:get_value(referredVariable, Data),
    Type = proplists:get_value(type, ReferredVariable),
    get_type(Type);

get_type([{expression, "CollectionLiteralExp"} | Data])->
    proplists:get_value(kind, Data);

get_type([{expression, "BooleanLiteralExp"} | _Data])->
    "boolean";

get_type([{expression, "IteratorExpImpl"} | Data])->
    Source = proplists:get_value(source, Data),
    get_type(Source);

get_type(Type)->
    Name = proplists:get_value(name, Type),
    [TypeKeyword | _] = string:tokens(Name, "("),
    TypeKeyword.

%%--------------------------------------------------------------------
%% initial_value
%%--------------------------------------------------------------------
initial_value(Type)->
    case eog_common_types:get_type(Type) of
        "Set" ->
            "[]";
        _ ->
            io:format("WARNING: Unknown initial_value for type: ~p ~n", [Type]),
            "undefined"
    end.