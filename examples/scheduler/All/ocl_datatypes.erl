-module(ocl_datatypes).

-export([typeof/2, get_property/2]).

%%====================================================================
%% API
%%====================================================================
%%--------------------------------------------------------------------
%% typeof
%%--------------------------------------------------------------------
typeof({T, _}, T) ->
    true;
typeof(_, _) ->
    false.

%%--------------------------------------------------------------------
%% typeof
%%--------------------------------------------------------------------
get_property(PropertyName, Variable) ->
    proplists:get_value(PropertyName, erlang:element(2, Variable)).