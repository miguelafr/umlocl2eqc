-module(ocl_set).

%% API
-export([eq/2, neq/2, is_empty/1, intersection/2, union/2, including/2,
         excluding/2, includes/2, new_set/0]).

%%====================================================================
%% API
%%====================================================================
%%--------------------------------------------------------------------
%% eq
%%--------------------------------------------------------------------
eq(X, Y)->
    X == Y.

%%--------------------------------------------------------------------
%% neq
%%--------------------------------------------------------------------
neq(X, Y)->
    not(eq(X,Y)).

%%--------------------------------------------------------------------
%% is_empty
%%--------------------------------------------------------------------
is_empty(X)->
    X == sets:new().

%%--------------------------------------------------------------------
%% intersection
%%--------------------------------------------------------------------
intersection(X, Y)->
    sets:intersection(X, Y).

%%--------------------------------------------------------------------
%% union
%%--------------------------------------------------------------------
union(X, Y)->
    sets:union(X, Y).

%%--------------------------------------------------------------------
%% add_element
%%--------------------------------------------------------------------
including(X, L) ->
    sets:add_element(X, L).

%%--------------------------------------------------------------------
%% del_element
%%--------------------------------------------------------------------
excluding(X, L) ->
    sets:del_element(X, L).

%%--------------------------------------------------------------------
%% is_member
%%--------------------------------------------------------------------
includes(X, L) ->
    sets:is_element(X, L).

%%--------------------------------------------------------------------
%% new_set
%%--------------------------------------------------------------------
new_set()->
    sets:new().