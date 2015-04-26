-module(ocl_seq).

%% API
-export([eq/2, neq/2, is_empty/1, including/2, excluding/2, includes/2,
         new/0, new/2, size/1, iterate/3, forAll/2, select/2,
         not_empty/1]).

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
    X == [].

%%--------------------------------------------------------------------
%% add_element
%%--------------------------------------------------------------------
including(X, L) ->
    L ++ [X].

%%--------------------------------------------------------------------
%% del_element
%%--------------------------------------------------------------------
excluding(X, L) ->
    lists:delete(X, L).

%%--------------------------------------------------------------------
%% includes
%%--------------------------------------------------------------------
includes(E, L)->
    lists:member(E, L).

%%--------------------------------------------------------------------
%% new_seq
%%--------------------------------------------------------------------
new()->
    [].

%%--------------------------------------------------------------------
%% new_seq
%%--------------------------------------------------------------------
new(A, B)->
    lists:seq(A, B).

%%--------------------------------------------------------------------
%% size
%%--------------------------------------------------------------------
size(L)->
    erlang:length(L).

%%--------------------------------------------------------------------
%% iterate
%%--------------------------------------------------------------------
iterate(F, Acc0, L)->
    lists:foldl(F, Acc0, L).

%%--------------------------------------------------------------------
%% forAll
%%--------------------------------------------------------------------
forAll(F, L)->
    lists:all(F, L).

%%--------------------------------------------------------------------
%% select
%%--------------------------------------------------------------------
select(F, L)->
    lists:filter(F, L).

%%--------------------------------------------------------------------
%% not_empty
%%--------------------------------------------------------------------
not_empty(L)->
    not(is_empty(L)).