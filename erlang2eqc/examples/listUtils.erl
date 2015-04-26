-module(listUtils).

-export([add/2, delete/2, sum/1, prod/1, min/1, max/1]).

add(E, L) ->
    [E | L].

delete(E, L) ->
    case lists:member(E, L) of
        true ->
            delete(E, lists:delete(E, L));
        false ->
            L
    end.

sum(L)->
    do_sum(L, 0).

do_sum([], R) ->
    R;
do_sum([{item,[{id, _Id},{value, Value}]}|T], R) ->
    do_sum(T, Value + R).

prod(L)->
    do_prod(L, 1).

do_prod([], R) ->
    R;
do_prod([{item,[{id, _Id},{value, Value}]}|T], R) ->
    do_prod(T, Value * R).

min([])->
    throw(empty);
min(L) ->
    do_min(L, undefined).

do_min([], R)->
    R;
do_min([{item,[{id, _Id},{value, Value}]}|T], R)->
    case (Value < R) of
        true ->
            do_min(T, Value);
        false ->
            do_min(T, R)
    end.

max([])->
    throw(empty);
max(L) ->
    do_max(L, undefined).

do_max([], R)->
    R;
do_max([{item,[{id, _Id},{value, Value}]}|T], R)->
    case (Value > R) of
        true ->
            do_max(T, Value);
        false ->
            do_max(T, R)
    end.
