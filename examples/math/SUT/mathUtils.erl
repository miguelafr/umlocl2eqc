-module(mathUtils).

-export([add/2, mult/2, division/2, pow/2]).

add(X, Y) ->
    X + Y.

mult(X, Y) ->
    X * Y.

division(X, Y) ->
    X div Y.

pow(X, Y) ->
    math:pow(X, Y).
