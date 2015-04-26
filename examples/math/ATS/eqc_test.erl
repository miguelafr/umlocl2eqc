-module(eqc_test).

-include_lib("eqc/include/eqc.hrl").

%% Properties
-export([test/0]).
-export([prop_add_/0, prop_mult_/0, prop_division_/0, prop_pow_/0]).


%%====================================================================
%% test
%%====================================================================
test() -> 
    eqc:quickcheck(prop_add_()),
    eqc:quickcheck(prop_mult_()),
    eqc:quickcheck(prop_division_()),
    eqc:quickcheck(prop_pow_()),
    ok.


%%====================================================================
%% property
%%====================================================================

%%--------------------------------------------------------------------
%% property: prop_add_
%%--------------------------------------------------------------------
prop_add_()->
    ?FORALL({A, B}, 
        {ocl_gen:gen_integer(), ocl_gen:gen_integer()},
        begin
            Result = mathUtils:add(A, B),
            (Result ==
            (A +
            B))
        end
    ).


%%--------------------------------------------------------------------
%% property: prop_mult_
%%--------------------------------------------------------------------
prop_mult_()->
    ?FORALL({A, B}, 
        {ocl_gen:gen_integer(), ocl_gen:gen_integer()},
        begin
            Result = mathUtils:mult(A, B),
            (Result ==
            (A *
            B))
        end
    ).


%%--------------------------------------------------------------------
%% property: prop_division_
%%--------------------------------------------------------------------
prop_division_()->
    ?FORALL({A, B}, 
        ?SUCHTHAT({A, B}, {ocl_gen:gen_integer(), ocl_gen:gen_integer()},
            begin
                (B /=
                0)
            end
        ),
        begin
            Result = mathUtils:division(A, B),
            (Result ==
            (A div
            B))
        end
    ).


%%--------------------------------------------------------------------
%% property: prop_pow_
%%--------------------------------------------------------------------
prop_pow_()->
    ?FORALL({A, B}, 
        {ocl_gen:gen_integer(), ocl_gen:gen_unlimitednatural()},
        begin
            Result = mathUtils:pow(A, B),
            (Result ==
            ocl_seq:iterate(
                fun(I, Acc) -> 
                    (Acc *
                    A)
                end, 
                1, ocl_seq:new_seq(1, B)
            ))
        end
    ).


%%====================================================================
%% generators
%%====================================================================
