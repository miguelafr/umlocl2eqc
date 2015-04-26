-module(eqc_test).

-include_lib("eqc/include/eqc.hrl").

%% Properties
-export([test/0]).
-export([prop_add_element_added/0, prop_add_longer_list/0, prop_delete_element_deleted/0, prop_delete_smaller_list/0, prop_sum_sum/0, prop_prod_prod/0, prop_min_min/0, prop_max_max/0]).


%%====================================================================
%% test
%%====================================================================
test() -> 
    eqc:quickcheck(prop_add_element_added()),
    eqc:quickcheck(prop_add_longer_list()),
    eqc:quickcheck(prop_delete_element_deleted()),
    eqc:quickcheck(prop_delete_smaller_list()),
    eqc:quickcheck(prop_sum_sum()),
    eqc:quickcheck(prop_prod_prod()),
    eqc:quickcheck(prop_min_min()),
    eqc:quickcheck(prop_max_max()),
    ok.


%%====================================================================
%% property
%%====================================================================

%%--------------------------------------------------------------------
%% property: prop_add_element_added
%%--------------------------------------------------------------------
prop_add_element_added()->
    ?FORALL({E, L}, 
        {gen_item(), gen_sequence_gen_item()},
        begin
            Result = listUtils:add(E, L),
            ocl_seq:includes(E, Result)
        end
    ).


%%--------------------------------------------------------------------
%% property: prop_add_longer_list
%%--------------------------------------------------------------------
prop_add_longer_list()->
    ?FORALL({E, L}, 
        {gen_item(), gen_sequence_gen_item()},
        begin
            Result = listUtils:add(E, L),
            (ocl_seq:size(Result) ==
            (ocl_seq:size(L) +
            1))
        end
    ).


%%--------------------------------------------------------------------
%% property: prop_delete_element_deleted
%%--------------------------------------------------------------------
prop_delete_element_deleted()->
    ?FORALL({E, L}, 
        {gen_item(), gen_sequence_gen_item()},
        begin
            Result = listUtils:delete(E, L),
            not(ocl_seq:includes(E, Result))
        end
    ).


%%--------------------------------------------------------------------
%% property: prop_delete_smaller_list
%%--------------------------------------------------------------------
prop_delete_smaller_list()->
    ?FORALL({E, L}, 
        {gen_item(), gen_sequence_gen_item()},
        begin
            Result = listUtils:delete(E, L),
            (not (ocl_seq:includes(E, L)) or
            (ocl_seq:size(Result) <
            ocl_seq:size(L)))
        end
    ).


%%--------------------------------------------------------------------
%% property: prop_sum_sum
%%--------------------------------------------------------------------
prop_sum_sum()->
    ?FORALL({L}, 
        {gen_sequence_gen_item()},
        begin
            Result = listUtils:sum(L),
            (Result ==
            ocl_seq:iterate(
                fun(X, Acc) -> 
                    (Acc +
                    ocl_datatypes:get_property(value, X))
                end, 
                0, L
            ))
        end
    ).


%%--------------------------------------------------------------------
%% property: prop_prod_prod
%%--------------------------------------------------------------------
prop_prod_prod()->
    ?FORALL({L}, 
        {gen_sequence_gen_item()},
        begin
            Result = listUtils:prod(L),
            (Result ==
            ocl_seq:iterate(
                fun(X, Acc) -> 
                    (Acc *
                    ocl_datatypes:get_property(value, X))
                end, 
                1, L
            ))
        end
    ).


%%--------------------------------------------------------------------
%% property: prop_min_min
%%--------------------------------------------------------------------
prop_min_min()->
    ?FORALL({L}, 
        ?SUCHTHAT({L}, {gen_sequence_gen_item()},
            begin
                ocl_seq:not_empty(L)
            end
        ),
        begin
            Result = listUtils:min(L),
            ocl_seq:forAll(
                fun(X) -> 
                    (Result =<
                    ocl_datatypes:get_property(value, X))
                end, 
                L
            )
        end
    ).


%%--------------------------------------------------------------------
%% property: prop_max_max
%%--------------------------------------------------------------------
prop_max_max()->
    ?FORALL({L}, 
        ?SUCHTHAT({L}, {gen_sequence_gen_item()},
            begin
                ocl_seq:not_empty(L)
            end
        ),
        begin
            Result = listUtils:max(L),
            ocl_seq:forAll(
                fun(X) -> 
                    (Result >=
                    ocl_datatypes:get_property(value, X))
                end, 
                L
            )
        end
    ).


%%====================================================================
%% generators
%%====================================================================

%%--------------------------------------------------------------------
%% generator: sequence_item
%%--------------------------------------------------------------------
gen_sequence_gen_item() ->
    ocl_gen:gen_sequence(gen_item()).


%%--------------------------------------------------------------------
%% generator: item
%%--------------------------------------------------------------------
gen_item() ->
    {item, [{id, ocl_gen:gen_integer()}, {value, ocl_gen:gen_integer()}]}.

