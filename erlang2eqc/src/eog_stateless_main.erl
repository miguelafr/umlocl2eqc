%% Author: miguelafr
%% Created: 26/02/2012
%% Description: eog_stateless_main
-module(eog_stateless_main).

%%====================================================================
%% Include files
%%====================================================================

%%====================================================================
%% Exported functions
%%====================================================================
-export([generate/5]).

%%====================================================================
%% API
%%====================================================================
generate(FileName, ModuleName, ClassName, Classes, Operations)->
    write_header(FileName, ModuleName),
    write_exports(FileName, Operations),
    
    write_functions_block_header(FileName, "test"),
    write_test_properties(FileName, Operations),
    
    write_functions_block_header(FileName, "property"),
    lists:map(fun({operation, OperationData}) ->
                      Constraints = proplists:get_value(constraints, OperationData),
                      Postconditions = proplists:get_value(postconditions, Constraints),
                      lists:map(fun({constraint, ConstraintData}) ->
                                        write_property(FileName, ClassName,
                                                       OperationData,
                                                       ConstraintData, Classes)
                                end, Postconditions)
              end, Operations),
    
    write_functions_block_header(FileName, "generators"),
    write_generators(FileName, Operations, Classes).

%%====================================================================
%% Internal functions
%%====================================================================
%%--------------------------------------------------------------------
%% write_functions_block_header
%%--------------------------------------------------------------------
write_functions_block_header(FileName, FunctionsBlock) ->
    file:write_file(FileName, eog_common_comment:get_block_header(FunctionsBlock),
                    [append]).

%%--------------------------------------------------------------------
%% write_header
%%--------------------------------------------------------------------
write_header(FileName, ModuleName)->
    file:write_file(FileName, eog_stateless_header:get_header(ModuleName), []).

%%--------------------------------------------------------------------
%% write_exports
%%--------------------------------------------------------------------
write_exports(FileName, Operations)->
    file:write_file(FileName, eog_stateless_header:get_exports(Operations), [append]).

%%--------------------------------------------------------------------
%% write_test_properties
%%--------------------------------------------------------------------
write_test_properties(FileName, Operations) ->
    file:write_file(FileName,
                    eog_stateless_prop:get_test_function(Operations),
                    [append]).

%%--------------------------------------------------------------------
%% write_property
%%--------------------------------------------------------------------
write_property(FileName, ClassName, OperationData, ConstraintData, Classes) ->
    file:write_file(FileName,
                    eog_stateless_prop:get_property(ClassName, OperationData,
                                                    ConstraintData, Classes),
                    [append]).

%%--------------------------------------------------------------------
%% write_generators
%%--------------------------------------------------------------------
write_generators(FileName, Operations, Classes) ->
    file:write_file(FileName, eog_common_gen:get_generators(Operations, Classes),
                    [append]).