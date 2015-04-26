-module(eog_statem_header).

%% API
-export([get_header/1, get_exports/3]).

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
        eog_util:line_break() ++
        "-include_lib(\"eqc/include/eqc_statem.hrl\")." ++
        eog_util:line_break() ++ eog_util:line_break() ++
        "-behaviour(eqc_statem)." ++ 
        eog_util:line_break() ++ eog_util:line_break().

%%--------------------------------------------------------------------
%% get_exports
%%--------------------------------------------------------------------
get_exports(Operations, StateVariables, Classes)->
    "%% EQC callbacks" ++ eog_util:line_break() ++
        (case eog_statem_state:use_dynamic_preconditions(
                Operations, StateVariables, Classes) of
            true ->
                "-export([initial_state/0, command/1, " ++
                "precondition/2, dynamic_precondition/2, " ++
                "postcondition/3, next_state/3]).";
            false ->
                "-export([initial_state/0, command/1, " ++
                "precondition/2, postcondition/3, next_state/3])."
        end) ++
        eog_util:line_break() ++ eog_util:line_break() ++
        "%% Wrapper functions" ++ eog_util:line_break() ++
        "-export([" ++ 
        lists:foldl(
            fun({operation, OperationData}, Result) ->
                Name = proplists:get_value(name, OperationData),
                Params = proplists:get_value(params, OperationData),
                case Result of
                    "" ->
                        eog_common_erlang:to_function_name(Name) ++ "/" ++
                            erlang:integer_to_list(erlang:length(Params));
                    _ ->
                        Result ++ ", " ++ eog_common_erlang:to_function_name(Name) ++
                            "/" ++ erlang:integer_to_list(erlang:length(Params))
                end
            end,
            "", Operations) ++
        "])." ++ eog_util:line_break() ++ eog_util:line_break() ++
        "%% Testing function" ++ eog_util:line_break() ++
         "-export([test/0])." ++
        eog_util:line_break() ++ eog_util:line_break().