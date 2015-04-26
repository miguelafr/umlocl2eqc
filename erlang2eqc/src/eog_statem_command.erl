-module(eog_statem_command).

%% API
-export([get_command/1]).

%%====================================================================
%% API
%%====================================================================
%%--------------------------------------------------------------------
%% get_command
%%--------------------------------------------------------------------
get_command(Operations)->
    eog_util:line_break() ++ "command(" ++ eog_statem_state:get_state_name() ++ ") ->" ++
        eog_util:line_break() ++ eog_util_indenter:indent(4) ++ "eqc_gen:oneof([" ++
        eog_util:line_break() ++ eog_util_indenter:indent(4) ++ 
        lists:foldl(
            fun({operation, OperationData}, Result) ->
                case Result of
                    "" ->
                        get_function_call(OperationData);
                    _ ->
                        Result ++ ", " ++
                            eog_util:line_break() ++ eog_util_indenter:indent() ++
                            get_function_call(OperationData)
                            
                end
            end,
            "",
            Operations
        ) ++ eog_util:line_break() ++ eog_util_indenter:indent(-4) ++ "])." ++
        eog_util:line_break().

%%====================================================================
%% Internal functions
%%====================================================================
%%--------------------------------------------------------------------
%% get_function_call
%%--------------------------------------------------------------------
get_function_call(OperationData)->
    Name = proplists:get_value(name, OperationData),
    Params = proplists:get_value(params, OperationData),
    
    "{call, ?MODULE, " ++ eog_common_erlang:to_function_name(Name) ++ ", [" ++
        get_function_call_params(Params) ++ "]}".

%%--------------------------------------------------------------------
%% get_function_call_params
%%--------------------------------------------------------------------
get_function_call_params(Params)->
    lists:foldl(
      fun({param, ParamData}, Result) ->   
          Type = proplists:get_value(type, ParamData),
          Name = eog_common_types:get_type(Type),
          case Result of
              "" ->
                  Result ++ eog_common_gen:get_gen_name(Name) ++ "()";
              _ ->
                  Result ++ ", " ++ eog_common_gen:get_gen_name(Name) ++ "()"
          end
      end,
      "", Params).