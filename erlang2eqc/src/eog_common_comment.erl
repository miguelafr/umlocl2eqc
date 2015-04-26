-module(eog_common_comment).

%% API
-export([get_record_header/1, get_block_header/1, get_function_header/2]).

%%====================================================================
%% API
%%====================================================================
%%--------------------------------------------------------------------
%% get_record_header
%%--------------------------------------------------------------------
get_record_header(RecordName) ->
    eog_util_indenter:set_value(0) ++
        eog_util:line_break() ++
        "%% " ++ RecordName ++
        eog_util:line_break().

%%--------------------------------------------------------------------
%% get_block_header
%%--------------------------------------------------------------------
get_block_header(FunctionsBlock) ->
    eog_util_indenter:set_value(0) ++
        eog_util:line_break() ++
        "%%====================================================================" ++
        eog_util:line_break() ++ "%% " ++
        FunctionsBlock ++ eog_util:line_break() ++
        "%%====================================================================" ++
        eog_util:line_break().

%%--------------------------------------------------------------------
%% get_function_header
%%--------------------------------------------------------------------
get_function_header(FunctionBlock, FunctionName) ->
    eog_util_indenter:set_value(0) ++
        eog_util:line_break() ++
        "%%--------------------------------------------------------------------" ++
        eog_util:line_break() ++ "%% " ++
        FunctionBlock ++ ": " ++ FunctionName ++ eog_util:line_break() ++ 
        "%%--------------------------------------------------------------------".