-module(eog_common_erlang).

%% API
-export([to_new_empty_record/1, to_new_record/2, to_record_set_values/3,
         to_record_def/2, to_record_access/2, to_record_field_access/3,
         to_variable_state/1, to_variable_name/1, to_field_name/1,
         to_field_name/2, to_module_name/1, to_function_name/1, 
         to_function_def/2, to_function_call/2, to_boolean_value/1,
         to_integer_value/1]).

%%====================================================================
%% API
%%
%% @type Field = {string(), string()}
%%====================================================================
%%--------------------------------------------------------------------
%% @spec to_new_empty_record(string()) -> string()
%%--------------------------------------------------------------------
to_new_empty_record(RecordName)->
    "#" ++ RecordName ++ "{}".

%%--------------------------------------------------------------------
%% @spec to_new_record(string(), [Field]) -> string()
%%--------------------------------------------------------------------
to_new_record(RecordName, Fields)->
    "#" ++ RecordName ++ " {" ++
        eog_util:line_break() ++ eog_util_indenter:indent(4) ++
        eog_util:foldl("", Fields,
                       fun({FieldName, FieldValue}) ->
                               FieldName ++ " = " ++ FieldValue
                       end,
                       fun() ->
                               ", " ++ eog_util:line_break() ++
                                   eog_util_indenter:indent()
                       end) ++
        eog_util:line_break() ++ eog_util_indenter:indent(-4) ++ "}".

%%--------------------------------------------------------------------
%% @spec to_record_set_values(string(), string(), [Field]) -> string()
%%--------------------------------------------------------------------
to_record_set_values(VarName, RecordName, Fields)->
    VarName ++ "#" ++ RecordName ++ " {" ++
        eog_util:line_break() ++ eog_util_indenter:indent(4) ++
        eog_util:foldl("", Fields,
                       fun({FieldName, FieldValue}) ->
                               FieldName ++ " = " ++ FieldValue
                       end,
                       fun() ->
                               ", " ++ eog_util:line_break()
                       end) ++
        eog_util:line_break() ++ eog_util_indenter:indent(-4) ++ "}".

%%--------------------------------------------------------------------
%% @spec to_record_def(string(), Field) -> string()
%%--------------------------------------------------------------------
to_record_def(RecordName, Fields) ->
    "-record(" ++ RecordName ++ ", {" ++ eog_util:line_break() ++
        eog_util_indenter:indent(4) ++
        eog_util:foldl("", Fields,
                       fun({FieldName, FieldValue}) ->
                               FieldName ++ " = " ++ FieldValue
                       end,
                       fun() ->
                               ", " ++ eog_util:line_break() ++
                                   eog_util_indenter:indent()
                       end) ++
        eog_util:line_break() ++ eog_util_indenter:indent(-4) ++ "})." ++
        eog_util:line_break().

%%--------------------------------------------------------------------
%% @spec to_record_access(string(), string()) -> string()
%%--------------------------------------------------------------------
to_record_access(VarName, RecordName)->
    VarName ++ "#" ++ RecordName.

%%--------------------------------------------------------------------
%% to_record_field_access
%%--------------------------------------------------------------------
to_record_field_access(VarName, RecordName, FieldName)->
    to_record_access(VarName, RecordName) ++ "." ++ FieldName.

%%--------------------------------------------------------------------
%% to_variable_state
%%--------------------------------------------------------------------
to_variable_state(VariableName)->
    to_record_field_access(eog_statem_state:get_state_name(),
                           eog_statem_state:get_testing_state_record_name(),
                           to_field_name(VariableName)).

%%--------------------------------------------------------------------
%% to_variable_name
%%--------------------------------------------------------------------
to_variable_name([{name, VariableName} | _Data])->
    to_variable_name(VariableName);

to_variable_name(VariableName)->
    string:to_upper(string:sub_string(VariableName, 1, 1)) ++
        string:sub_string(VariableName, 2).

%%--------------------------------------------------------------------
%% to_field_name
%%--------------------------------------------------------------------
to_field_name([{name, VariableName} | _Data])->
    to_field_name(VariableName);

to_field_name(FieldName)->
    string:to_lower(string:sub_string(FieldName, 1, 1)) ++
        string:sub_string(FieldName, 2).

to_field_name(VariableName, FieldName)->
    "proplists:get_value(" ++ to_field_name(FieldName) ++ ", " ++
        "erlang:element(2, " ++ VariableName ++ "))".

%%--------------------------------------------------------------------
%% to_module_name
%%--------------------------------------------------------------------
to_module_name(ModuleName)->
    string:to_lower(string:sub_string(ModuleName, 1, 1)) ++
        string:sub_string(ModuleName, 2).

%%--------------------------------------------------------------------
%% to_function_name
%%--------------------------------------------------------------------
to_function_name([{name, VariableName} | _Data])->
    to_function_name(
      string:join(
        lists:map(
          fun(S) ->
                  string:to_lower(string:sub_string(S, 1, 1)) ++
                      string:sub_string(S, 2)
          end,
          string:tokens(VariableName, "()")), "_"));

to_function_name(FunctionName)->
    string:to_lower(string:sub_string(FunctionName, 1, 1)) ++
        string:sub_string(FunctionName, 2).

%%--------------------------------------------------------------------
%% to_function_def
%%--------------------------------------------------------------------
to_function_def(FunctionName, Args) ->
    FunctionName ++ "(" ++
        eog_util:foldl(
      "", Args, fun(Arg) -> Arg end, fun() -> ", " end) ++
        ") ->".

%%--------------------------------------------------------------------
%% to_function_call
%%--------------------------------------------------------------------
to_function_call(FunctionName, Args) ->
    FunctionName ++ "(" ++
        eog_util:foldl(
      "", Args, fun(Arg) -> Arg end, fun() -> ", " end) ++
        ")".

%%--------------------------------------------------------------------
%% to_boolean_value
%%--------------------------------------------------------------------
to_boolean_value(Boolean) ->
    erlang:atom_to_list(Boolean).

%%--------------------------------------------------------------------
%% to_integer_value
%%--------------------------------------------------------------------
to_integer_value(Integer) ->
    erlang:integer_to_list(Integer).