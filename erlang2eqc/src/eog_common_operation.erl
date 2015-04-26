-module(eog_common_operation).

%% API
-export([get_operation/3]).

%%====================================================================
%% API
%%====================================================================
%%--------------------------------------------------------------------
%% get_operation
%%--------------------------------------------------------------------
get_operation(Operation,
              [Content  = [{expression,"OperationCallExp"} | ContentData]| []], Classes) ->
    ContentsOp = proplists:get_value(contents, ContentData),
    case ContentsOp of
        [ContentOp] ->
            Type = eog_common_types:get_type(ContentOp),
            case exist_class(Type, Classes) of
                true ->
                    {eog_common_erlang:to_module_name(Type) ++ ":" ++ Operation,
                     function};
                false ->
                    get_operation(Operation, [Content])
            end;
        _ ->
            get_operation(Operation, [Content])
    end;

get_operation(Operation,
              [Content  = [{expression,"PropertyCallExp"} | ContentData]| []], Classes) ->

    case proplists:get_value(source, ContentData) of
        [{expression, "VariableExp"} | SourceData] ->
            ReferredVariable = proplists:get_value(referredVariable, SourceData),
            VariableType = proplists:get_value(type, ReferredVariable),
            Type = eog_common_types:get_type(VariableType),
            case exist_class(Type, Classes) of
                true ->
                    {eog_common_erlang:to_module_name(Type) ++ ":get_" ++ Operation,
                     function};
                false ->
                    get_operation(Operation, [Content])
            end;
        _ ->
            get_operation(Operation, [Content])
    end;

get_operation(Operation, Contents, _Classes)->
    get_operation(Operation, Contents).

%%====================================================================
%% Internal functions
%%====================================================================
%%--------------------------------------------------------------------
%% get_operation
%%--------------------------------------------------------------------
get_operation("+", _Contents)->
    {"+", in};

get_operation("-", _Contents)->
    {"-", in};

get_operation("*", _Contents)->
    {"*", in};

get_operation("<", _Contents)->
    {"<", in};

get_operation("<=", _Contents)->
    {"=<", in};

get_operation(">", _Contents)->
    {">", in};

get_operation(">=", _Contents)->
    {">=", in};

get_operation("div", _Contents)->
    {"div", in};

get_operation("and", _Contents)->
    {"andalso", in};

get_operation("not", _Contents)->
    {"not", function};

get_operation("=", [Content | _])->
    ContentType = eog_common_types:get_type(Content),
    case ContentType of
        "Set" ->
            {"ocl_set:eq", function};
        "OrderedSet" ->
            {"ocl_orderedset:eq", function};
        "Sequence" ->
            {"ocl_seq:eq", function};
        "Integer" ->
            {"==", in}; 
        "Boolean" ->
            {"==", in}; 
        "String" ->
            {"==", in}; 
        _ ->
            io:format("WARNING: Using default operator ~p for type ~p ~n",
                      ["=", ContentType]),
            {"ocl:eq", function}
    end;

get_operation("<>", [Content | _])->
    ContentType = eog_common_types:get_type(Content),
    case ContentType of
        "Set" ->
            {"ocl_set:neq", function};
        "OrderedSet" ->
            {"ocl_orderedset:neq", function};
        "Sequence" ->
            {"ocl_seq:neq", function};
        "Integer" ->
            {"/=", in}; 
        "Boolean" ->
            {"/=", in}; 
        "String" ->
            {"/=", in}; 
        _ ->
            io:format("WARNING: Using default operator ~p for type ~p ~n",
                      ["<>", ContentType]),
            {"ocl:neq", function}
    end;

get_operation("isEmpty", [Content | _])->
    ContentType = eog_common_types:get_type(Content),
    case ContentType of
        "Set" ->
            {"ocl_set:is_empty", function};
        "OrderedSet" ->
            {"ocl_orderedset:is_empty", function};
        "Sequence" ->
            {"ocl_seq:is_empty", function};
        _ ->
            io:format("WARNING: Using default operator ~p for type ~p ~n",
                      ["is_empty", ContentType]),
            {"ocl:is_empty", function}
    end;

get_operation("notEmpty", [Content | _])->
    ContentType = eog_common_types:get_type(Content),
    case ContentType of
        "Set" ->
            {"ocl_set:not_empty", function};
        "OrderedSet" ->
            {"ocl_orderedset:not_empty", function};
        "Sequence" ->
            {"ocl_seq:not_empty", function};
        _ ->
            io:format("WARNING: Using default operator ~p for type ~p ~n",
                      ["not_empty", ContentType]),
            {"ocl:not_empty", function}
    end;

get_operation("includes", [Content | _])->
    ContentType = eog_common_types:get_type(Content),
    case ContentType of
        "Set" ->
            {"ocl_set:includes", function_revert};
        "OrderedSet" ->
            {"ocl_orderedset:includes", function_revert};
        "Sequence" ->
            {"ocl_seq:includes", function_revert};
        _ ->
            io:format("WARNING: Using default operator ~p for type ~p ~n",
                      ["includes", ContentType]),
            {"ocl:includes", function_revert}
    end;

get_operation("union", [Content | _])->
    ContentType = eog_common_types:get_type(Content),
    case ContentType of
        "Set" ->
            {"ocl_set:union", function_revert};
        "OrderedSet" ->
            {"ocl_orderedset:union", function_revert};
        _ ->
            io:format("WARNING: Using default operator ~p for type ~p ~n",
                      ["union", ContentType]),
            {"ocl:union", function_revert}
    end;

get_operation("intersection", [Content | _])->
    ContentType = eog_common_types:get_type(Content),
    case ContentType of
        "Set" ->
            {"ocl_set:intersection", function_revert};
        "OrderedSet" ->
            {"ocl_orderedset:intersection", function_revert};
        _ ->
            io:format("WARNING: Using default operator ~p for type ~p ~n",
                      ["intersection", ContentType]),
            {"ocl:intersection", function_revert}
    end;

get_operation("including", [Content | _])->
    ContentType = eog_common_types:get_type(Content),
    case ContentType of
        "Set" ->
            {"ocl_set:including", function_revert};
        "OrderedSet" ->
            {"ocl_orderedset:including", function};
        "Sequence" ->
            {"ocl_seq:including", function_revert};
        _ ->
            io:format("WARNING: Using default operator ~p for type ~p ~n",
                      ["including", ContentType]),
            {"ocl:including", function_revert}
    end;

get_operation("excluding", [Content | _])->
    ContentType = eog_common_types:get_type(Content),
    case ContentType of
        "Set" ->
            {"ocl_set:excluding", function_revert};
        "OrderedSet" ->
            {"ocl_orderedset:excluding", function_revert};
        "Sequence" ->
            {"ocl_seq:excluding", function_revert};
        _ ->
            io:format("WARNING: Using default operator ~p for type ~p ~n",
                      ["excluding", ContentType]),
            {"ocl:excluding", function_revert}
    end;

get_operation("oclIsTypeOf", _Contents)->
    {"ocl_datatypes:typeof", function};

get_operation("size", [Content | _])->
    ContentType = eog_common_types:get_type(Content),
    case ContentType of
        "Set" ->
            {"ocl_set:size", function};
        "OrderedSet" ->
            {"ocl_orderedset:size", function};
        "Sequence" ->
            {"ocl_seq:size", function};
        _ ->
            io:format("WARNING: Using default operator ~p for type ~p ~n",
                      ["size", ContentType]),
            {"ocl:size", function}
    end;

get_operation("implies", _Contents)->
    {"or", in_not};

get_operation("assign", _Contents)->
    {"=", in};

get_operation("seq", _Contents)->
    {",", in_nospaces};

get_operation(_Operation, _Contents)->
    {"-----", function}.

%%--------------------------------------------------------------------
%% exist_class
%%--------------------------------------------------------------------
exist_class(ClassName, [{class, ClassData} | MoreClasses])->
    case proplists:get_value(name, ClassData) of
        ClassName ->
            true;
        _ ->
            exist_class(ClassName, MoreClasses)
    end;
exist_class(_ClassName, [])->
    false.