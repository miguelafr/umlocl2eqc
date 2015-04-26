-module(eog_common_gen).

%% API
-export([get_gen_name/1, get_generators/2]).

%%====================================================================
%% API
%%====================================================================
%%--------------------------------------------------------------------
%% get_gen_name
%%--------------------------------------------------------------------
get_gen_name([{name, Name} | _])->
    get_gen_name(Name);

get_gen_name("Integer")->
    "ocl_gen:gen_integer";

get_gen_name("UnlimitedNatural")->
    "ocl_gen:gen_unlimitednatural";

get_gen_name("String")->
    "ocl_gen:gen_string";

get_gen_name("Sequence" ++ ClassName)->
    BaseClassName = string:sub_string(ClassName, 2,
                                      erlang:length(ClassName) - 1),
    "gen_sequence_" ++ get_gen_name(BaseClassName);

get_gen_name(Type)->
    "gen_" ++ eog_common_erlang:to_function_name(Type).

%%--------------------------------------------------------------------
%% get_generators
%%--------------------------------------------------------------------
get_generators(Operations, Classes) ->
    GenCache = ets:new(parameters, []),
    get_generators(Operations, Classes, GenCache).

%%====================================================================
%% Internal functions
%%====================================================================
get_generators([{operation, OperationData} | MoreOperations], Classes, GenCache) ->
    Params = proplists:get_value(params, OperationData),
    lists:foldl(
      fun({param, ParamData}, Result) ->
              Type = proplists:get_value(type, ParamData),
              case ets:lookup(GenCache, Type) of
                  [] ->
                      {Gen, GenType} = get_generator(Type, Classes),
                      case GenType of
                          complex ->
                              ets:insert(GenCache, {Type, Gen}),
                              eog_common_comment:get_function_header("generator",
                                                                     eog_common_erlang:to_function_name(Type)) ++
                                  eog_util:line_break() ++ eog_util_indenter:set_value(0) ++
                                  get_gen_name(Type) ++ "() ->" ++
                                  eog_util:line_break() ++ eog_util_indenter:indent(4) ++
                                  Gen ++ "." ++
                                  eog_util:line_break() ++ eog_util_indenter:indent(-4) ++
                                  eog_util:line_break() ++ Result;
                          simple ->
                              "" ++ Result
                      end;
                  [{Type, _Gen}]->
                      Result
              end
      end,
      "",  
      Params) ++
        get_generators(MoreOperations, Classes, GenCache);
get_generators([], _Classes, _GenCache)->
    "".

%%--------------------------------------------------------------------
%% get_generator
%%--------------------------------------------------------------------
get_generator([{name, Name} | _], Classes)->
    get_generator(Name, Classes);

get_generator(Name, Classes)->
    case Name of
        "Sequence" ++ ClassName ->
            BaseClassName = string:sub_string(ClassName, 2,
                                              erlang:length(ClassName) - 1),
            {"ocl_gen:gen_sequence(" ++ get_gen_name(BaseClassName) ++ "())", complex};
        "Integer" ->
            {get_gen_name(Name) ++ "()", simple};
        "UnlimitedNatural" ->
            {get_gen_name(Name) ++ "()", simple};
        "String" ->
            {get_gen_name(Name) ++ "()", simple};
        _ ->
            {"{" ++ eog_common_erlang:to_module_name(Name) ++ ", [" ++
                case get_class(Name, Classes) of
                    {class, ClassData} ->
                        Attributes = proplists:get_value(attributes, ClassData),
                        lists:foldl(
                          fun({attribute, AttributeData}, Result) ->
                                  AttributeName = proplists:get_value(name, AttributeData),
                                  AttributeType = proplists:get_value(type, AttributeData),
                                  {Gen, _GenType} = get_generator(AttributeType, Classes),
                                  case Result of
                                      "" ->
                                          Result ++
                                              "{" ++ AttributeName ++ ", " ++
                                              Gen ++ "}";
                                      _ ->
                                          Result ++ ", " ++
                                              "{" ++ AttributeName ++ ", " ++
                                              Gen ++ "}"
                                  end
                          end,
                          "",
                          Attributes);
                    _ ->
                        ""
                end ++ "]}", complex}
    end.

%%--------------------------------------------------------------------
%% get_class
%%--------------------------------------------------------------------
get_class(ClassName, [{class, ClassData} | MoreClasses])->
    case proplists:get_value(name, ClassData) of
        ClassName ->
            {class, ClassData};
        _ ->
            get_class(ClassName, MoreClasses)
    end;
get_class(_ClassName, [])->
    [].