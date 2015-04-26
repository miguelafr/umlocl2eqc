-module(eog_statem_prop).

%% API
-export([get_property/0]).

%%====================================================================
%% API
%%====================================================================
%%--------------------------------------------------------------------
%% get_prop
%%--------------------------------------------------------------------
get_property()->
    eog_util:line_break() ++ "test() ->" ++
        eog_util:line_break() ++ eog_util_indenter:set_value(4) ++ 
        "?FORALL(Cmds, commands(?MODULE)," ++
        eog_util:line_break() ++ eog_util_indenter:indent(4) ++ 
        "begin" ++ eog_util:line_break() ++ eog_util_indenter:indent(4) ++ 
        "{H, " ++ eog_statem_state:get_state_name() ++ ", Res} = " ++
        "run_commands(?MODULE, Cmds)," ++
        eog_util:line_break() ++ eog_util_indenter:indent() ++ 
        "?WHENFAIL(io:format(\"H: ~p\\n" ++
        eog_statem_state:get_state_name() ++ ": ~p\\nRes: ~p\\n\", [H, " ++
        eog_statem_state:get_state_name() ++ ", Res])," ++
        eog_util:line_break() ++ eog_util_indenter:indent(4) ++ 
        "aggregate(command_names(Cmds), Res==ok))" ++
        eog_util:line_break() ++ eog_util_indenter:indent(-8) ++
        "end)." ++ eog_util:line_break() ++ eog_util_indenter:indent(-8).