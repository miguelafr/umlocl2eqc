-module(eog_util_indenter).

-record(state, {
        value = 0
    }).

%% API
-export([set_value/1, indent/0, indent/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
         code_change/3]).

%%====================================================================
%% API
%%====================================================================
%%--------------------------------------------------------------------
%% set_value
%%--------------------------------------------------------------------
set_value(Value)->
    try
        tab(gen_server:call(?MODULE, {set_value, Value}))
    catch
        exit:{noproc,_} ->
            gen_server:start_link({local, ?MODULE}, ?MODULE, 0, []),
            indent(Value)
    end.

%%--------------------------------------------------------------------
%% indent
%%--------------------------------------------------------------------
indent()->
    try
        tab(gen_server:call(?MODULE, value))
    catch
        exit:{noproc,_} ->
            gen_server:start_link({local, ?MODULE}, ?MODULE, 0, []),
            indent()
    end.

%%--------------------------------------------------------------------
%% indent
%%--------------------------------------------------------------------
indent(Value)->
    try
        tab(gen_server:call(?MODULE, {value, Value}))
    catch
        exit:{noproc,_} ->
            gen_server:start_link({local, ?MODULE}, ?MODULE, 0, []),
            indent(Value)
    end.

%%====================================================================
%% Genserver callbacks
%%====================================================================
init(Value) ->
    State = #state {
        value = Value
    },
    {ok, State}.

handle_call(value, _From, State) ->
    {reply, State#state.value, State};

handle_call({set_value, Value}, _From, State) ->
    NewState = State#state {
        value = Value
    },
    {reply, NewState#state.value, NewState};

handle_call({value, Value}, _From, State) ->
    NewState = State#state {
        value = State#state.value + Value
    },
    {reply, NewState#state.value, NewState};

handle_call(_Request, _From, State) ->
    {reply, {error, not_implemented}, State}.

handle_cast(stop, State) ->
    {stop, normal, State};

handle_cast(_Request, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%====================================================================
%% Internal functions
%%====================================================================
%%--------------------------------------------------------------------
%% tab
%%--------------------------------------------------------------------
tab(Size) when Size > 0 ->
    " " ++ tab(Size - 1);
tab(_Size)->
    "".