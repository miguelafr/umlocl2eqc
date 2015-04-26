-module(eog_util_unique).

-behaviour(gen_server).

-record(state, {
        last_value = 0
    }).

-export([start/0, stop/0]).
-export([reset/0, next/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
         code_change/3]).

%%====================================================================
%% Genserver start/stop
%%====================================================================
start()->
    case gen_server:start_link({local, ?MODULE}, ?MODULE, [], []) of
        {ok,Pid} ->
            Pid;
        {error,{already_started, Pid}} ->
            Pid
    end.

stop() ->
    gen_server:cast(?MODULE, stop).

%%====================================================================
%% API
%%====================================================================
reset() ->
    return(gen_server:call(?MODULE, reset)).

next()->
    return(gen_server:call(?MODULE, next)).

%%====================================================================
%% Genserver callbacks
%%====================================================================
init(_Args) ->
    State = #state {
        last_value = 0
    },
    {ok, State}.

handle_call(reset, _From, State) ->
    NewState = State#state {
        last_value = 0
    },
    {reply, NewState#state.last_value, NewState};

handle_call(next, _From, State) ->
    NewState = #state {
        last_value = State#state.last_value + 1
    },
    {reply, NewState#state.last_value, NewState};

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
return({ok, R}) ->
    R;
return({error, E}) ->
    throw(E);
return(R) ->
    R.