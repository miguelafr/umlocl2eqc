-module(process).

-behaviour(gen_server).

-record(state, {
        id = undefined
    }).

-export([new/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
         code_change/3]).

%%====================================================================
%% Genserver start/stop
%%====================================================================
new(Id)->
    {ok, Pid} = gen_server:start_link(?MODULE, Id, []),
    Pid.

%%====================================================================
%% Genserver callbacks
%%====================================================================
init(Id) ->
    State = #state {
        id = Id
    },
    {ok, State}.

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
