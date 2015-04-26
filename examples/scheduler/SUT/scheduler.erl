-module(scheduler).

-behaviour(gen_server).

-record(state, {
        active = undefined,
        ready = sets:new(),
        waiting = sets:new()
    }).

-export([start/0, stop/0]).
-export([init/0, new/1, ready/1, swap/0]).
-export([get_active/0, get_ready/0, get_waiting/0]).
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
    try
        gen_server:call(?MODULE, stop)
    catch
        exit:{noproc,{gen_server,call,[?MODULE,stop]}} ->
            ok;
        exit:{normal,{gen_server,call,[?MODULE,stop]}} ->
            ok
    end.

%%====================================================================
%% API
%%====================================================================
init() ->
    return(gen_server:call(?MODULE, init)).

new(P)->
    return(gen_server:call(?MODULE, {new, P})).

ready(P)->
    return(gen_server:call(?MODULE, {ready, P})).

swap() ->
    return(gen_server:call(?MODULE, swap)).

%%====================================================================
%% State
%%====================================================================
get_active()->
    return(gen_server:call(?MODULE, get_active)).

get_ready()->
    return(gen_server:call(?MODULE, get_ready)).

get_waiting()->
    return(gen_server:call(?MODULE, get_waiting)).

%%====================================================================
%% Genserver callbacks
%%====================================================================
init(_Args) ->
    State = #state {
        active = undefined,
        ready = sets:new(),
        waiting = sets:new()
    },
    {ok, State}.

handle_call(init, _From, _State) ->
    NewState = #state {
        active = undefined,
        ready = sets:new(),
        waiting = sets:new()
    },
    {reply, ok, NewState};

handle_call({new, P}, _From, State) ->
    NewState = State#state {
        waiting = sets:add_element(P, State#state.waiting)
    },
    {reply, ok, NewState};

handle_call({ready, P}, _From, State) ->
    NewState = case State#state.active of
        undefined ->
            State#state {
                active = P,
                waiting = sets:del_element(P, State#state.waiting)
            };
        _ ->
            State#state {
                ready = sets:add_element(P, State#state.ready),
                waiting = sets:del_element(P, State#state.waiting)
            }
    end,
    {reply, ok, NewState};

handle_call(swap, _From, State) ->
    {NewActive, NewReady, NewWaiting} = case sets:size(State#state.ready) of
        0 ->
            Active = undefined,
            Ready = sets:new(),
            Waiting = sets:add_element(State#state.active, State#state.waiting),
            {Active, Ready, Waiting};
        _ ->
            Active = lists:last(sets:to_list(State#state.ready)),
            Ready = sets:del_element(Active, State#state.ready),
            Waiting = sets:add_element(State#state.active, State#state.waiting),
            {Active, Ready, Waiting}
    end,
    NewState = State#state {
        active = NewActive,
        ready = NewReady,
        waiting = NewWaiting
    },
    {reply, ok, NewState};

handle_call(get_active, _From, State) ->
    {reply, State#state.active, State};

handle_call(get_ready, _From, State) ->
    {reply, State#state.ready, State};

handle_call(get_waiting, _From, State) ->
    {reply, State#state.waiting, State};

handle_call(stop, _From, State) ->
    {stop, normal, State};

handle_call(_Request, _From, State) ->
    {reply, {error, not_implemented}, State}.

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
