-module(stack).

-behaviour(gen_server).

-record(state, {
        elements = []
    }).

-export([init/0]).
-export([pop/0, push/1, top/0, size/0, isEmpty/0, elements/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
         code_change/3]).

%%====================================================================
%% API
%%====================================================================
init()->
    case gen_server:start_link({local, ?MODULE}, ?MODULE, [], []) of
        {ok,_Pid} ->
            ok;
        {error,{already_started, _Pid}} ->
            return(gen_server:call(?MODULE, init)),
            ok
    end.

pop()->
    try
        return(gen_server:call(?MODULE, pop))
    catch
        _:_ ->
            init(),
            pop()
    end.

push(X)->
    try
        return(gen_server:call(?MODULE, {push, X}))
    catch
        _:_ ->
            init(),
            push(X)
    end.

top()->
    try
        return(gen_server:call(?MODULE, top))
    catch
        _:_ ->
            init(),
            top()
    end.

size() ->
    try
        return(gen_server:call(?MODULE, size))
    catch
        _:_ ->
            init(),
            size()
    end.

isEmpty() ->
    try
        return(gen_server:call(?MODULE, isEmpty))
    catch
        _:_ ->
            init(),
            isEmpty()
    end.

elements()->
    try
        return(gen_server:call(?MODULE, elements))
    catch
        _:_ ->
            init(),
            elements()
    end.

%%====================================================================
%% Genserver callbacks
%%====================================================================
init(_Args) ->
    State = #state {
        elements = []
    },
    {ok, State}.

handle_call(init, _From, _State) ->
    NewState = #state {
        elements = []
    },
    {reply, ok, NewState};

handle_call(pop, _From, State) ->
	[H | T] = State#state.elements,
    NewState = State#state {
        elements = T
    },
    {reply, H, NewState};

handle_call({push, X}, _From, State) ->
    NewState = State#state {
        elements = [X | State#state.elements]
    },
    {reply, ok, NewState};

handle_call(top, _From, State) ->
    {reply, erlang:hd(State#state.elements), State};

handle_call(size, _From, State) ->
    {reply, erlang:length(State#state.elements), State};

handle_call(isEmpty, _From, State) ->
    {reply, erlang:length(State#state.elements) == 0, State};

handle_call(elements, _From, State) ->
    {reply, State#state.elements, State};

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
