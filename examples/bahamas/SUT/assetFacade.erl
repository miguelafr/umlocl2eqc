-module(assetFacade).

-behaviour(gen_server).

-record(state, {
        assets = []
    }).

-export([init/0]).
-export([create/1, delete/1, findAllAssets/0, get_assets/0]).
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

create(Asset)->
    try
        return(gen_server:call(?MODULE, {create, Asset}))
    catch
        _:_ ->
            init(),
            create(Asset)
    end.

delete(AssetId)->
    try
        return(gen_server:call(?MODULE, {delete, AssetId}))
    catch
        _:_ ->
            init(),
            delete(AssetId)
    end.

findAllAssets()->
    try
        return(gen_server:call(?MODULE, findAllAssets))
    catch
        _:_ ->
            init(),
            findAllAssets()
    end.

get_assets()->
    try
        return(gen_server:call(?MODULE, get_assets))
    catch
        _:_ ->
            init(),
            get_assets()
    end.

%%====================================================================
%% Genserver callbacks
%%====================================================================
init(_Args) ->
    State = #state {
        assets = []
    },
    {ok, State}.

handle_call(init, _From, _State) ->
    NewState = #state {
        assets = []
    },
    {reply, ok, NewState};

handle_call({create, Asset = {asset, AssetToCreateData}}, _From, State) ->
    AssetIdToCreate = proplists:get_value(assetId, AssetToCreateData),
    case exists_asset(AssetIdToCreate, State#state.assets) of
        true ->
            {reply, {duplicateInstanceException, []}, State};
        false ->
            NewState = State#state {
                assets = State#state.assets ++ [Asset]
            },
            {reply, ok, NewState}
    end;

handle_call({delete, AssetId}, _From, State) ->
    case exists_asset(AssetId, State#state.assets) of
        false ->
            {reply, {instanceNotFoundException, []}, State};
        true ->
            NewState = State#state {
                assets = lists:filter(
                    fun({asset, AssetData}) ->
                        case proplists:get_value(assetId, AssetData) of
                            AssetId ->
                                false;
                            _ ->
                                true
                        end
                    end, State#state.assets)
            },
            {reply, ok, NewState}
    end;

handle_call(findAllAssets, _From, State) ->
    {reply, State#state.assets, State};

handle_call(get_assets, _From, State) ->
    {reply, State#state.assets, State};

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

exists_asset(AssetId, Assets) ->
    lists:any(
        fun({asset, AssetData}) ->
            case proplists:get_value(assetId, AssetData) of
                AssetId ->
                    true;
                _ ->
                    false
            end
        end, Assets).

