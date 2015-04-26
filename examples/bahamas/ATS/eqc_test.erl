-module(eqc_test).

-include_lib("eqc/include/eqc.hrl").
-include_lib("eqc/include/eqc_statem.hrl").

-behaviour(eqc_statem).

%% EQC callbacks
-export([initial_state/0, command/1, precondition/2, postcondition/3, next_state/3]).

%% Wrapper functions
-export([create/1, delete/1, findAllAssets/0]).

%% Testing function
-export([test/0]).


%% State
-record(ts, {
    assets = undefined
}).

%%====================================================================
%% property
%%====================================================================

test() ->
    ?FORALL(Cmds, commands(?MODULE),
        begin
            {H, PreState, Res} = run_commands(?MODULE, Cmds),
            ?WHENFAIL(io:format("H: ~p\nPreState: ~p\nRes: ~p\n", [H, PreState, Res]),
                aggregate(command_names(Cmds), Res==ok))
        end).

%%====================================================================
%% command
%%====================================================================

command(PreState) ->
    eqc_gen:oneof([
        {call, ?MODULE, create, [gen_asset()]}, 
        {call, ?MODULE, delete, [ocl_gen:gen_string()]}, 
        {call, ?MODULE, findAllAssets, []}
    ]).

%%====================================================================
%% initial_state
%%====================================================================

%%--------------------------------------------------------------------
%% initial_state: 
%%--------------------------------------------------------------------
initial_state() ->
    #ts {
        assets = assetFacade:get_assets()
    }.

%%====================================================================
%% precondition
%%====================================================================

%%--------------------------------------------------------------------
%% create: preconditions
%%--------------------------------------------------------------------
precondition(PreState, {call, ?MODULE, create, [Asset]})->
    true;

%%--------------------------------------------------------------------
%% delete: preconditions
%%--------------------------------------------------------------------
precondition(PreState, {call, ?MODULE, delete, [AssetId]})->
    true;

%%--------------------------------------------------------------------
%% findAllAssets: preconditions
%%--------------------------------------------------------------------
precondition(PreState, {call, ?MODULE, findAllAssets, []})->
    true;

%%--------------------------------------------------------------------
%% preconditions: default
%%--------------------------------------------------------------------
precondition(_PreState, _C) ->
    false.

%%====================================================================
%% next_state
%%====================================================================

%%--------------------------------------------------------------------
%% next_state: create
%%--------------------------------------------------------------------
next_state(PreState, Result, {call, ?MODULE, create, [Asset]}) ->
    DynAfterState = {call, erlang, element, [3, Result]},
    PreState22 = case (ocl_seq:size(ocl_seq:filter(fun(A) -> (ocl_datatypes:get_property(assetId, A) == ocl_datatypes:get_property(assetId, Asset)) end, PreState#ts.assets)) > 0) of 
        true -> 
            PreState23 = PreState#ts {
                assets = PreState#ts.assets
            }, 
            PreState23;
        false -> 
            PreState24 = PreState#ts {
                assets = ocl_seq:including(Asset, PreState#ts.assets)
            }, 
            PreState24
    end, 
    PreState22;

%%--------------------------------------------------------------------
%% next_state: delete
%%--------------------------------------------------------------------
next_state(PreState, Result, {call, ?MODULE, delete, [AssetId]}) ->
    DynAfterState = {call, erlang, element, [3, Result]},
    PreState25 = case (ocl_seq:size(ocl_seq:filter(fun(A) -> (ocl_datatypes:get_property(assetId, A) == AssetId) end, PreState#ts.assets)) > 0) of 
        true -> 
            PreState26 = PreState#ts {
                assets = ocl_seq:filter(fun(Asset) -> (ocl_datatypes:get_property(assetId, Asset) /= AssetId) end, PreState#ts.assets)
            }, 
            PreState26;
        false -> 
            PreState27 = PreState#ts {
                assets = PreState#ts.assets
            }, 
            PreState27
    end, 
    PreState25;

%%--------------------------------------------------------------------
%% next_state: findAllAssets
%%--------------------------------------------------------------------
next_state(PreState, Result, {call, ?MODULE, findAllAssets, []}) ->
    DynAfterState = {call, erlang, element, [3, Result]},
    PreState28 = PreState#ts {
        assets = PreState#ts.assets
    }, 
    PreState28;

%%--------------------------------------------------------------------
%% next_state: default
%%--------------------------------------------------------------------
next_state(PreState, Result, C) ->
    PreState.

%%====================================================================
%% postcondition
%%====================================================================

postcondition(PreState, C, R)->
    AfterState = eqc_symbolic:eval(next_state(PreState, R, C)), 
    postcondition(PreState, AfterState, C, R).

%%--------------------------------------------------------------------
%% create: postconditions
%%--------------------------------------------------------------------    
postcondition(PreState, AfterState, {call, ?MODULE, create, [Asset]}, {Result, DynPreState, DynAfterState})->
    case (ocl_seq:size(ocl_seq:filter(fun(A) -> (ocl_datatypes:get_property(assetId, A) == ocl_datatypes:get_property(assetId, Asset)) end, PreState#ts.assets)) > 0) of 
        true -> 
            (ocl_seq:eq(AfterState#ts.assets, PreState#ts.assets) andalso ocl_datatypes:typeof(Result, duplicateInstanceException));
        false -> 
            ocl_seq:eq(AfterState#ts.assets, ocl_seq:including(Asset, PreState#ts.assets))
    end;

%%--------------------------------------------------------------------
%% delete: postconditions
%%--------------------------------------------------------------------    
postcondition(PreState, AfterState, {call, ?MODULE, delete, [AssetId]}, {Result, DynPreState, DynAfterState})->
    case (ocl_seq:size(ocl_seq:filter(fun(A) -> (ocl_datatypes:get_property(assetId, A) == AssetId) end, PreState#ts.assets)) > 0) of 
        true -> 
            (ocl_seq:eq(AfterState#ts.assets, ocl_seq:filter(fun(Asset) -> (ocl_datatypes:get_property(assetId, Asset) /= AssetId) end, PreState#ts.assets)) andalso (ocl_seq:size(ocl_seq:filter(fun(Asset) -> (ocl_datatypes:get_property(assetId, Asset) == AssetId) end, AfterState#ts.assets)) == 0));
        false -> 
            (ocl_seq:eq(AfterState#ts.assets, PreState#ts.assets) andalso ocl_datatypes:typeof(Result, instanceNotFoundException))
    end;

%%--------------------------------------------------------------------
%% findAllAssets: postconditions
%%--------------------------------------------------------------------    
postcondition(PreState, AfterState, {call, ?MODULE, findAllAssets, []}, {Result, DynPreState, DynAfterState})->
    (ocl_seq:eq(AfterState#ts.assets, PreState#ts.assets) andalso ocl_seq:eq(Result, AfterState#ts.assets));

%%--------------------------------------------------------------------
%% postconditions: default
%%--------------------------------------------------------------------
postcondition(_PreState, _AfterState, _C, _R) ->
    false.

%%====================================================================
%% generators
%%====================================================================

%%--------------------------------------------------------------------
%% generator: asset
%%--------------------------------------------------------------------
gen_asset() ->
    {asset, [{assetId, ocl_gen:gen_string()}, {title, ocl_gen:gen_string()}, {summary, ocl_gen:gen_string()}]}.


%%====================================================================
%% wrappers
%%====================================================================

create(Asset)->
    DynPreState = #ts {
        
    }, 
    Result = assetFacade:create(Asset),
    DynAfterState = #ts {
        assets = assetFacade:get_assets()
    }, 
    {Result, DynPreState, DynAfterState}.

delete(AssetId)->
    DynPreState = #ts {
        
    }, 
    Result = assetFacade:delete(AssetId),
    DynAfterState = #ts {
        assets = assetFacade:get_assets()
    }, 
    {Result, DynPreState, DynAfterState}.

findAllAssets()->
    DynPreState = #ts {
        
    }, 
    Result = assetFacade:findAllAssets(),
    DynAfterState = #ts {
        assets = assetFacade:get_assets()
    }, 
    {Result, DynPreState, DynAfterState}.
