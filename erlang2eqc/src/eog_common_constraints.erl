%% Author: miguelafr
%% Created: 26/02/2012
%% Description: eog_common_contraints
-module(eog_common_constraints).

%%====================================================================

%% Include files
%%====================================================================

%%====================================================================
%% Exported functions
%%====================================================================
-export([group_constraints/1]).

%%====================================================================
%% API
%%====================================================================
%%--------------------------------------------------------------------
%% group_constraints
%%--------------------------------------------------------------------
group_constraints([])->
    [];

group_constraints([Constraint | []]) ->
    [Constraint];

group_constraints(Constraints) ->
    [{constraint, [
                   {name, ""},
                   {body, do_group_constraints(Constraints)}
                  ]}].

%%====================================================================
%% Internal functions
%%====================================================================
%%--------------------------------------------------------------------
%% do_group_constraints
%%--------------------------------------------------------------------
do_group_constraints([{constraint, ConstraintData} | []])->
    proplists:get_value(body, ConstraintData);

do_group_constraints([{constraint, ConstraintData} | MoreConstraints])->
    [
     {expression, "OperationCallExp"},
     {name, ""},
     {referredOperation, [
                          {name, "and"},
                          {type, [
                                  {name, "Boolean"},
                                  {qualifiedName, "Boolean"}
                                 ]}
                         ]},
     {contents, [
                 {content, proplists:get_value(body, ConstraintData)},
                 {content, do_group_constraints(MoreConstraints)}
                ]
     }
    ];

do_group_constraints([])->
    [].
