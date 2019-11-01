-module(chef_sql_nodes).

-compile([export_all, nowarn_export_all]).

-include_lib("eunit/include/eunit.hrl").
-include("chef_db.hrl").
-include("chef_types.hrl").

%%%======================================================================
%%% NODES
%%%======================================================================
make_node(Prefix) ->
    Id = chef_test_suite_helper:make_id(Prefix),
    AzId = chef_test_suite_helper:make_az_id(Prefix),
    Name = <<"node_", Prefix/binary>>,
    #chef_node{id=Id, authz_id = AzId,
               org_id=chef_test_suite_helper:the_org_id(), name=Name,
               environment="_default", serialized_object= <<"{\"key\":\"fake node\"}">>}.

node_list() ->
    ActorId = chef_test_suite_helper:make_az_id(<<"node-maker">>),
    [ {make_node(<<"01">>), ActorId}, {make_node(<<"02">>), ActorId}].

insert_node_data() ->
    Ctx = chef_db:make_context(?API_MIN_VER, <<"itest-nodes">>),
    Nodes = node_list(),
    Expected = lists:duplicate(length(Nodes), ok),
    Results = [ chef_db:create(Node, Ctx, ActorId) || {Node, ActorId} <- Nodes ],
    ?assertEqual(Expected, Results).
