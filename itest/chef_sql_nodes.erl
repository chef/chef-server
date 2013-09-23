-module(chef_sql_nodes).

-compile([export_all]).

-include_lib("eunit/include/eunit.hrl").
-include_lib("chef_db/include/chef_db.hrl").
-include_lib("chef_objects/include/chef_types.hrl").

%%%======================================================================
%%% NODES
%%%======================================================================
make_node(Prefix) ->
    Id = itest_util:make_id(Prefix),
    AzId = itest_util:make_az_id(Prefix),
    Name = <<"node_", Prefix/binary>>,
    #chef_node{
                id=Id, authz_id=AzId, org_id=itest_util:the_org_id(), name=Name,
                environment="_default", last_updated_by="noone", serialized_object= <<"">>,
                created_at= {datetime,{{2011,10,1},{16,47,46}}}, updated_at= {datetime,{{2011,10,1},{16,47,46}}} }.

node_list() ->
    [ make_node(<<"01">>) ].

insert_node_data() ->
    Nodes = node_list(),
    Expected = lists:duplicate(length(Nodes), {ok, 1}),
    Results = [chef_sql:create_node(Node) || Node <- Nodes ],
    ?assertEqual(Expected, Results).
