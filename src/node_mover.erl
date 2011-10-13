%% -*- erlang-indent-level: 4;indent-tabs-mode: nil;fill-column: 92 -*-
%% ex: ts=4 sw=4 et
%% @author Seth Falcon <seth@opscode.com>
%% @copyright 2011 Opscode, Inc.

-module(node_mover).

%% TODO:
%% Consider turning this module into gen_fsm behavior with the following states:
%%
%% load_orgs
%%   loads/creates org name/id table
%% load_nodes_for_orgs
%%   loads/creates node meta data cache for all orgs
%% ready_to_migrate_orgs
%%   waits to be told how many batches of orgs to migrate
%% start_migration_of_orgs
%%   mark orgs in org dets table as in-progress
%%   send POST to darklaunch to mark next batch of orgs read-only
%%   spawn a node_mover_worker for each org
%% waiting_for_orgs
%%   waits for node_mover_worker processes to complete.  Maintains counter.  Transitions to
%%   next state when all complete.  Needs to also be able to track crashed workers.
%% orgs_complete
%%   mark orgs in dets table as complete or failed
%%   POST to get nginx to enable migrated orgs
%%   goto ready_to_migrate_orgs
%%
%% NOTES:
%%
%% - should the org table be moved entirely to the db?  Then we get the persistence and
%%   would have a single place to track org migration state.

-export([connect/2,
         load_orgs/1,
         load_nodes_for_org/3,
         load_all_nodes/1,
         open_table/3,
         init_dets_tables/1,
         shutdown/0,
         fetch_node_cache/4,
         setup/0
        ]).

-include("node_mover.hrl").

setup() ->
    ibrowse:start(),
    S = node_mover:connect("localhost", 8484),
    init_dets_tables(S),
    Config = [{org_name, <<"userprimary">>}, {org_id, <<"60d3ed4da757402ea5dd6da9131baeef">>}, {batch_size, 3}, {chef_otto, S}],
    {S, Config}.

%% {ok, Pid} = node_mover_worker:start_link(Config).
%% node_mover_worker:migrate(Pid).

%% setup() ->
%%     application:set_env(chef_common, mysql_host, "localhost"),
%%     application:set_env(chef_common, mysql_port, 3306),
%%     application:set_env(chef_common, mysql_user, "dev"),
%%     application:set_env(chef_common, mysql_pass, "opensesame"),
%%     application:set_env(chef_common, mysql_db_name, "opscode_chef"),
%%     application:set_env(chef_common, mysql_pool_size, 5),
%%     application:start(

connect(Host, Port) ->
    chef_otto:connect(Host, Port).

shutdown() ->
    dets:close(all_orgs),
    dets:close(all_nodes),
    dets:close(error_nodes),
    ok.

init_dets_tables(S) ->
    open_table(all_orgs, ?ORG_ESTIMATE, fun() -> load_orgs(S) end),
    open_table(all_nodes, ?NODE_ESTIMATE, fun() -> ok end),
    open_table(error_nodes, 1000, fun() -> ok end).

open_table(Table, Count, Fun) ->
    case dets:info(Table) of
        undefined ->
            DetsFile = atom_to_list(Table) ++ ".dets",
            {ok, Table} = dets:open_file(Table,
                                         [{file, DetsFile},
                                          {estimated_no_objects, Count}]),
            case proplists:get_value(size, dets:info(Table)) of
                0 -> Fun();
                N -> {ok, {existing, N}}
            end;
        _Info ->
            already_opened
    end.

load_orgs(S) ->
    [ dets:insert(all_orgs, {Name, Guid})
      || {Name, Guid} <- chef_otto:fetch_assigned_orgs(S) ],
    N = proplists:get_value(size, dets:info(all_orgs)),
    {ok, {loaded, N}}.

load_all_nodes(S) ->
    %% assumes load_orgs has been called
    GuidCheck = make_guid_checker(),
    dets:foldl(fun({OrgName, OrgId}, {ok, Acc}) ->
                       case GuidCheck(OrgId) of
                           ok ->
                               ok = load_nodes_for_org(S, OrgName, OrgId),
                               {ok, Acc + 1};
                           error ->
                               error_logger:error_msg("skipping org '~s' with bad id '~s'~n",
                                                      [OrgName, OrgId]),
                               {ok, Acc}
                       end
              end, {ok, 0}, all_orgs).

load_nodes_for_org(S, OrgName, OrgId) ->
    NodeList = chef_otto:fetch_nodes_with_ids(S, OrgId),
    [ store_node_cache(S, OrgName, OrgId, Name, Id)
      || {Name, Id} <- NodeList ],
    ok.

store_node_cache(S, OrgName, OrgId, NodeName, NodeId) ->
    case fetch_node_cache(S, OrgId, NodeName, NodeId) of
        #node_cache{}=Cache ->
            dets:insert(all_nodes, {{OrgName, NodeName}, Cache});
        {error, Why} ->
            dets:insert(error_nodes, {{OrgName, NodeName}, Why})
    end.

fetch_node_cache(S, OrgId, NodeName, NodeId) ->
    case chef_otto:fetch_by_name(S, OrgId, NodeName, authz_node) of
        {ok, MixlibNode} ->
            MixlibId = ej:get({<<"_id">>}, MixlibNode),
	    AuthzId = chef_otto:fetch_auth_join_id(S, MixlibId, user_to_auth),
	    RequestorId = ej:get({<<"requester_id">>}, MixlibNode),
            #node_cache{name = NodeName, id = NodeId,
                        authz_id = AuthzId,
                        requestor_id = RequestorId};
        Error ->
            {error, Error}
    end.

make_guid_checker() ->
    {ok, Regex} = re:compile("[a-f0-9]{32}"),
    fun(Id) ->
            case re:run(Id, Regex) of
                nomatch -> error;
                {match, _} -> ok
            end
    end.
