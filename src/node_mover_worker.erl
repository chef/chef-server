%% -*- erlang-indent-level: 4;indent-tabs-mode: nil;fill-column: 92 -*-
%% ex: ts=4 sw=4 et
%% @author Seth Falcon <seth@opscode.com>
%% @copyright 2011 Opscode, Inc.

-module(node_mover_worker).
-behaviour(gen_fsm).
-define(SERVER, ?MODULE).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([start_link/1,
         migrate/1,
         init/1,
         handle_event/3,
         handle_sync_event/4,
         handle_info/3,
         terminate/3,
         code_change/4]).

%% States
-export([mark_migration_start/2,
         verify_read_only/2,
         get_node_list/2,
         migrate_nodes/2,
         mark_migration_end/2]).

-include("node_mover.hrl").
-include_lib("chef_common/include/chef_sql.hrl").

-define(MAX_INFLIGHT_CHECKS, 20).
-define(INFLIGHT_WAIT, 1000).

-record(state, {org_name,
                org_id,
                batch_size,
                chef_otto,
                node_count = 0,
                node_list = [],
                node_marker,
                redis}).

start_link(Config) ->
    gen_fsm:start_link(?MODULE, Config, []).

migrate(Pid) ->
    gen_fsm:send_event(Pid, start).

%% {S, Config} = node_mover:setup().
%% {ok, Pid} = node_mover_worker:start_link(Config).
%% node_mover_worker:migrate(Pid).

init(Config) ->
    OrgName = proplists:get_value(org_name, Config),
    OrgId = proplists:get_value(org_id, Config),
    BatchSize = proplists:get_value(batch_size, Config),
    Otto = proplists:get_value(chef_otto, Config),
    error_logger:info_msg("worker starting with config ~p~n", [Config]),
    {ok, mark_migration_start, #state{org_name = OrgName,
                                      org_id = OrgId,
                                      batch_size = BatchSize,
                                      chef_otto = Otto,
                                      redis = mover_redis:connect()}}.

mark_migration_start(start, #state{org_name = OrgName}=State) ->
    error_logger:info_msg("Starting migration of ~s~n", [OrgName]),
    {next_state, verify_read_only, State, 0}.

verify_read_only(timeout, State) ->
    verify_read_only(0, State);
verify_read_only(N, #state{org_name = OrgName, redis = Redis}=State)
  when is_integer(N) andalso N =< ?MAX_INFLIGHT_CHECKS ->
    case mover_redis:inflight_requests_for_org(Redis, OrgName) of
        [] ->
            error_logger:info_msg("No in-flight writes for ~s, proceeding~n",
                                  [OrgName]),
            mover_redis:delete_tracking(Redis, OrgName),
            {next_state, get_node_list, State, 0};
        _Pids ->
            error_logger:info_msg("waiting for in-flight requests to finish (~s)~n",
                                  [OrgName]),
            timer:apply_after(?INFLIGHT_WAIT, gen_fsm, send_event, [self(), N + 1]),
            {next_state, verify_read_only, State}
    end;
verify_read_only(N, #state{org_name = OrgName}=State) when N > ?MAX_INFLIGHT_CHECKS ->
    error_logger:error_msg("~s exceeded timeout waiting for in-flight requests~n",
                           [OrgName]),
    {stop, normal, State}.


get_node_list(timeout, #state{org_name = OrgName, org_id = OrgId,
                             batch_size = BatchSize,
                             chef_otto = S}=State) ->
    error_logger:info_msg("Migrating nodes for ~s~n", [OrgName]),
    %% here, we'll bulk fetch nodes using batch_size, migrate and then
    %% transition to ourselves. Only when we don't find any nodes do
    %% we transition to the wrap up state.

    %% get full node list.  Write this to a nodes to migrate file
    NodeList = chef_otto:fetch_nodes_with_ids(S, OrgId),
    NodesToMigrate = << <<Name/binary, "\n">> || {Name, _} <- NodeList >>,
    file:write_file(<<OrgName/binary, "_nodes_to_migrate.txt">>, NodesToMigrate),
    
    {next_state, migrate_nodes,
     State#state{node_list = safe_split(BatchSize, NodeList)}, 0}.
    
migrate_nodes(timeout, #state{node_list = {[], []}}=State) ->
    {next_state, mark_migration_end, State, 0};
migrate_nodes(timeout, #state{node_list = {NodeBatch, NodeList},
                             org_name = OrgName,
                             org_id = OrgId,
                             batch_size = BatchSize,
                             chef_otto = S}=State) ->
    error_logger:info_msg("Starting batch ~B nodes (~B remaining) ~s~n",
                          [length(NodeBatch), length(NodeList), OrgName]),
    NodeCouchIds = [ Id || {_, Id} <- NodeBatch ],
    OrgDb = chef_otto:dbname(OrgId),
    NodeDocs = chef_otto:bulk_get(S, OrgDb, NodeCouchIds),
    NodeMeta = [ fetch_node_meta_data(S, OrgName, OrgId, Name, Id,
                                      make_node_cache_validator())
                 || {Name, Id} <- NodeBatch ],
    Ctx = chef_db:make_context(<<"my-req-id-for-mover">>, S),
    write_nodes_to_sql(Ctx, OrgId, lists:zip(NodeMeta, NodeDocs)),
    {next_state, migrate_nodes,
     State#state{node_list = safe_split(BatchSize, NodeList)}, 0}.

mark_migration_end(timeout, #state{org_name = OrgName}=State) ->
    error_logger:info_msg("Ending migration of ~s~n", [OrgName]),
    {stop, normal, State}.
    
handle_event(_Event, StateName, State) ->
    {next_state, StateName, State}.

handle_sync_event(_Event, _From, StateName, State) ->
    {reply, ok, StateName, State}.

handle_info(_Info, StateName, State) ->
    {next_state, StateName, State}.

terminate(_Reason, _StateName, _State) ->
    ok.

code_change(_OldVsn, StateName, State, _Extra) ->
    {ok, StateName, State}.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

write_nodes_to_sql(S, OrgId, [{#node_cache{}=MetaData, NodeData} | Rest]) ->
    #node_cache{name = Name, authz_id = AuthzId,
                id = OrigId,
                requestor_id = RequestorId} = MetaData,
    %% NodeData comes from chef_otto:bulk_get which for some reason is unwrapping the
    %% top-level tuple of the ejson format.  So we restore it here.
    Node = chef_otto:convert_couch_json_to_node_record(OrgId, AuthzId, RequestorId,
                                                       {NodeData}),
    %% validate name match
    Name = ej:get({<<"name">>}, NodeData),
    %% this is where we'd call chef_db:create_node(S, Node) shortcut
    %% will be to pass 'ignore' for first arg since not needed for
    %% emysql backed node creation.
    case chef_db:create_node(S, Node, RequestorId) of
        ok ->
            io:format("migrated: ~p~n", [Name]),
            ok = send_node_to_solr(Node, {NodeData}),
            file:write_file(<<OrgId/binary, "_nodes_complete.txt">>, 
                            iolist_to_binary([Name, <<"\t">>, OrigId, <<"\t">>,
                                              Node#chef_node.id, "\n"]),
                            [append]);
        {conflict, _} ->
            io:format("skipping: ~p (already exists)~n", [Name]);
        Error ->
            error_logger:error_report({node_create_failed, {Node, Error}})
    end,
    write_nodes_to_sql(S, OrgId, Rest);
write_nodes_to_sql(S, OrgId, [{error, _NodeData}|Rest]) ->
    %% FIXME: how should we track errors of this sort if they occur?
    %% This is a case where we found node json, but no matching mixlib
    %% or authz data.
    write_nodes_to_sql(S, OrgId, Rest);
write_nodes_to_sql(_, _, []) ->
    ok.

fetch_node_meta_data(S, OrgName, OrgId, Name, Id, Validator) ->
    Ans = case dets:lookup(all_nodes, {OrgName, Name}) of
        [{{OrgName, Name}, MetaData}] ->
            MetaData;
        _ ->
            %% node data not found in cache, attempt to look it up
            %% here.
            case node_mover:fetch_node_cache(S, OrgId, Name, Id) of
                #node_cache{}=MetaData -> MetaData;
                {error, Why} ->
                    error_logger:warning_report({node_not_found, OrgName, Name, Id,
                                                 Why}),
                    error
            end
    end,
    Validator(Ans).

send_node_to_solr(#chef_node{id = Id, org_id = OrgId}, NodeJson) ->
    ok = chef_index_queue:set(node, Id,
                              chef_otto:dbname(OrgId),
                              chef_node:ejson_for_indexing(NodeJson)),
    ok.

make_node_cache_validator() ->
    {ok, Regex} = re:compile("[a-f0-9]{32}"),
    fun(error) -> error;
       (#node_cache{authz_id = AuthzId, requestor_id = RequestorId}=Input) ->
               case {re:run(AuthzId, Regex), re:run(RequestorId, Regex)} of
                   {{match, _}, {match, _}} -> Input;
                   _ -> error
               end
    end.

safe_split(N, L) ->
    try
        lists:split(N, L)
    catch
        error:badarg ->
            {L, []}
    end.
