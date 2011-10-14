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
                redis,
                log_dir}).

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
    filelib:ensure_dir(<<"node_migration_log/", OrgName/binary, "/dummy_for_ensure">>),
    log(info, OrgName, "starting (~B nodes per batch)", [BatchSize]),
    {ok, mark_migration_start, #state{org_name = OrgName,
                                      org_id = OrgId,
                                      batch_size = BatchSize,
                                      chef_otto = Otto,
                                      redis = mover_redis:connect(),
                                      log_dir = <<"node_migration_log/", OrgName/binary>>}}.

mark_migration_start(start, #state{org_name = OrgName}=State) ->
    log(info, OrgName, "starting migration"),
    {next_state, verify_read_only, State, 0}.

verify_read_only(timeout, State) ->
    verify_read_only(0, State);
verify_read_only(N, #state{org_name = OrgName, redis = Redis}=State)
  when is_integer(N) andalso N =< ?MAX_INFLIGHT_CHECKS ->
    case mover_redis:inflight_requests_for_org(Redis, OrgName) of
        [] ->
            log(info, OrgName, "no in-flight writes, proceeding"),
            mover_redis:delete_tracking(Redis, OrgName),
            {next_state, get_node_list, State, 0};
        _Pids ->
            log(info, OrgName, "waiting for in-flight requests to finish"),
            timer:apply_after(?INFLIGHT_WAIT, gen_fsm, send_event, [self(), N + 1]),
            {next_state, verify_read_only, State}
    end;
verify_read_only(N, #state{org_name = OrgName}=State) when N > ?MAX_INFLIGHT_CHECKS ->
    log(err, OrgName, "FAILED: timeout exceeded waiting for in-flight requests"),
    {stop, normal, State}.


get_node_list(timeout, #state{org_name = OrgName, org_id = OrgId,
                             batch_size = BatchSize,
                             chef_otto = S,
                             log_dir = LogDir}=State) ->
    log(info, OrgName, "fetching list of nodes"),
    %% here, we'll bulk fetch nodes using batch_size, migrate and then
    %% transition to ourselves. Only when we don't find any nodes do
    %% we transition to the wrap up state.

    %% get full node list.  Write this to a nodes to migrate file
    NodeList = chef_otto:fetch_nodes_with_ids(S, OrgId),
    log(info, OrgName, "found ~B nodes", [length(NodeList)]),
    NodesToMigrate = << <<Name/binary, "\n">> || {Name, _} <- NodeList >>,
    file:write_file(<<LogDir/binary, "/nodes_to_migrate.txt">>, NodesToMigrate),
    
    {next_state, migrate_nodes,
     State#state{node_list = safe_split(BatchSize, NodeList)}, 0}.
    
migrate_nodes(timeout, #state{node_list = {[], []}}=State) ->
    {next_state, mark_migration_end, State, 0};
migrate_nodes(timeout, #state{node_list = {NodeBatch, NodeList},
                             org_name = OrgName,
                             org_id = OrgId,
                             batch_size = BatchSize,
                             chef_otto = S}=State) ->
    log(info, OrgName, "starting batch ~B nodes (~B remaining)",
        [length(NodeBatch), length(NodeList)]),
    
    OrgDb = chef_otto:dbname(OrgId),
    %% Get the node meta data first via read-through cache.  This way, we can filter out
    %% nodes that don't have authz data (yes, it happens) and avoid pulling the node data
    %% from couch for nodes that were already migrated.
    NodeMeta = fetch_meta_data_for_nodes(S, OrgName, OrgId, NodeBatch),
    %% only bulk-get data for nodes we have meta data on and that have not already been
    %% migrated.
    NodeCouchIds = [ Id || #node{id = Id} <- NodeMeta ],
    NodeDocs = chef_otto:bulk_get(S, OrgDb, NodeCouchIds),
    %% FIXME: do we need to worry about the bulk_get returning fewer ids then input?
    %% let's just assert for now
    case length(NodeCouchIds) =:= length(NodeDocs) of
        true -> ok;
        false ->
            log(err, OrgName, "bulk_get returned wrong number of nodes"),
            throw(bulk_get_node_count_mismatch)
    end,
    Ctx = chef_db:make_context(<<"my-req-id-for-mover">>, S),
    write_nodes_to_sql(Ctx, OrgName, lists:zip(NodeMeta, NodeDocs)),
    {next_state, migrate_nodes,
     State#state{node_list = safe_split(BatchSize, NodeList)}, 0}.

mark_migration_end(timeout, #state{org_name = OrgName}=State) ->
    log(info, OrgName, "migration complete"),
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

write_nodes_to_sql(S, OrgName, [{#node{status = couchdb}=MetaData, NodeData} | Rest]) ->
    #node{id = OrigId, name = Name, authz_id = AuthzId,
          org_id = OrgId,
          requestor = RequestorId} = MetaData,
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
            log(info, OrgName, "migrated node: ~s ~s (~s => ~s)",
                [OrgId, Name, OrigId, Node#chef_node.id]),
            ok = send_node_to_solr(Node, {NodeData}),
            mover_manager:mark_node(complete, OrigId),
            %% FIXME: should be using log_dir, but need to refactor.  ETOOMANYARGS alraedy
            %% :(
            file:write_file(<<"node_migration_log/", OrgName/binary, "/nodes_complete.txt">>,
                            iolist_to_binary([Name, <<"\t">>, OrigId, <<"\t">>,
                                              Node#chef_node.id, "\n"]),
                            [append]);
        {conflict, _} ->
            %% In theory, we won't ever get here since we match on node meta data status of
            %% couchdb.
            log(warn, OrgName, "node skipped: ~s (already exists)", [Name]);
        Error ->
            error_logger:error_report({node_create_failed, {Node, Error}}),
            log(err, OrgName, "node create failed"),
            fast_log:err(node_errors, OrgName, "~p", [{Node, Error}]),
            mover_manager:mark_node(error, OrigId, Error)
    end,
    write_nodes_to_sql(S, OrgName, Rest);
write_nodes_to_sql(S, OrgName, [{#node{name = Name, status = mysql}, _} | Rest]) ->
    %% Note that this is here for safetly. Based on how this function is called, only good
    %% unmigrated nodes will be provided.
    log(warn, OrgName, "node skipped: ~s (already exists)", [Name]),
    write_nodes_to_sql(S, OrgName, Rest);
write_nodes_to_sql(S, OrgName, [{#node{status={error, _}}, _}|Rest]) ->
    %% FIXME: how should we track errors of this sort if they occur?
    %% This is a case where we found node json, but no matching mixlib
    %% or authz data.
    write_nodes_to_sql(S, OrgName, Rest);
write_nodes_to_sql(_, _, []) ->
    ok.

fetch_meta_data_for_nodes(S, OrgName, OrgId, NodeBatch) ->
    fetch_meta_data_for_nodes(S, OrgName, OrgId, NodeBatch, [],
                              make_node_cache_validator()).

%% @doc Given a list of all org nodes as {Name, Id} pairs, fetch meta data for the node from
%% the cache or read through chef_otto to fetch the meta data.  The returned list may be
%% smaller than the input list of nodes as we filter out any nodes for which we cannot find
%% meta data as well as any nodes that have already been migrated (cache meta data has state
%% other than 'couchdb').
fetch_meta_data_for_nodes(S, OrgName, OrgId, [{Name, Id}|Rest], Acc, Validator) ->
    Acc1 = case dets:lookup(all_nodes, Id) of
               [#node{status = couchdb}=Node] ->
                   add_if_valid(Validator(Node), Acc);
               [] ->
                   %% node data not found in cache, attempt to look it up
                   %% here.
                   case mover_manager:store_node(S, OrgId, Id, Name) of
                       #node{status = couchdb}=Node ->
                           add_if_valid(Validator(Node), Acc);
                       #node{status = {error, Why}} ->
                           log(err, OrgName, "unable to get meta data for node ~s ~s",
                               [Name, Id]),
                           fast_log:err(node_errors, OrgName, "no meta data:~n~p",
                                        [{Name, Id, Why}]),
                           error_logger:error_report({node_not_found,
                                                      OrgName, Name, Id, Why}),
                           Acc
                   end
           end,
    fetch_meta_data_for_nodes(S, OrgName, OrgId, Rest, Acc1, Validator);
fetch_meta_data_for_nodes(_S, _OrgName, _OrgId, [], Acc, _Validator) ->
    Acc.

send_node_to_solr(#chef_node{id = Id, org_id = OrgId}, NodeJson) ->
    case is_dry_run() of
        true -> ok;
        false ->
            ok = chef_index_queue:set(node, Id,
                                      chef_otto:dbname(OrgId),
                                      chef_node:ejson_for_indexing(NodeJson)),
            ok
    end.

make_node_cache_validator() ->
    {ok, Regex} = re:compile("[a-f0-9]{32}"),
    fun(error) -> error;
       (#node{authz_id = AuthzId, requestor = RequestorId}=Input) 
          when is_binary(AuthzId) andalso is_binary(RequestorId) ->
               case {re:run(AuthzId, Regex), re:run(RequestorId, Regex)} of
                   {{match, _}, {match, _}} -> Input;
                   _ -> 
                       fast_log:err(node_errors, "invalid: ~256P", [Input, 20]),
                       error
               end;
       (Input) -> 
            fast_log:err(node_errors, "invalid: ~256P", [Input, 20]),
            error
    end.

add_if_valid(error, Acc) ->
    Acc;
add_if_valid(Item, Acc) ->
    [Item|Acc].

safe_split(N, L) ->
    try
        lists:split(N, L)
    catch
        error:badarg ->
            {L, []}
    end.

is_dry_run() ->
    {ok, DryRun} = application:get_env(mover, dry_run),
    DryRun.

log(Level, OrgName, Msg) ->
    fast_log:Level(mover_worker_log, OrgName, Msg).

log(Level, OrgName, Fmt, Args) when is_list(Args) ->
    fast_log:Level(mover_worker_log, OrgName, Fmt, Args).
