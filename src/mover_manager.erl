%% -*- erlang-indent-level: 4;indent-tabs-mode: nil;fill-column: 92 -*-
%% ex: ts=4 sw=4 et
%% @author Kevin Smith <kevin@opscode.com>
%% @author Seth Falcon <seth@opscode.com>
%% @copyright 2011 Opscode, Inc.
-module(mover_manager).

-behaviour(gen_fsm).

%% API
-export([start_link/1,
         get_going/3,
         status/0,
         mark_node/2,
         mark_node/3,
         mark_org_time/2,
         store_node/5,
         make_worker_config/3,
         darklaunch_couchdb_nodes/2
        ]).

%% States
-export([init_storage/2,
         load_orgs/2,
         preload_org_nodes/2,
         label_orgs_in_darklaunch/2,
         ready/3,
         start_batch/2,
         running/3]).

%% gen_fsm callbacks
-export([init/1,
         handle_event/3,
         handle_sync_event/4,
         handle_info/3,
         terminate/3,
         code_change/4]).

-include("mover.hrl").

-define(SERVER, ?MODULE).
-define(DETS_OPTS(EstSize), [{auto_save, 1000},
                             {keypos, 2},
                             {estimated_no_objects, EstSize}]).
-define(ORG_SPEC(Preloaded, Active, Migrated, Error),
        ?wildcard_org_spec#org{
           guid = '$1',
           name = '$2',
           preloaded = Preloaded,
           active = Active,
           migrated = Migrated,
           error = Error}).

-record(state, {couch_cn,
                preload_amt,
                workers = 0,
                batches = 0,
                orgs_per_batch = 10,
                node_batch_size = 20,
                orgs_to_migrate}).

start_link(PreloadAmt) ->
    gen_fsm:start_link({local, ?SERVER}, ?MODULE, [PreloadAmt], []).

status() ->
    gen_fsm:sync_send_all_state_event(?SERVER, status, 10000).

get_going(NumBatches, OrgsPerBatch, NodeBatchSize) ->
    gen_fsm:sync_send_event(?SERVER, {start, NumBatches, OrgsPerBatch, NodeBatchSize}).

init([PreloadAmt]) ->
    {ok, init_storage, #state{preload_amt=PreloadAmt}, 0}.

init_storage(timeout, State) ->
    error_logger:info_msg("dry_run is ~p~n", [is_dry_run()]),
    error_logger:info_msg("initializing migration storage~n"),
    log(info, "initializing migration storage"),
    {ok, _} = dets:open_file(all_orgs, ?DETS_OPTS(?ORG_ESTIMATE)),
    {ok, _} = dets:open_file(all_nodes, ?DETS_OPTS(?NODE_ESTIMATE)),
    {ok, _} = dets:open_file(error_nodes, ?DETS_OPTS(10000)),
    {next_state, load_orgs, State, 0}.

load_orgs(timeout, State) ->
    error_logger:info_msg("loading unassigned orgs~n"),
    log(info, "loading unassigned orgs"),
    Cn = chef_otto:connect(),
    [insert_org(NameGuid) || NameGuid <- chef_otto:fetch_assigned_orgs(Cn)],
    Summary = mover_status:summarize_orgs(),
    error_logger:info_msg("loaded orgs: ~p~n", [Summary]),
    log(info, "~256P", [Summary, 10]),
    {next_state, preload_org_nodes, State#state{couch_cn=Cn}, 0}.

preload_org_nodes(timeout, #state{preload_amt=Amt}=State) ->
    error_logger:info_msg("preloading nodes for ~B orgs~n", [Amt]),
    log(info, "preloading nodes for ~B orgs", [Amt]),
    case preload_orgs(Amt, State) of
        {ok, State1} ->
            {Preloaded, Active, Migrated, Error} = {true, false, false, false},
            Spec = ?ORG_SPEC(Preloaded, Active, Migrated, Error),
            Candidates = case ?fix_table(all_orgs, dets:match(all_orgs, Spec)) of
                             {error, Why} -> throw({error, Why});
                             Data ->
                                 [{Guid, Name} || [Guid, Name] <- Data]
                         end,
            Summary = mover_status:summarize_orgs(),
            log(info, "preloading complete"),
            error_logger:info_msg("org summary after preloading: ~p~n", [Summary]),
            log(info, "~256P", [Summary, 10]),
            error_logger:info_msg("preloading complete~n"),
            {next_state, label_orgs_in_darklaunch, State1#state{orgs_to_migrate = Candidates}, 0};
        Error ->
            error_logger:error_msg("preloading failed~n"),
            {stop, Error, State}
    end.

label_orgs_in_darklaunch(timeout, State) ->
    error_logger:info_msg("marking all unmigrated orgs in nginx~n"),
    %% this will find all unmigrated orgs and put them in the couchdb list in nginx
    ok = route_orgs_to_erchef_sql(),
    error_logger:info_msg("marking all unmigrated orgs in darklaunch 'couchdb_nodes'~n"),
    ok = darklaunch_couchdb_nodes_for_all(),
    error_logger:info_msg("ready for action~n"),
    {next_state, ready, State}.

ready({start, NumBatches, OrgsPerBatch, NodeBatchSize}, _From,
      #state{workers = 0, batches = 0}=State) ->
    State1 = State#state{batches = NumBatches,
                         orgs_per_batch = OrgsPerBatch,
                         node_batch_size = NodeBatchSize},
    {reply, {ok, moveit}, start_batch, State1, 0}.

start_batch(timeout, #state{workers = 0,
                            batches = Batches,
                            orgs_per_batch = NumOrgs,
                            node_batch_size = NodeBatchSize,
                            orgs_to_migrate = Candidates}=State) ->
    case safe_split(NumOrgs, Candidates) of
        {[], _} ->
            error_logger:info_msg("no migration candidates~n"),
            log(info, "no migration candidates"),
            {next_state, ready, State#state{batches = 0, orgs_to_migrate = []}};
        {Orgs, OrgsRest} ->
            %% Tell darklaunch to put these orgs into read-only
            %% mode for nodes.
            darklaunch_read_only_nodes([ Name || {_, Name} <- Orgs ], true),
            [ mark_org(read_only, OrgId) || {OrgId, _} <- Orgs ],
            case start_workers(Orgs, NodeBatchSize) of
                0 ->
                    error_logger:error_msg("unable to start workers~n"),
                    log(err, "unable to start workers for: ~256P", [[ Name || {_, Name} <- Orgs ], 10]),
                    {next_state, ready, State};
                Count ->
                    BatchesLeft = Batches - 1,
                    log(info, "~B workers ok, ~B batches to go, ~B candidate orgs",
                        [Count, BatchesLeft, length(OrgsRest)]),
                    State1 = State#state{workers=Count, batches = BatchesLeft,
                                         orgs_to_migrate = OrgsRest},
                    {next_state, running, State1}
            end
    end.
    
running({start, _BatchSize, _NodeBatchSize}, _From, State) ->
    {reply, {error, running_batch}, running, State}.

handle_event(_Event, StateName, State) ->
    {next_state, StateName, State}.

handle_sync_event(status, _From, StateName, State) when StateName =:= init_storage;
                                                        StateName =:= load_orgs;
                                                        StateName =:= preload_org_nodes ->
    {reply, {busy, StateName}, StateName, State};
handle_sync_event(status, _From, StateName, #state{workers=Workers, batches = Batches}=State) ->
    Summary = [{orgs_in_progress, Workers}, {batches_left, Batches}],
    {reply, {ok, Summary}, StateName, State};
handle_sync_event(_Event, _From, StateName, State) ->
    {next_state, StateName, State}.

handle_info({'DOWN', _MRef, process, Pid, normal}, StateName,
            #state{workers = Workers, batches = Batches}=State) when Workers > 0 ->
    case find_org_by_worker(Pid) of
        #org{}=Org ->
            mark_org(migrated, Org#org.guid),
            darklaunch_couchdb_nodes(Org#org.name, false),
            %% the org is now marked as migrated and we regenerate the list of unmigrated
            %% orgs and send updates to our nginx lbs.  Marking the orgs as not_read_only is
            %% only for accounting so that we can find orgs that are migrated, but not
            %% turned on in nginx later.
            case route_orgs_to_erchef_sql() of
                ok ->
                    %% marking not_read_only sets stop time on org
                    Org1 = mark_org(not_read_only, Org#org.guid),
                    Total = proplists:get_value(total, mover_status:migration_time(Org1)),
                    log(info, "~s routed to erchef. Migration time: ~256P",
                        [Org1#org.name, Total, 50]);
                _Ignore ->
                    log(err, "~s FAILED to route to erchef", [Org#org.name]),
                    ok
            end,
            WorkerCount = Workers - 1,
            State1 = State#state{workers = WorkerCount},
            case {WorkerCount > 0, Batches > 0} of
                {true, _} ->                    % workers still working on batch
                    {next_state, StateName, State1};
                {false, true} ->                % workers done, more batches to do
                    {next_state, start_batch, State1, 0};
                {false, false} ->               % workers done, no batches left
                    {next_state, ready, State1}
            end;
        _NotFound ->
            %% ignore the msg
            {next_state, StateName, State}
    end;
handle_info({'DOWN', _MRef, process, Pid, _Failed}, StateName,
            #state{workers = Workers}=State) when Workers > 0 ->
    case find_org_by_worker(Pid) of
        #org{}=Org ->
            mark_org(nodes_failed, Org#org.guid),
            error_logger:error_msg("nodes failed for org ~s~n", [Org#org.name]),
            log(err, "nodes failed for org ~s", [Org#org.name]),
            %% this org has failed nodes.  To minimize downtime for this org, we will fail
            %% the migration and turn on writes back in couch-land.
            darklaunch_read_only_nodes(Org#org.name, false),
            WorkerCount = Workers - 1,
            NextState = case WorkerCount > 0 of
                            true -> StateName;
                            false -> ready
                        end,
            %% since there was a node failure, we'll make this the last batch for this run
            %% by setting batches to zero.
            {next_state, NextState, State#state{workers = WorkerCount, batches = 0}};
        _NotFound ->
            %% ignore the msg
            {next_state, StateName, State}
    end;
handle_info(_Info, StateName, State) ->
    {next_state, StateName, State}.

terminate(_Reason, _StateName, _State) ->
    ok.

code_change(_OldVsn, StateName, State, _Extra) ->
    {ok, StateName, State}.

%% Internal functions

insert_org({Name, Guid}) ->
    case dets:lookup(all_orgs, Guid) of
        [] ->
            Org = #org{guid=Guid, name=Name},
            dets:insert(all_orgs, Org);
        [Org] ->
            %% XXX: we assume we are only inserting orgs at startup and not concurrently
            %% ad-hoc.  Any org that is being inserted is then by-definition not active.  If
            %% the manager crashes, there may be an org left active with a stale worker, so
            %% we'll reset that here.
            %% 
            %% Notice that we don't touch the 'migrated' field used for candidate selection
            %% and skipping over completed orgs.
            dets:insert(all_orgs, Org#org{active=false, worker=undefined})
    end.

preload_orgs(BatchSize, State) ->
    case find_preload_candidates(BatchSize) of
        {ok, none} ->
            {ok, State};
        {ok, Orgs} ->
            load_org_nodes(Orgs, State);
        Error ->
            Error
    end.

load_org_nodes([], State) ->
    {ok, State};
load_org_nodes([{OrgId, OrgName}|T], #state{couch_cn=Cn}=State) ->
    log(info, "~s: preloading node data", [OrgName]),
    NodeList = chef_otto:fetch_nodes_with_ids(Cn, OrgId),
    log(info, "~s: found ~B nodes for preloading", [OrgName, length(NodeList)]),
    [store_node(Cn, OrgName, OrgId, NodeId, NodeName) || {NodeName, NodeId} <- NodeList],
    mark_org(preload, OrgId),
    log(info, "~s: preloading complete", [OrgName]),
    load_org_nodes(T, State).

find_preload_candidates(BatchSize) ->
    %% dets:match/3 with N does not act like ets:match/3 with Limit. So to keep it simple,
    %% we waste some memory and just pull back all candidate orgs for preloading and then
    %% return the desired batch size.  This isn't so bad since for the actual migration we
    %% want to preload all orgs. An alternative would be to use dets:traverse/2 doing the
    %% match "manually" and accumulating the desired number before returning {done, Value}.
    %%
    {Preloaded, Active, Migrated, Error} = {false, false, false, false},
    Spec = ?ORG_SPEC(Preloaded, Active, Migrated, Error),
    case ?fix_table(all_orgs, dets:match(all_orgs, Spec)) of
        {error, Why} ->
            {error, Why};
        [] ->
            {ok, none};
        Data ->
            {Batch, _} = safe_split(BatchSize, Data),
            Orgs = [{Guid, Name} || [Guid, Name] <- Batch],
            {ok, Orgs}
    end.

store_node(Cn, OrgName, OrgId, NodeId, NodeName) ->
    Node = case chef_otto:fetch_by_name(Cn, OrgId, NodeName, authz_node) of
               {ok, MixlibNode} ->
                   MixlibId = ej:get({<<"_id">>}, MixlibNode),
                   %% Note that this can return a {not_found, _} tuple so we use
                   %% status_for_ids to validate that we have binaries and otherwise mark
                   %% node as an error.
                   AuthzId = chef_otto:fetch_auth_join_id(Cn, MixlibId, user_to_auth),
                   RequestorId = ej:get({<<"requester_id">>}, MixlibNode),
                   #node{id = NodeId,
                         name = NodeName,
                         org_id = OrgId,
                         authz_id = AuthzId,
                         requestor = RequestorId,
                         status = status_for_ids(AuthzId, RequestorId)};
               Error ->
                   #node{id = NodeId,
                         name = NodeName,
                         org_id = OrgId,
                         status = status_for_error(Error)}
           end,
    dets:insert(all_nodes, Node),
    log_node_stored(OrgName, Node),
    Node.

status_for_error({not_found, authz_node}) ->
    {error, {missing_authz, no_mixlib_doc}};
status_for_error(Error) ->
    {error, Error}.

status_for_ids(AuthzId, RequestorId) when is_binary(AuthzId),
                                          is_binary(RequestorId) ->
    {ok, Regex} = re:compile("[a-f0-9]{32}"),
    case {re:run(AuthzId, Regex), re:run(RequestorId, Regex)} of
        {{match, _}, {match, _}} -> couchdb;
        _NoMatch -> 
            {error,
             {bad_id_format, [{authz_id, AuthzId}, {requestor, RequestorId}]}}
    end;
status_for_ids({not_found, missing}, _RequestorId) ->
    {error, {missing_authz, no_auth_join}};
status_for_ids(AuthzId, RequestorId) ->
    {error, {unknown, [{authz_id, AuthzId}, {requestor, RequestorId}]}}.

mark_org(preload, OrgId) ->
    case dets:lookup(all_orgs, OrgId) of
        [] ->
            ok;
        [Org] ->
            Org1 = Org#org{preloaded=true},
            ok = dets:insert(all_orgs, Org1),
            Org1
    end;
mark_org(read_only, OrgId) ->
    case dets:lookup(all_orgs, OrgId) of
        [] ->
            ok;
        [Org] ->
            StartTime = {start, os:timestamp()},
            Time = [StartTime|Org#org.time],
            Org1 = Org#org{read_only=true, time = Time},
            ok = dets:insert(all_orgs, Org1),
            Org1
    end;
mark_org(not_read_only, OrgId) ->
    case dets:lookup(all_orgs, OrgId) of
        [] ->
            ok;
        [Org] ->
            EndTime = {stop, os:timestamp()},
            Time = [EndTime|Org#org.time],
            Org1 = Org#org{read_only=false, time = Time},
            ok = dets:insert(all_orgs, Org1),
            Org1
    end;
mark_org(migrated, OrgId) ->
    case dets:lookup(all_orgs, OrgId) of
        [] ->
            ok;
        [Org] ->
            Org1 = Org#org{migrated=true, active=false, worker=undefined},
            ok = dets:insert(all_orgs, Org1),
            Org1
    end;
mark_org(nodes_failed, OrgId) ->
    case dets:lookup(all_orgs, OrgId) of
        [] ->
            ok;
        [Org] ->
            Org1 = Org#org{migrated=false, error=true, worker=undefined},
            ok = dets:insert(all_orgs, Org1),
            Org1
    end.

mark_org_time(Tag, OrgId) ->
    case dets:lookup(all_orgs, OrgId) of
        [] ->
            ok;
        [Org] ->
            Org1 = Org#org{time = [{Tag, os:timestamp()}|Org#org.time]},
            ok = dets:insert(all_orgs, Org1),
            Org1
    end.

mark_org(active, OrgId, WorkerPid) ->
    case dets:lookup(all_orgs, OrgId) of
        [] ->
            ok;
        [Org] ->
            Org1 = Org#org{active=true, worker=WorkerPid},
            ok = dets:insert(all_orgs, Org1),
            Org1
    end.

mark_node(complete, Id) ->
    case dets:lookup(all_nodes, Id) of
        [] -> ok;
        [Node] ->
            Node1 = Node#node{status = mysql, solr = both},
            ok = dets:insert(all_nodes, Node1),
            Node1
    end;
mark_node(solr_clean, Id) ->
    case dets:lookup(all_nodes, Id) of
        [] -> ok;
        [#node{status = mysql, solr = both} = Node] ->
            Node1 = Node#node{solr = mysql},
            ok = dets:insert(all_nodes, Node1),
            Node1
    end.

mark_node(error, Id, Why) ->
    case dets:lookup(all_nodes, Id) of
        [] -> ok;
        [Node] ->
            Node1 = Node#node{status = {error, Why}},
            ok = dets:insert(all_nodes, Node1),
            Node1
    end.

find_org_by_worker(Pid) ->
    Spec = (?wildcard_org_spec)#org{worker = Pid},
    case ?fix_table(all_orgs, dets:match_object(all_orgs, Spec)) of
        [] ->
            error_logger:error_msg("No org found for pid ~p~n", [Pid]),
            log(err, "No org found for pid ~p", [Pid]),
            not_found;
        [#org{}=Org|_Ignore] ->
            %% XXX: we should only every match one record, but we think we are encountering
            %% a race condition in how we are using dets that causes this match_object call
            %% to return two of the same record ?!.
            Org;
        {error, Why} ->
            error_logger:error_report({error, {find_org_by_worker, Pid, Why}}),
            log(err, "find_org_by_worker unexpected dets error"),
            {error, Why}
    end.
                
start_workers(Orgs, BatchSize) ->
    lists:foldl(fun({Guid, Name}, Count) ->
                        Config = make_worker_config(Guid, Name, BatchSize),
                        case mover_worker_sup:new_mover(Config) of
                            {ok, Pid} -> 
                                mover_worker:migrate(Pid),
                                monitor(process, Pid),
                                mark_org(active, Guid, Pid),
                                Count + 1;
                            _NoPid ->
                                error_logger:error_msg("unable to launch worker for ~p ~p~n", [Name, _NoPid]),
                                log(err, "unable to launch worker for '~s'", [Name]),
                                Count
                        end
                end, 0, Orgs).

make_worker_config(Guid, Name, BatchSize) ->
    [{org_name, Name}, {org_id, Guid}, {batch_size, BatchSize},
     {chef_otto, chef_otto:connect()}].

%% @doc Return a list of #org{} records corresponding to orgs that have not been
%% migrated. This includes orgs that have had an error or are being actively migrated.
%%
list_unmigrated_orgs() ->
    Spec = (?wildcard_org_spec)#org{migrated = false},
    ?fix_table(all_orgs, dets:match_object(all_orgs, Spec)).

route_orgs_to_erchef_sql() ->
    %% so we actually need to send a list of all non-migrated orgs each time.
    %% That's any org not complete
    {ok, NginxControlUrls} = application:get_env(mover, nginx_control_urls),
    Body = format_response(list_unmigrated_orgs()),
    %% dialyzer doesn't see the use of PostFun and warns that fake_post_to_nginx and
    %% post_to_nginx are unused.
    %%
    %% PostFun = case is_dry_run() of
    %%               true -> fake_post_to_nginx;
    %%               false -> post_to_nginx
    %%           end,
    Results = [ case is_dry_run() of
                    true -> fake_post_to_nginx(Url, Body);
                    false -> post_to_nginx(Url, Body)
                end
                || Url <- NginxControlUrls ],
    BadResults = [ X || X <- Results, X =/= ok ], 
    case BadResults of
        [] -> ok;
        _ -> {error, BadResults}
    end.

fake_post_to_nginx(_Url, _Body) ->
    ok.

post_to_nginx(Url, Body) ->
    Headers = [{"Content-Type", "application/json"},
               {"Accept", "application/json"}],
    IbrowseOpts = [{ssl_options, []}, {response_format, binary}],
    case ibrowse:send_req(Url, Headers, post, Body, IbrowseOpts) of
        {ok, [$2, $0|_], _H, _Body} -> ok;
        Error ->
            log(err, "post_to_nginx failed: ~256P", [Error, 100]),
            error_logger:error_msg("post_to_nginx failed: ~p~n", [Error]),
            {error, Error}
    end.

format_response(Orgs) ->
    OrgNames = [ Org#org.name || Org <- Orgs ],
    ejson:encode({[{<<"couchdb_orgs">>, OrgNames}]}).

darklaunch_read_only_nodes(OrgName, Value) when is_binary(OrgName) ->
    darklaunch_read_only_nodes([OrgName], Value);
darklaunch_read_only_nodes(OrgNames, Value) ->
    case is_dry_run() of
        true ->
            log(info, "FAKE darklaunch disabling node writes for: ~256P",
                [OrgNames, 200]),
            ok;
        false ->
            Res = [ update_darklaunch("nodes_read_only", Org, Value) || Org <- OrgNames ],
            case lists:all(fun(X) -> X =:= ok end, Res) of
                true ->
                    log(info, "darklaunch (read_only_nodes: ~s) for: ~256P",
                        [Value, OrgNames, 200]);
                false ->
                    log(err, "darklaunch (read_only_nodes: ~s) FAILED for (~256P)",
                        [Value, OrgNames, 200]),
                    throw({darklaunch_disable_node_writes_failed, OrgNames, Value})
            end
    end.

darklaunch_couchdb_nodes(OrgName, Value) when is_binary(OrgName) ->
    case is_dry_run() of
        true ->
            log(info, "FAKE darklaunch couchdb_nodes: ~s for ~s", [Value, OrgName]),
            ok;
        false ->
            case update_darklaunch("couchdb_nodes", OrgName, Value) of
                ok ->
                    log(info, "~s darklaunch couchdb_nodes: ~s", [OrgName, Value]);
                _Err ->
                    log(err, "~s darklaunch couchdb_nodes FAILED", [OrgName, Value]),
                    throw({darklaunch_couchdb_nodes_failed, OrgName, Value})
            end
    end.

update_darklaunch(Feature, Org, Value) ->
    {ok, Urls} = application:get_env(mover, darklaunch_urls),
    scatter_to_all_darklaunch(Urls, Feature, Org, Value),
    Res = [ post_to_darklaunch(Url, Feature, Org, Value) || Url <- Urls ],
    case lists:all(fun(X) -> X =:= ok end, Res) of
        true -> ok;
        false -> error
    end.

scatter_to_all_darklaunch(Urls, Feature, Org, Value) ->
    Owner = self(),
    Pids = [spawn_link(make_darklaunch_update_worker(Owner, Url, Feature, Org, Value)) || Url <- Urls],
    gather_from_all_darklaunch(length(Pids), []).

gather_from_all_darklaunch(0, Results) ->
    Results;
gather_from_all_darklaunch(Count, Results) ->
    receive
        {darklaunch_result, Result} ->
            gather_from_all_darklaunch(Count - 1, [Result|Results])
    end.

make_darklaunch_update_worker(Owner, Url, Feature, Org, Value) ->
    fun() ->
            Result = post_to_darklaunch(Url, Feature, Org, Value),
            Owner ! {darklaunch_result, Result} end.



post_to_darklaunch(Url, Feature, Org, Value) when Value =:= true;
                                                  Value =:= false ->
    Url1 = binary_to_list(iolist_to_binary([Url, "/", Feature, "/", Org])),
    Body = iolist_to_binary(["{\"enabled\":", atom_to_list(Value), "}"]),
    Headers = [{"Content-Type", "application/json"},
               {"Accept", "application/json"}],
    IbrowseOpts = [{ssl_options, []}, {response_format, binary}],
    case ibrowse:send_req(Url1, Headers, post, Body, IbrowseOpts) of
        {ok, [$2, $0|_], _H, _Body} -> ok;
        Error ->
            log(err, "post_to_darklaunch failed ~s, enable:~s, reason:~256P",
                [Url1, Value, Error, 100]),
            error_logger:error_msg("post_to_darklaunch failed ~p", [Error]),
            {error, Error}
    end.

%% Fetch darklaunch state from first darklaunch server in list. Modify the couchdb_nodes key
%% to contain all unmigrated orgs. POST this full config to all darklaunch servers.
darklaunch_couchdb_nodes_for_all() ->
    case is_dry_run() of
        true ->
            log(info, "FAKE darklaunch_couchdb_nodes_for_all"),
            ok;
        false ->
            {ok, Urls} = application:get_env(mover, darklaunch_urls),
            Darklaunch0 = get_darklaunch_state(hd(Urls)),
            OrgList = [ Org#org.name || Org <- list_unmigrated_orgs() ],
            log(info, "adding ~B orgs to darklaunch couchdb_nodes", [length(OrgList)]),
            Darklaunch = ej:set({<<"couchdb_nodes">>}, Darklaunch0, OrgList),
            Body = ejson:encode(Darklaunch),
            Res = [ post_full_darklaunch(Url, Body) || Url <- Urls ],
            case lists:all(fun(X) -> X =:= ok end, Res) of
                true -> ok;
                false -> error
            end
    end.

get_darklaunch_state(Url) ->
    Headers = [{"Accept", "application/json"}],
    IbrowseOpts = [{ssl_options, []}, {response_format, binary}],
    case ibrowse:send_req(Url, Headers, get, [], IbrowseOpts) of
        {ok, "200", _H, Body} -> ejson:decode(Body);
        Error ->
            log(err, "get_darklaunch_state failed ~s, reason:~256P",
                [Url, Error, 100]),
            error_logger:error_msg("get_darklaunch_state failed ~p", [Error]),
            {error, Error}
    end.

post_full_darklaunch(Url, Body) ->
    Headers = [{"Content-Type", "application/json"},
               {"Accept", "application/json"}],
    IbrowseOpts = [{ssl_options, []}, {response_format, binary}],
    case ibrowse:send_req(Url, Headers, post, Body, IbrowseOpts) of
        {ok, [$2, $0|_], _H, _Body} -> ok;
        Error ->
            log(err, "post_full_darklaunch failed ~s, reason:~256P",
                [Url, Error, 100]),
            error_logger:error_msg("post_full_darklaunch failed ~p", [Error]),
            {error, Error}
    end.

is_dry_run() ->
    {ok, DryRun} = application:get_env(mover, dry_run),
    DryRun.

log(Level, Msg) ->
    Id = pid_to_list(self()),
    fast_log:Level(mover_manager_log, Id, Msg).

log(Level, Fmt, Args) when is_list(Args) ->
    Id = pid_to_list(self()),
    fast_log:Level(mover_manager_log, Id, Fmt, Args).

log_node_stored(OrgName, #node{status=couchdb, id=Id, name=Name, org_id=OrgId}) ->
    Self = pid_to_list(self()),
    fast_log:info(node_errors, Self, "node authz ok: ~s ~s ~s ~s",
                  [OrgName, Name, OrgId, Id]);
log_node_stored(OrgName, #node{status={error, {missing_authz, Type}},
                               id=Id, name=Name, org_id=OrgId}=Node) ->
    dets:insert(error_nodes, Node),
    Self = pid_to_list(self()),
    fast_log:err(node_errors, Self, "missing authz data (~p): ~s ~s ~s ~s",
                 [Type, OrgName, Name, OrgId, Id]);
log_node_stored(OrgName, #node{status={error, Why}, id=Id, name=Name, org_id=OrgId}=Node) ->
    dets:insert(error_nodes, Node),
    Self = pid_to_list(self()),
    fast_log:err(node_errors, Self, "node authz fail: ~s ~s ~s ~s ~p",
                 [OrgName, Name, OrgId, Id, Why]).

safe_split(N, L) ->
    try
        lists:split(N, L)
    catch
        error:badarg ->
            {L, []}
    end.
