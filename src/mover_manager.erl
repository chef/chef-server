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
         store_node/4
         ]).

%% States
-export([init_storage/2,
         load_orgs/2,
         preload_org_nodes/2,
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

-include("node_mover.hrl").

-define(SERVER, ?MODULE).
-define(DETS_OPTS(EstSize), [{auto_save, 1000},
                             {keypos, 2},
                             {estimated_no_objects, EstSize}]).
-define(ORG_SPEC(Preloaded, Active, Complete),
        #org{guid = '$1',
             name = '$2',
             preloaded = Preloaded,
             read_only = '_',
             active = Active,
             complete = Complete,
             worker = '_'}).

-record(state, {couch_cn,
                preload_amt,
                workers = 0,
                batches = 0,
                orgs_per_batch = 10,
                node_batch_size = 20}).

start_link(PreloadAmt) ->
    gen_fsm:start_link({local, ?SERVER}, ?MODULE, [PreloadAmt], []).

status() ->
    gen_fsm:sync_send_all_state_event(?SERVER, status, 10000).

get_going(NumBatches, OrgsPerBatch, NodeBatchSize) ->
    gen_fsm:sync_send_event(?SERVER, {start, NumBatches, OrgsPerBatch, NodeBatchSize}).

init([PreloadAmt]) ->
    {ok, init_storage, #state{preload_amt=PreloadAmt}, 0}.

init_storage(timeout, State) ->
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
    Summary = summarize_orgs(),
    error_logger:info_msg("loaded orgs: ~p~n", [Summary]),
    log(info, "~256P", [Summary, 10]),
    {next_state, preload_org_nodes, State#state{couch_cn=Cn}, 0}.

preload_org_nodes(timeout, #state{preload_amt=Amt}=State) ->
    error_logger:info_msg("preloading nodes for ~B orgs~n", [Amt]),
    log(info, "preloading nodes for ~B orgs", [Amt]),
    case preload_orgs(Amt, State) of
        {ok, State1} ->
            error_logger:info_msg("preloading complete~n"),
            log(info, "preloading complete"),
            {next_state, ready, State1};
        Error ->
            {stop, Error, State}
    end.

%% find_migration_candidates
%% mark_candidates_as_read_only
%% 
ready({start, NumBatches, OrgsPerBatch, NodeBatchSize}, _From,
      #state{workers = 0, batches = 0}=State) ->
    State1 = State#state{batches = NumBatches,
                         orgs_per_batch = OrgsPerBatch,
                         node_batch_size = NodeBatchSize},
    {reply, {ok, moveit}, start_batch, State1, 0}.

start_batch(timeout, #state{workers = 0,
                            batches = Batches,
                            orgs_per_batch = NumOrgs,
                            node_batch_size = NodeBatchSize}=State) ->
    case find_migration_candidates(NumOrgs) of
        {ok, none} ->
            error_logger:info_msg("no migration candidates~n"),
            log(info, "no migration candidates"),
            {next_state, ready, State#state{batches = 0}};
        {ok, Orgs} ->
            %% Tell darklaunch to put these orgs into read-only
            %% mode for nodes.
            ok = darklaunch_disable_node_writes([ Name || {_, Name} <- Orgs ]),
            [ mark_org(read_only, OrgId) || {OrgId, _} <- Orgs ],
            case start_workers(Orgs, NodeBatchSize) of
                0 ->
                    error_logger:error_msg("unable to start workers~n"),
                    log(err, "unable to start workers for: ~256P", [[ Name || {_, Name} <- Orgs ], 10]),
                    {next_state, ready, State};
                Count ->
                    BatchesLeft = Batches - 1,
                    log(info, "~B workers ok, ~B batches to go", [Count, BatchesLeft]),
                    {next_state, running, State#state{workers=Count, batches = BatchesLeft}}
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
            mark_org(complete, Org#org.guid),
            %% the org is now marked as complete and we regenerate the list of unmigrated
            %% orgs and send updates to our nginx lbs.  Marking the orgs as not_read_only is
            %% only for accounting so that we can find orgs that are migrated, but not
            %% turned on in nginx later.
            case route_orgs_to_erchef_sql() of
                ok -> mark_org(not_read_only, Org#org.guid);
                _Ignore -> ok
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
            darklaunch_enable_node_writes(Org#org.name),
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
        [#org{}] ->
            ok
        %% Hey Kevin, why change state of orgs already loaded?
        %% [Org] ->
        %%     dets:insert(all_orgs, Org#org{preloaded=false, complete=false, active=false})
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
load_org_nodes([{OrgId, _Name}|T], #state{couch_cn=Cn}=State) ->
    NodeList = chef_otto:fetch_nodes_with_ids(Cn, OrgId),
    [store_node(Cn, OrgId, NodeId, NodeName) || {NodeName, NodeId} <- NodeList],
    mark_org(preload, OrgId),
    load_org_nodes(T, State).

find_preload_candidates(BatchSize) ->
    {Preloaded, Active, Complete} = {false, false, false},
    case dets:match(all_orgs, ?ORG_SPEC(Preloaded, Active, Complete), BatchSize) of
        {error, Why} ->
            {error, Why};
        '$end_of_table' ->
            {ok, none};
        {[], _Cont} ->
            {ok, none};
        {Data, _Cont} ->
            Orgs = [{Guid, Name} || [Guid, Name] <- Data],
            {ok, Orgs}
    end.

find_migration_candidates(BatchSize) ->
    {Preloaded, Active, Complete} = {true, false, false},
    case dets:match(all_orgs, ?ORG_SPEC(Preloaded, Active, Complete), BatchSize) of
        {error, Why} ->
            {error, Why};
        '$end_of_table' ->
            {ok, none};
        {[], _Cont} ->
            {ok, none};
        {Data, _Cont} ->
            Orgs = [{Guid, Name} || [Guid, Name] <- Data],
            {ok, Orgs}
    end.

store_node(Cn, OrgId, NodeId, NodeName) ->
    case chef_otto:fetch_by_name(Cn, OrgId, NodeName, authz_node) of
        {ok, MixlibNode} ->
            MixlibId = ej:get({<<"_id">>}, MixlibNode),
            %% Note that this can return a {not_found, _} tuple so we use status_for_ids to
            %% validate that we have binaries and otherwise mark node as an error.
            AuthzId = chef_otto:fetch_auth_join_id(Cn, MixlibId, user_to_auth),
            RequestorId = ej:get({<<"requester_id">>}, MixlibNode),
            Node = #node{id=NodeId,
                         name=NodeName,
                         org_id = OrgId,
                         authz_id=AuthzId,
                         requestor=RequestorId,
                         status = status_for_ids(AuthzId, RequestorId)},
            dets:insert(all_nodes, Node),
            Node;
        Error ->
            Node = #node{id=NodeId,
                         name=NodeName,
                         org_id = OrgId,
                         status={error, Error}},
            dets:insert(error_nodes, Node),
            Node
    end.

status_for_ids(AuthzId, RequestorId) when is_binary(AuthzId),
                                          is_binary(RequestorId) ->
    couchdb;
status_for_ids(AuthzId, RequestorId) ->
    {error, [{authz_id, AuthzId}, {requestor, RequestorId}]}.

mark_org(preload, OrgId) ->
    case dets:lookup(all_orgs, OrgId) of
        [] ->
            ok;
        [Org] ->
            Org1 = Org#org{preloaded=true},
            dets:insert(all_orgs, Org1)
    end;
mark_org(read_only, OrgId) ->
    case dets:lookup(all_orgs, OrgId) of
        [] ->
            ok;
        [Org] ->
            Org1 = Org#org{read_only=true},
            dets:insert(all_orgs, Org1)
    end;
mark_org(not_read_only, OrgId) ->
    case dets:lookup(all_orgs, OrgId) of
        [] ->
            ok;
        [Org] ->
            Org1 = Org#org{read_only=false},
            dets:insert(all_orgs, Org1)
    end;
mark_org(complete, OrgId) ->
    case dets:lookup(all_orgs, OrgId) of
        [] ->
            ok;
        [Org] ->
            Org1 = Org#org{complete=true, active=false, worker=undefined},
            dets:insert(all_orgs, Org1)
    end;
mark_org(nodes_failed, OrgId) ->
    case dets:lookup(all_orgs, OrgId) of
        [] ->
            ok;
        [Org] ->
            Org1 = Org#org{complete=nodes_failed, worker=undefined},
            dets:insert(all_orgs, Org1)
    end.

mark_org(active, OrgId, WorkerPid) ->
    case dets:lookup(all_orgs, OrgId) of
        [] ->
            ok;
        [Org] ->
            Org1 = Org#org{active=true, worker=WorkerPid},
            dets:insert(all_orgs, Org1)
    end.

mark_node(complete, Id) ->
    case dets:lookup(all_nodes, Id) of
        [] -> ok;
        [Node] -> dets:insert(all_nodes, Node#node{status = mysql, solr = both})
    end;
mark_node(solr_clean, Id) ->
    case dets:lookup(all_nodes, Id) of
        [] -> ok;
        [#node{status = mysql, solr = both} = Node] ->
            dets:insert(all_nodes, Node#node{solr = mysql})
    end.

mark_node(error, Id, Why) ->
    case dets:lookup(all_nodes, Id) of
        [] -> ok;
        [Node] -> dets:insert(all_nodes, Node#node{status = {error, Why}})
    end.

find_org_by_worker(Pid) ->
    Spec = (wildcard_org_spec())#org{worker = Pid},
    case dets:match_object(all_orgs, Spec) of
        [] ->
            error_logger:error_msg("No org found for pid ~p~n", [Pid]),
            log(err, "No org found for pid ~p", [Pid]),
            not_found;
        [#org{}=Org] ->
            Org;
        {error, Why} ->
            error_logger:error_report({error, {find_org_by_worker, Pid, Why}}),
            log(err, "find_org_by_worker unexpected dets error"),
            {error, Why}
    end.
                
start_workers(Orgs, BatchSize) ->
    lists:foldl(fun({Guid, Name}, Count) ->
                        Config = make_worker_config(Guid, Name, BatchSize),
                        case node_mover_sup:new_mover(Config) of
                            {ok, Pid} -> 
                                node_mover_worker:migrate(Pid),
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

list_unmigrated_orgs() ->
    Spec = (wildcard_org_spec())#org{complete = true, worker = undefined},
    dets:match_object(all_orgs, Spec).

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
    %% error_logger:info_msg("fake POST of data to nginx at ~s~n~p~n", [Url, Body]),
    ok.

post_to_nginx(Url, Body) ->
    Headers = [{"content-type", "application/json"}],
    IbrowseOpts = [{ssl_options, []}, {response_format, binary}],
    case ibrowse:send_req(Url, Headers, post, Body, IbrowseOpts) of
        {ok, [$2, $0|_], _H, _Body} -> ok;
        Error -> {error, Error}
    end.

format_response(Orgs) ->
    OrgNames = [ Org#org.name || Org <- Orgs ],
    ejson:encode({[{<<"couch-orgs">>, OrgNames}]}).


darklaunch_enable_node_writes(OrgName) when is_binary(OrgName) ->
    darklaunch_enable_node_writes([OrgName]);
darklaunch_enable_node_writes(OrgNames) ->
    case is_dry_run() of
        true ->
            log(info, "FAKE enable node writes for ~B org via darklaunch",
                [length(OrgNames)]),
            ok;
        false ->
            error(implement_me)
    end.

darklaunch_disable_node_writes(OrgNames) ->
    case is_dry_run() of
        true ->
            log(info, "FAKE darklaunch disabling node writes for: ~256P", [OrgNames, 10]),
            ok;
        false ->
            error(implement_me)
    end.

is_dry_run() ->
    {ok, DryRun} = application:get_env(mover, dry_run),
    DryRun.

wildcard_org_spec() ->
    #org{guid = '_',
         name = '_',
         preloaded = '_',
         read_only = '_',
         active = '_',
         complete = '_',
         worker = '_'}.

summarize_orgs() ->
    Counts = dets:foldl(fun(Org, {NTotal, NPreloaded, NReadOnly,
                                  NActive, NComplete, NError}) ->
                                {NTotal + 1,
                                 NPreloaded + preloaded_count(Org),
                                 NReadOnly + as_number(Org#org.read_only),
                                 NActive + as_number(Org#org.active),
                                 NComplete + as_number(Org#org.complete),
                                 NError + error_count(Org)}
                        end, {0, 0, 0, 0, 0, 0}, all_orgs),
    Labels = [total, preloaded, read_only, active, complete, error],
    lists:zip(Labels, tuple_to_list(Counts)).

preloaded_count(#org{preloaded=true, complete = false}) ->
    1;
preloaded_count(#org{}) ->
    0.

error_count(#org{complete = nodes_failed}) ->
    1;
error_count(#org{}) ->
    0.

as_number(true) ->
    1;
as_number(_) ->
    0.

log(Level, Msg) ->
    Id = pid_to_list(self()),
    fast_log:Level(mover_manager_log, Id, Msg).

log(Level, Fmt, Args) when is_list(Args) ->
    Id = pid_to_list(self()),
    fast_log:Level(mover_manager_log, Id, Fmt, Args).
