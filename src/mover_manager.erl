%% -*- erlang-indent-level: 4;indent-tabs-mode: nil;fill-column: 92 -*-
%% ex: ts=4 sw=4 et
%% @author Kevin Smith <kevin@opscode.com>
%% @copyright 2011 Opscode, Inc.
-module(mover_manager).

-behaviour(gen_fsm).

%% API
-export([start_link/1]).

%% States
-export([init_storage/2,
         load_orgs/2,
         preload_org_nodes/2,
         ready/3,
         running/3]).

%% gen_fsm callbacks
-export([init/1,
         handle_event/3,
         handle_sync_event/4,
         handle_info/3,
         terminate/3,
         code_change/4]).

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
                workers=0}).

-record(org, {guid,
              name,
              preloaded=false,
              read_only=false,
              active=false,
              complete=false,
              worker}).

-record(node, {name,
               id,
               authz_id,
               requestor,
               error}).

-include("node_mover.hrl").

start_link(PreloadAmt) ->
    gen_fsm:start_link({local, ?SERVER}, ?MODULE, [PreloadAmt], []).

init([PreloadAmt]) ->
    {ok, init_storage, #state{preload_amt=PreloadAmt}, 0}.

init_storage(timeout, State) ->
    {ok, _} = dets:open_file(all_orgs, ?DETS_OPTS(?ORG_ESTIMATE)),
    {ok, _} = dets:open_file(all_nodes, ?DETS_OPTS(?NODE_ESTIMATE)),
    {ok, _} = dets:open_file(error_nodes, ?DETS_OPTS(10000)),
    {next_state, load_orgs, State, 0}.

load_orgs(timeout, State) ->
    Cn = chef_otto:connect(),
    [insert_org(NameGuid) || NameGuid <- chef_otto:fetch_orgs(Cn)],
    {next_state, preload_org_nodes, State#state{couch_cn=Cn}, 0}.

preload_org_nodes(timeout, #state{preload_amt=Amt}=State) ->
    case preload_orgs(Amt, State) of
        {ok, State1} ->
            {next_state, ready, State1};
        Error ->
            {stop, Error, State}
    end.

%% find_migration_candidates
%% mark_candidates_as_read_only
%% 
ready(status, _From, State) ->
    {reply, {ok, 0}, ready, State};
ready({start, BatchSize, NodeBatchSize}, _From,
                 #state{workers=0}=State) ->
    {Reply, NextState, State1} = case find_migration_candidates(BatchSize) of
                                     {ok, Orgs} ->
                                         %% Tell darklaunch to put these orgs into read-only
                                         %% mode for nodes.
                                         ok = darklaunch_disable_node_writes([ Name || {_, Name} <- Orgs ]),
                                         [ mark_org(read_only, OrgId) || {OrgId, _} <- Orgs ],
                                         case start_workers(Orgs, NodeBatchSize) of
                                             0 ->
                                                 {{error, none_started}, ready, State};
                                             Count ->
                                                 {{ok, Count}, running, State#state{workers=Count}}
                                         end;
                                     Error ->
                                         {Error, ready, State}
                                 end,
    {reply, Reply, NextState, State1}.

running(status, _From, #state{workers=Workers}=State) ->
    {reply, {ok, Workers}, running, State};
running({start, _BatchSize, _NodeBatchSize}, _From, State) ->
    {reply, {error, running_batch}, running, State}.

handle_event(_Event, StateName, State) ->
    {next_state, StateName, State}.

handle_sync_event(_Event, _From, StateName, State) ->
    Reply = ok,
    {reply, Reply, StateName, State}.

handle_info({'DOWN', _MRef, process, Pid, normal}, StateName,
            #state{workers = Workers}=State) when Workers > 0 ->
    %% TODO: Preload another org to keep pipe full?
    case find_org_by_worker(Pid) of
        #org{}=Org ->
            mark_org(complete, Org#org.guid),
            WorkerCount = Workers - 1,
            NextState = case WorkerCount > 0 of
                            true -> StateName;
                            false -> ready
                        end,
            %% the org is now marked as complete and we regenerate the list of unmigrated
            %% orgs and send updates to our nginx lbs.  Marking the orgs as not_read_only is
            %% only for accounting so that we can find orgs that are migrated, but not
            %% turned on in nginx later.
            case route_orgs_to_erchef_sql() of
                ok -> mark_org(not_read_only, Org#org.guid);
                _Ignore -> ok
            end,
            {next_state, NextState, State#state{workers = WorkerCount}};
        _NotFound ->
            %% ignore the msg
            {next_state, StateName, State}
    end;
%% FIXME: when worker terminates w/ nodes_failed, is this what we get?
handle_info({'DOWN', _MRef, process, Pid, nodes_failed}, StateName,
            #state{workers = Workers}=State) when Workers > 0 ->
    case find_org_by_worker(Pid) of
        #org{}=Org ->
            mark_org(nodes_failed, Org#org.guid),
            WorkerCount = Workers - 1,
            NextState = case WorkerCount > 0 of
                            true -> StateName;
                            false -> ready
                        end,
            %% this org has failed nodes.  To minimize downtime for this org, we will fail
            %% the migration and turn on writes back in couch-land.
            darklaunch_enable_node_writes(Org#org.name),
            {next_state, NextState, State#state{workers = WorkerCount}};
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
    case dets:lookup(all_orgs, Name) of
        [] ->
            Org = #org{guid=Guid, name=Name},
            dets:insert(all_orgs, Org);
        [#org{complete=true}] ->
            ok;
        [Org] ->
            dets:insert(all_orgs, Org#org{preloaded=false, complete=false, active=false})
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
        {[], _Cont} ->
            {ok, none};
        {Data, _Cont} ->
            Orgs = [{Guid, Name} || [Guid, Name] <- Data],
            {ok, Orgs};
        Error ->
            Error
    end.

find_migration_candidates(BatchSize) ->
    {Preloaded, Active, Complete} = {true, false, false},
    case dets:match(all_orgs, ?ORG_SPEC(Preloaded, Active, Complete), BatchSize) of
        {[], _Cont} ->
            {ok, none};
        {Data, _Cont} ->
            Orgs = [{Guid, Name} || [Guid, Name] <- Data],
            {ok, Orgs};
        Error ->
            Error
    end.

store_node(Cn, OrgId, NodeId, NodeName) ->
    case chef_otto:fetch_by_name(Cn, OrgId, NodeName, authz_node) of
        {ok, MixlibNode} ->
            MixlibId = ej:get({<<"_id">>}, MixlibNode),
            AuthzId = chef_otto:fetch_auth_join_id(Cn, MixlibId, user_to_auth),
            RequestorId = ej:get({<<"requester_id">>}, MixlibNode),
            Node = #node{name=NodeName, id=NodeId,
                         authz_id=AuthzId, requestor=RequestorId},
            dets:insert(all_nodes, Node);
        Error ->
            dets:insert(error_nodes, #node{name=NodeName, id=NodeId, error=Error})
    end.

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
            Org1 = Org#org{complete=true, worker=undefined},
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

find_org_by_worker(Pid) ->
    Spec = (wildcard_org_spec())#org{worker = Pid},
    case dets:match_object(all_orgs, Spec) of
        [] ->
            error_logger:error_msg("No org found for pid ~p~n", [Pid]),
            not_found;
        [#org{}=Org] ->
            Org;
        {error, Why} ->
            error_logger:error_report({error, {find_org_by_worker, Pid, Why}}),
            {error, Why}
    end.
                
start_workers(Orgs, BatchSize) ->
    lists:foldl(fun({Guid, Name}, Count) ->
                        Config = make_worker_config(Guid, Name, BatchSize),
                        case node_mover_sup:new_mover(Config) of
                            {ok, Pid} -> 
                                monitor(process, Pid),
                                mark_org(active, Guid, Pid),
                                Count + 1;
                            _NoPid -> Count
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
    Results = [ post_to_nginx(Url, Body) || Url <- NginxControlUrls ],
    BadResults = [ X || X <- Results, X =/= ok ], 
    case BadResults of
        [] ->
            ok;
        _ ->
            {error, BadResults}
    end.

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


darklaunch_enable_node_writes(OrgNames) ->
    error_logger:info_msg("enabling node writes for ~p via darklaunch~n", [OrgNames]).

darklaunch_disable_node_writes(OrgNames) ->
    error_logger:info_msg("disabling node writes for ~p via darklaunch~n", [OrgNames]).

wildcard_org_spec() ->
    #org{guid = '_',
         name = '_',
         preloaded = '_',
         read_only = '_',
         active = '_',
         complete = '_',
         worker = '_'}.
