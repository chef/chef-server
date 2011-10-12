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
-define(ORG_SPEC(Preloaded, Active, Complete), {org, '$1', '$2', Preloaded, Active, Complete}).

-record(state, {couch_cn,
                preload_amt,
                workers=0}).

-record(org, {guid,
              name,
              preloaded=false,
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
    Cn = couch_connect(),
    [insert_org(NameGuid) || NameGuid <- chef_otto:fetch_orgs(Cn)],
    {next_state, preload_org_nodes, State#state{couch_cn=Cn}, 0}.

preload_org_nodes(timeout, #state{preload_amt=Amt}=State) ->
    case preload_orgs(Amt, State) of
        {ok, State1} ->
            {next_state, ready, State1};
        Error ->
            {stop, Error, State}
    end.

ready(status, _From, State) ->
    {reply, {ok, 0}, ready, State};
ready({start, BatchSize, NodeBatchSize}, _From,
                 #state{workers=0}=State) ->
    {Reply, NextState, State1} = case find_migration_candidates(BatchSize) of
                                     {ok, Orgs} ->
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

handle_info({'EXIT', _Worker, _Reason}, StateName, State) ->
    %% Update DETS table
    %% Preload another org to keep pipe full
    {next_state, StateName, State};
handle_info(_Info, StateName, State) ->
    {next_state, StateName, State}.

terminate(_Reason, _StateName, _State) ->
    ok.

code_change(_OldVsn, StateName, State, _Extra) ->
    {ok, StateName, State}.

%% Internal functions
couch_connect() ->
    {ok, Host} = application:get_env(chef_common, couchdb_host),
    {ok, Port} = application:get_env(chef_common, couchdb_port),
    chef_otto:connect(Host, Port).

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
    case dets:match(all_orgs, ?ORG_SPEC(false, false, false), BatchSize) of
        {[], _Cont} ->
            {ok, none};
        {Data, _Cont} ->
            Orgs = [list_to_tuple(Org) || Org <- Data],
            {ok, Orgs};
        Error ->
            Error
    end.

find_migration_candidates(BatchSize) ->
    case dets:match(all_orgs, ?ORG_SPEC(true, false, false), BatchSize) of
        {[], _Cont} ->
            {ok, none};
        {Data, _Cont} ->
            Orgs = [list_to_tuple(Org) || Org <- Data],
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
    end.
%% Commented out to make dialyzer happy
%% mark_org(active, OrgId, WorkerPid) ->
%%     case dets:lookup(all_orgs, OrgId) of
%%         [] ->
%%             ok;
%%         [Org] ->
%%             Org1 = Org#org{active=true, worker=WorkerPid},
%%             dets:insert(all_orgs, Org1)
%%     end.

start_workers(_Orgs, _BatchSize) ->
    %% Use node_mover_sup to start up workers
    %% Monitor each pid as it's returned
    %% Update corresponding DETS entry for org
    %% to track PID and set active flag.
    random:uniform(5) - 1.
