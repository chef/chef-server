%% -*- erlang-indent-level: 4;indent-tabs-mode: nil;fill-column: 92 -*-
%% ex: ts=4 sw=4 et
%% @author Seth Falcon <seth@opscode.com>
%% @copyright 2011 Opscode, Inc.
%% @doc Mover Node Deletion Tool
%%
%% == Quick Start ==
%%
%% Edit `mover_rmrf.config'.
%%
%% Make a backup copy of all_nodes dets file
%%
%% Start Erlang: `erl -pa ebin deps/*/ebin'
%%
%% `mover_rmrf:start_link().'
%%
%% `mover_rmrf:nuke_nodes().'
%%
%%
%% == Configuration ==
%%
%% Relies on a config file `mover_rmrf.config' in the current working directory. This should
%% be a file of erlang terms suitable for file:consult like this:
%% ```
%%         {stop, false}.
%%         {min_delay, 1000}.
%%         {dry_run, true}.
%%         {couchdb_url, "http://localhost:5984"}.
%% '''
%%
%% Also expects to find an `all_nodes' dets file in current working directory. WARNING: the
%% code will modify entries in this dets file marking nodes as deleted so you might want to
%% make a backup before starting.
%%
%% You can change the delay rate by editing the config file. The `min_delay' key is in
%% milliseconds. The code checks the file before each delete. If you set `stop' to true, the
%% code will stop. You can also send the following control commands that will take effect
%% after the current org-based batch is complete:
%% ```
%% mover_rmrf:pause().
%% mover_rmrf:resume().
%% mover_rmrf:stop().
%% '''
%%
%% == Logging ==
%%
%% Activity logs are written to `mover_rmrf.log'
%% You can also check current progress with: `summarize_nodes/0'.
%%
%% == Dry Run Mode ==
%%
%% In dry_run mode, no HTTP requests are made, but the dets table is updated.
%%
%%
%% == Notes ==
%%
%% At startup, the code loads entries in the all_nodes dets table by org_id. Then it deletes
%% nodes by org enforcing a minimum delay between deletes. Each delete is two HTTP calls,
%% one to fetch the current revision and one to issue the delete. If an error occurs for
%% either call, the node is marked as a delete_error and the code continues on.
%%
-module(mover_rmrf).

-behaviour(gen_fsm).

%% API
-export([start_link/0,
         nuke_nodes/0,
         pause/0,
         resume/0,
         stop/0,
         summarize_nodes/0
        ]).

%% States
-export([init_storage/2,
         ready/2
        ]).

%% gen_fsm callbacks
-export([init/1,
         handle_event/3,
         handle_sync_event/4,
         handle_info/3,
         terminate/3,
         code_change/4]).

-record(node, {id,                              % guid for node data doc in couchdb
               name,                            % node name
               org_id,                          % guid for org
               authz_id,                        %
               requestor,                       % authz id for requesting actor
               status = couchdb,                % couchdb | mysql | {error, term()}
               solr = couchdb}).                % couchdb | both | mysql

-define(CONFIG_FILE, "mover_rmrf.config").

-define(NODE_ESTIMATE, 25000).

-define(fix_table(Tab, Expr),
        begin
            dets:safe_fixtable(Tab, true),
            try
                Expr
            after
                dets:safe_fixtable(Tab, false)
            end
        end).

-define(SERVER, ?MODULE).

-define(DETS_OPTS(EstSize), [{auto_save, 1000},
                             {keypos, 2},
                             {estimated_no_objects, EstSize}]).

-define(IBROWSE_OPTS, [{ssl_options, []}, {response_format, binary},
                       {connect_timeout, 30000}]).

-record(state, {couch_url,
                log_file,
                org_dict,
                orgs
               }).

%% @doc Start a new mover_rmrf process
%%
%% Configuration comes from `mover_rmrf.config' and the `all_nodes' dets file, both found in
%% the current working directory.
start_link() ->
    gen_fsm:start_link({local, ?SERVER}, ?MODULE, [], []).

%% @doc Start deleting nodes.
%%
%% Nodes are organized by org_id for deletion.
nuke_nodes() ->
    gen_fsm:send_event(?SERVER, timeout).

%% @doc Pause org nuking
%%
%% After the nodes for the current org are deleted, pause and wait for further instructions.
pause() ->
    gen_fsm:send_event(?SERVER, pause).

%% @doc Resume org nuking
resume() ->
    gen_fsm:send_event(?SERVER, timeout).

%% @doc Stop org nuking, shutdown.
%%
%% After the nodes for the current org are deleted, shutdown. If you want a more immediate
%% clean stop, edit the config file and set stop to true.
stop() ->
    gen_fsm:send_event(?SERVER, stop).

init([]) ->
    [ application:start(App) || App <- [crypto, public_key, ssl, ejson] ],
    ibrowse:start(),
    {ok, LogFile} = file:open("mover_rmrf.log", [append]),
    error_logger:info_msg("writing logs to ~s~n", ["mover_rmrf.log"]),
    %% ugly hack using process dictionary to get some logging
    erlang:put(log_fh, LogFile),
    CouchUrl = couchdb_url(get_config()),
    error_logger:info_msg("Will delete from couchdb at ~s~n", [CouchUrl]),
    log(info, "couchdb url: ~s", [CouchUrl]),
    {ok, init_storage, #state{couch_url = CouchUrl, log_file = LogFile}, 0}.

init_storage(timeout, State) ->
    error_logger:info_msg("dry_run is ~p~n", [is_dry_run()]),
    error_logger:info_msg("initializing node storage~n"),
    log(info, "initializing node storage"),
    {ok, _} = dets:open_file(all_nodes, ?DETS_OPTS(?NODE_ESTIMATE)),
    OrgDict = build_org_dict(),
    State1 = State#state{org_dict = OrgDict, orgs = dict:fetch_keys(OrgDict)},
    error_logger:info_msg("Loaded ~B orgs~n", [length(State1#state.orgs)]),
    error_logger:info_report(summarize_nodes()),
    log(info, "Loaded ~B orgs", [length(State1#state.orgs)]),
    error_logger:info_msg("What to play a game?~n~nHow about global thermonuclear node deletion?~n>> YES~n~n"),
    {next_state, ready, State1}.

ready(timeout, #state{orgs = []}=State) ->
    error_logger:info_msg("no orgs with nodes, stopping~n"),
    log(info, "no orgs with nodes, stopping"),
    {stop, normal, State};
ready(timeout, #state{couch_url = CouchUrl, org_dict = OrgDict,
                         orgs = [CurOrg|Orgs]}=State) ->
    Nodes = dict:fetch(CurOrg, OrgDict),
    log(info, "preparing to delete ~B nodes from ~s", [length(Nodes), CurOrg]),
    DoDelete = make_delete_fun(CouchUrl),
    try
        lists:foldl(DoDelete, now(), Nodes),
        OrgDict1 = dict:erase(CurOrg, OrgDict),
        {next_state, ready, State#state{org_dict = OrgDict1, orgs = Orgs}, 0}
    catch
        throw:stop_requested ->
            error_logger:info_msg("stop requested. I'm out."),
            log(info, "stop requested. shutting down."),
            {stop, normal, State}
    end;
ready(pause, State) ->
    log(info, "pause event received, holding steady now."),
    error_logger:info_msg("paused~n"),
    {next_state, ready, State};
ready(stop, State) ->
    log(info, "stop event received, shutting down."),
    error_logger:info_msg("goodbye~n"),
    file:close(get_log_fh()),
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

%% Internal functions

build_org_dict() ->
    OrgDict = dict:new(),
    dets:foldl(fun(#node{status = deleted}, Dict) ->
                       %% skip deleted nodes
                       Dict;
                  (#node{org_id = OrgId}=Node, Dict) ->
                       dict:append(OrgId, Node, Dict)
               end, OrgDict, all_nodes).

make_delete_fun(CouchUrl) ->
    fun(#node{status = deleted}, _PrevTime) ->
            %% already deleted, skip
            now();
       (#node{id = NodeId, org_id = OrgId}, PrevTime) ->
            case delete_node_delay(OrgId, NodeId, CouchUrl, PrevTime) of
                {ok, NewTime} ->
                    mark_node(deleted, NodeId),
                    NewTime;
                {Error, NewTime} ->
                    mark_node(delete_error, NodeId, Error),
                    NewTime
            end
    end.

mark_node(deleted, Id) ->
    case dets:lookup(all_nodes, Id) of
        [] -> ok;
        [Node] ->
            Node1 = Node#node{status = deleted},
            ok = dets:insert(all_nodes, Node1),
            Node1
    end.

mark_node(delete_error, Id, Why) ->
    case dets:lookup(all_nodes, Id) of
        [] -> ok;
        [Node] ->
            Node1 = Node#node{status = {delete_error, Why}},
            ok = dets:insert(all_nodes, Node1),
            Node1
    end.

delete_node_delay(OrgId, NodeId, CouchUrl, PrevTime) ->
    Config = get_config(),
    case should_stop(Config) of
        true -> throw(stop_requested);
        false -> ok
    end,
    Delta = ms_since(PrevTime),
    MinTime = delay_time(Config),
    SleepTime = MinTime - Delta,
    case SleepTime > 0 of
        true ->
            log(info, "Waiting for ~B ms", [SleepTime]),
            timer:sleep(SleepTime);
        false -> ok
    end,
    case revision_for_node(CouchUrl, OrgId, NodeId) of
        {ok, Rev} -> {delete_node(CouchUrl, OrgId, NodeId, Rev), now()};
        Error -> {Error, now()}
    end.

delete_node(CouchUrl,OrgId, NodeId, Rev) ->
    Bin = iolist_to_binary([CouchUrl, "/chef_", OrgId, "/", NodeId, "?rev=", Rev]),
    Url = binary_to_list(Bin),
    case is_dry_run() of
        true ->
            log(info, "FAKE DELETE ~s", [Url]),
            ok;
        false ->
            Headers = [{"Content-Type", "application/json"},
                       {"Accept", "application/json"}],
            case ibrowse:send_req(Url, Headers, delete, [], ?IBROWSE_OPTS) of
                {ok, [$2, $0|_], _H, _Body} ->
                    log(info, "OK DELETE ~s", [Url]),
                    ok;
                Error ->
                    log(err, "unable to delete ~s", [Url]),
                    Error
            end
    end.

revision_for_node(CouchUrl, OrgId, NodeId) ->
    Bin = iolist_to_binary([CouchUrl, "/chef_", OrgId, "/", NodeId, "?revs_info=true"]),
    Url = binary_to_list(Bin),
    case is_dry_run() of
        true ->
            log(info, "FAKE GET REV ~s", [Url]),
            {ok, "1234"};
        false ->
            Headers = [{"Content-Type", "application/json"},
                       {"Accept", "application/json"}],
            case ibrowse:send_req(Url, Headers, delete, [], ?IBROWSE_OPTS) of
                {ok, "200", _H, Body} ->
                    Rev = binary_to_list(ej:get({<<"_rev">>}, ejson:decode(Body))),
                    log(info, "Got rev ~s ~s", [Rev, Url]),
                    {ok, Rev};
                Error ->
                    log(err, "unable to get rev for ~s", [Url]),
                    Error
            end
    end.

get_config() ->
    {ok, Config} = file:consult(?CONFIG_FILE),
    Config.

delay_time(Config) ->
    case lists:keyfind(min_delay, 1, Config) of
        {min_delay, Delay} -> Delay;
        false -> 1000
    end.

should_stop(Config) ->
    case lists:keyfind(stop, 1, Config) of
        {stop, Bool} -> Bool;
        false -> false
    end.

couchdb_url(Config) ->
    case lists:keyfind(couchdb_url, 1, Config) of
        {couchdb_url, Url} -> Url;
        false -> error(missing_couchdb_url)
    end.

is_dry_run() ->
    case lists:keyfind(dry_run, 1, get_config()) of
        {dry_run, Bool} -> Bool;
        false -> true
    end.

log(Level, Msg) ->
    log(Level, "~s", [Msg]).

log(Level, Fmt, Args) when is_list(Args) ->
    Id = pid_to_list(self()),
    FH = get_log_fh(),
    io:fwrite(FH, "~s ~s " ++ Fmt ++ "\n", [atom_to_list(Level), Id|Args]).

get_log_fh() ->
    erlang:get(log_fh).

ms_since(When) ->
    now_ms(now()) - now_ms(When).

now_ms({MegaSecs,Secs,MicroSecs}) ->
    ((MegaSecs * 1000000 + Secs)*1000000 + MicroSecs) div 1000.

%% @doc Summarize delete progress
summarize_nodes() ->
    Counts = ?fix_table(all_nodes,
                        dets:foldl(
                          fun(Node, {NTotal, NDeleted, NToDelete, NDeleteError}) ->
                                  {NTotal + 1,
                                   NDeleted + deleted_count(Node),
                                   NToDelete + to_delete_count(Node),
                                   NDeleteError + delete_error_count(Node)}
                          end, {0, 0, 0, 0}, all_nodes)),
    Labels = [total, deleted, to_delete, delete_error],
    lists:zip(Labels, tuple_to_list(Counts)).

deleted_count(#node{status = deleted}) -> 1;
deleted_count(_) -> 0.

to_delete_count(#node{status = deleted}) -> 0;
to_delete_count(_) -> 1.

delete_error_count(#node{status = {delete_error, _}}) -> 1;
delete_error_count(_) -> 0.


