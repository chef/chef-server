%% -*- erlang-indent-level: 4;indent-tabs-mode: nil; fill-column: 92-*-
%% Copyright 2015 Chef Software, Inc.
%%
%% This file is provided to you under the Apache License,
%% Version 2.0 (the "License"); you may not use this file
%% except in compliance with the License.  You may obtain
%% a copy of the License at
%%
%%   http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing,
%% software distributed under the License is distributed on an
%% "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
%% KIND, either express or implied.  See the License for the
%% specific language governing permissions and limitations
%% under the License.
%%
-module(bksw_sync).
%%
%% bksw_sync: Asyncronous syncing of uploaded files to a remote node.
%%
%% This module provides handles asyncronous requests sent by the
%% application anytime a file is created or delete. It calls the
%% external `rsync` command to ensure that a remote host also has a
%% copy of these files.
%%
%% On startup, a full_sync message is added to our queue. This ensures
%% that any missed messages when we were down (say, because of a
%% failure) are correctly handled.
%%
%% Everytime a file is created or deleted, bksw_sync:new or
%% bksw_sync:deleted should be called with the path of the created or
%% deleted file. bksw_sync will add this path to a list of unsynced
%% paths.
%%
%% If no new paths have been added for wait_interval
%% milliseconds, we will call rsync on any unsynced paths.
%% Alternatively, if the unsynced list grows to max_pending_paths, we
%% will call rsync on the unsynced paths.
%%
%% The calls to rsync are done syncronously such that only one rsync
%% process is working at a time. Paths created or deleted during a
%% running sync will queue up in the processes mailbox for later
%% handling.
%%
%% If a full_sync or partial_sync succeeds, we clear the
%% unsynced_paths list and wait for more updates.
%%
%% If a sync fails, we do not clear the list, and try again in retry_interval
%% milliseconds.
%%
-behavior(gen_server).

-export([start_link/3,
         new/1,
         delete/1,
         sync/0,
         full_sync/0,
         status/0,
         stop/0,
         init/1,
         handle_info/2,
         handle_call/3,
         handle_cast/2,
         terminate/2,
         code_change/3
        ]).

-define(RSYNC_EXE, "rsync").
-define(RSYNC_ARGS, "-r --delete --delete-missing-args -vv").

-record(sync_state, {
          % A list of paths that have not yet been synced.
          % These paths are stored relative to the base_directory.
          unsynced_paths,
          % The base bookshelf data directory.  We initialize this to
          % bksw_conf:data_store() in init/1,
          base_directory,
          % A valid rsync remote URI.  We expect the user to supply
          % this explicitly as part of the configuration.
          rsync_remote,
          % The number of milliseconds we will wait before retrying
          % a failure
          retry_interval,
          % The number of milliseconds we will wait for more updates
          % before syncing pending updates.
          wait_interval,
          % The maximum number of paths we will collect before sending
          % a sync message.
          max_pending_paths}).

%%
%% Public API
%%
start_link(BaseDir, RemoteURI, Config) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [BaseDir, RemoteURI, Config], []).

new(Path) ->
    gen_server:cast(?MODULE, {created, Path}).

delete(Path) ->
    gen_server:cast(?MODULE, {deleted, Path}).

sync() ->
    gen_server:cast(?MODULE, sync).

full_sync() ->
    gen_server:cast(?MODULE, full_sync).

stop() ->
    gen_server:cast(?MODULE, stop).

status() ->
    gen_server:call(?MODULE, status).


%%
%% gen_server callbacks
%%
init([BaseDir, RemoteURI, Config]) ->
    %% Send ourselves a full_sync to ensure that we always do a
    %% full_sync on startup. Note: this doesn't block init() since it
    %% is a gen_server:cast underneath.
    full_sync(),

    lager:info("starting bksw_rsync for ~p ~p", [BaseDir, RemoteURI]),
    {ok, #sync_state{unsynced_paths = [],
                     retry_interval = retry_interval(Config),
                     wait_interval = wait_interval(Config),
                     max_pending_paths = max_pending_paths(Config),
                     base_directory = to_binary(BaseDir),
                     rsync_remote = RemoteURI}}.

handle_cast({created, Path}, State) ->
    add_to_unsynced(Path, State);
handle_cast({deleted, Path}, State) ->
    add_to_unsynced(Path, State);
handle_cast(full_sync, State = #sync_state{retry_interval = RetryTime}) ->
    lager:info("starting full rsync run"),
    RsyncCmd = exec:run(mk_rsync_cmd(full, State), [stdout, stderr, sync]),
    case RsyncCmd of
        {ok, _Rest} ->
            lager:info("full rsync run successful."),
            {noreply, State};
        {error, Result} ->
            lager:error("full rsync run failed, retrying in ~p ms", [RetryTime]),
            log_rsync_error(Result),
            timer:apply_after(RetryTime, ?MODULE, full_sync, []),
            {noreply, State}
    end;
handle_cast(sync, State = #sync_state{unsynced_paths = []}) ->
    lager:info("sync message recieved but no data to sync"),
    {noreply, State};
handle_cast(sync, State = #sync_state{unsynced_paths = UP,
                                      retry_interval = RetryTime}) ->
    lager:info("starting partial sync on ~p paths", [length(UP)]),
    {ok, File, Filename} = tempfile(),
    [file:write(File, iolist_to_binary([P, $\n])) || P <- UP ],
    file:close(File),
    Cmd = mk_rsync_cmd(partial, State, Filename),
    CmdStatus = exec:run(Cmd, [stdout, stderr, sync]),
    file:delete(Filename),
    case CmdStatus of
        {ok, _Rest} ->
            lager:info("partial rsync run successful."),
            {noreply, State#sync_state{unsynced_paths = []}};
        {error, Result} ->
            lager:error("partial rsync run failed, retrying in ~p ms", [RetryTime]),
            log_rsync_error(Result),
            timer:apply_after(RetryTime, ?MODULE, sync, []),
            {noreply, State}
    end;
handle_cast(stop, State) ->
    {stop, normal, State}.

handle_call(status, _From, State = #sync_state{wait_interval = WaitTime}) ->
    % Use a timeout here to prevent status calls from stranding
    % unsynced paths for longer than expected.
    {reply, State, State, WaitTime}.

handle_info(timeout, State = #sync_state{unsynced_paths = UP,
                                         wait_interval = WaitTime}) ->
    lager:debug("No updates for ~p ms", [WaitTime]),
    case UP of
        [] ->
            lager:debug("Up to date. Nothing to do."),
            {noreply, State};
        _Paths ->
            sync(),
            {noreply, State}
    end.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

terminate(_Reason, _State) ->
    ok.

%%
%% Internal Functions
%%
log_rsync_error(Result) when is_list(Result) ->
    log_if_exists("rsync stdout", stdout, Result),
    log_if_exists("rsync stderr", stderr, Result);
log_rsync_error(Result) ->
    lager:error("Unkown response type from exec:run: ~p", [Result]).

log_if_exists(Prefix, Key, Data) ->
    case proplists:get_value(Key, Data) of
        undefined ->
            lager:error("~p: <NONE>", [Prefix]);
        Other ->
            lager:error("~p: ~p", [Prefix, Other])
    end.

%%
%% Add a newly recieved paths to our list of unsynced_paths.  If we
%% have reached the max allowed unsynced paths, put a sync message on
%% the queue.
%%
%% Set a timeout in our gen_server response to handle any unsynced
%% paths if we never hit the max.
%%
add_to_unsynced(Element, State = #sync_state{unsynced_paths = UpdateSet,
                                             base_directory = BaseDir,
                                             max_pending_paths = Limit,
                                             wait_interval = WaitTime}) ->
    NewSet = [relative(Element, BaseDir) | UpdateSet],
    State1 = State#sync_state{unsynced_paths = NewSet},
    case length(NewSet) of
        N when N > Limit ->
            lager:info("Maximum unsynced paths reached. Sending sync message."),
            sync(),
            {noreply, State1, WaitTime};
        _ ->
            {noreply, State1, WaitTime}
    end.

to_binary(Path) when is_list(Path) ->
    list_to_binary(Path);
to_binary(Path) when is_binary(Path) ->
    Path.

relative(Path, BaseDir) ->
    Size = byte_size(BaseDir),
    case Path of
        <<BaseDir:Size/binary, Rest/binary>> ->
            Rest;
        _Other ->
            throw(not_child_of_base_dir)
    end.

mk_rsync_cmd(full, #sync_state{base_directory = Dir,
                               rsync_remote = Rem}) ->
    binary_to_list(iolist_to_binary([?RSYNC_EXE, " ", ?RSYNC_ARGS,
                                      " ", Dir, " ", Rem])).

mk_rsync_cmd(partial, #sync_state{base_directory = Dir,
                               rsync_remote = Rem}, TempFile) ->
    binary_to_list(iolist_to_binary([?RSYNC_EXE, " ", ?RSYNC_ARGS,
                                     " --files-from ", TempFile, " ", Dir, " ", Rem])).

tempfile() ->
    Filename = bksw_io_names:write_path("bookshelf-tmp", "inc-sync"),
    {ok, FD} = file:open(Filename, [write, exclusive]),
    {ok, FD, Filename}.


%%
%% Config Helpers
%%
retry_interval(Config) ->
    proplists:get_value(retry_interval, Config, 2000).

wait_interval(Config) ->
    proplists:get_value(wait_interval, Config, 2000).

max_pending_paths(Config) ->
    proplists:get_value(max_pending_paths, Config, 250).
