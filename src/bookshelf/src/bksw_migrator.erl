%% -*- erlang-indent-level: 4;indent-tabs-mode: nil; fill-column: 92 -*-
%% ex: ts=4 sw=4 et
%% @author Mark Anderson <mark@chef.io>
%% Copyright 2017 Chef, Inc. All Rights Reserved.
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

-module(bksw_migrator).

-behaviour(gen_server).

%% API
-export([start_link/0,
         update_phase/1,
         add_files/2,
         mark_file_migrated/2,
         start_migration/0,
         stats/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

%% Appease the dead code warning
-export([simple_migrator/0]).

-define(SERVER, ?MODULE).

-record(state, {
          files_to_migrate = 0:: integer,
          files_migrated = 0:: integer,

          current_phase = listing_buckets :: all_at_once | listing_buckets | listing_files | migrating_buckets | done,
          migration_worker = undefined :: undefined | pid()
         }).

%%%===================================================================
%%% API
%%%===================================================================

update_phase(Phase) ->
    gen_server:call(?MODULE, {update_phase, Phase}).

add_files(Bucket, Files) ->
    gen_server:call(?MODULE, {add_file, Bucket, Files}).

mark_file_migrated(Bucket, File) ->
    gen_server:call(?MODULE, {file_migrated, Bucket, File}).

%% For testing
start_migration() ->
    gen_server:call(?MODULE, start_migration).

stats() ->
    gen_server:call(?MODULE, stats).

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([]) ->
    InFlight = case bksw_conf:auto_start_migration() of
                   true ->
                       spawn_link(fun () -> simple_migrator() end);
                   false ->
                       undefined
               end,
    {ok, #state{current_phase = all_at_once,
                migration_worker = InFlight}}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @spec handle_call(Request, From, State) ->
%%                                   {reply, Reply, State} |
%%                                   {reply, Reply, State, Timeout} |
%%                                   {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, Reply, State} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_call({file_migrated, _Bucket, _File}, _From, #state{files_migrated = M} = State) ->
    {reply, ok, State#state{files_migrated = M+1}};
handle_call({update_phase, Phase}, _From, #state{} = State) ->
    {reply, ok, State#state{current_phase = Phase}};
handle_call(stats, _From, #state{files_migrated = M,
                                 current_phase = Phase,
                                 migration_worker = W} = State) ->
    error_logger:error_msg("bksw_migrator status ~p~n~n", [State]),
    {reply, #{files_migrated => M,
              current_phase => Phase,
              migration_worker => W
             }, State};
handle_call(start_migration, _From, #state{migration_worker = undefined} = State) ->
    InFlight = spawn_link(fun () -> simple_migrator() end ),
    {reply, ok, State#state{migration_worker = InFlight} };
handle_call(start_migration, _From, #state{migration_worker = W} = State) ->
    error_logger:error_msg("Migration worker already in flight ~p", [W]),
    {reply, ok, State };
handle_call(Request, _From, State) ->
    error_logger:error_msg("Unexpected call ~p", [Request, State]),
    Reply = ok,
    {reply, Reply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @spec handle_cast(Msg, State) -> {noreply, State} |
%%                                  {noreply, State, Timeout} |
%%                                  {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_cast(Msg, State) ->
    error_logger:error_msg("Unexpected cast ~p", [Msg]),
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_info({'EXIT', Pid, normal}, #state{migration_worker = Pid} = State) ->
    %% The migration worker has exited normally
    State1 = State#state{migration_worker = undefined},
    error_logger:info_msg("Migration unexpected worker completed has completed", []),
    {noreply, State1};
handle_info({'EXIT', Pid, Error}, #state{migration_worker = Pid} = State) ->
    %% The migration worker has died unexpectedly
    State1 = State#state{migration_worker = undefined},
    error_logger:info_msg("Migration worker unexpectedly has died", [Pid, Error]),
    {noreply, State1};
handle_info(_Info, State) ->
     error_logger:error_msg("Migratior recieved unexpected info msg", [_Info, State]),
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================


%%%%
%%%%
%%%%
simple_migrator() ->
    migrate_unless_done_already(bksw_io:is_sql_migration_complete()).

migrate_unless_done_already(true) ->
    %% Already done the migration
    error_logger:info_msg("Migrator considered already complete since I found file ~p, no need to migrate", [bksw_io:sql_migration_complete_marker_path()]),
    ok;
migrate_unless_done_already(false) ->
    error_logger:info_msg("Migrator enumerating and migrating buckets"),
    {ok, BucketsMigrated} = bksw_migrate_file:migrate_buckets(),
    error_logger:info_msg("Listing files in buckets ~p", [BucketsMigrated]),

    Files = bksw_migrate_file:list_files(BucketsMigrated),

    error_logger:info_msg("Migrating files ~p total", [length(Files)]),

    %% stuff the task execution system with the list of files to migrate
    [ bksw_executor:add_task( fun({B,F}) -> migrate_process(B, F) end, Entry) ||
        Entry <- Files ],

    error_logger:info_msg("Waiting for migration to finish"),

    bksw_executor:wait_for_idle(),

    error_logger:info_msg("Migration complete"),

    Errors = bksw_executor:error_list(),

    case Errors of
        [] ->
            error_logger:info_msg("Completed migration successfully, safe to run in sql mode"),
            bksw_io:mark_sql_migration_complete();
        _ ->
            error_logger:error_msg("Failed migration from fileysystem to sql; retrying"),
            simple_migrator()
    end.

migrate_process(Bucket, File) ->
    case bksw_migrate_file:migrate_file(Bucket, File) of
        ok ->
            ok;
        no_op ->
            ok;
        Error ->
            error_logger:error_msg("Error migrating ~p", [Error]),
            error(Error)
    end.
