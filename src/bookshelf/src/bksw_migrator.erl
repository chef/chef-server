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

-behaviour(supervisor).

-export([start_link/0]).

-export([init/1]).

%%===================================================================
%% API functions
%%===================================================================

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%%===================================================================
%% Supervisor callbacks
%%===================================================================

init(_Args) ->
    prepare_storage_type(bksw_conf:storage_type()),

    RestartStrategy = one_for_one,
    MaxRestarts = 1000,
    MaxSecondsBetweenRestarts = 3600,
    SupFlags = {RestartStrategy, MaxRestarts, MaxSecondsBetweenRestarts},

    WebmachineSup = {bksw_webmachine_sup, {bksw_webmachine_sup, start_link, []},
                     permanent, infinity, supervisor, [bksw_webmachine_sup]},
    {ok, {SupFlags, maybe_with_cleanup_task([WebmachineSup])}}.


maybe_with_cleanup_task(ChildSpecs) ->
    CleanupTask = {bksw_cleanup_task, {bksw_cleanup_task, start_link, []},
                   permanent, brutal_kill, worker, [bksw_cleanup_task]},
    case bksw_conf:storage_type() of
        sql ->
            [CleanupTask| ChildSpecs];
        _ ->
            ChildSpecs
    end.

prepare_storage_type(filesystem) ->
    bksw_io:ensure_disk_store(),
    bksw_io:upgrade_disk_format();
prepare_storage_type(sql) ->
    ensure_default_bucket().

ensure_default_bucket() ->
    DefaultBucket = <<"bookshelf">>,
    case bksw_sql:bucket_exists(DefaultBucket) of
        true ->
            lager:info("Default bucket ~p already exists.", [DefaultBucket]),
            ok;
        false ->
            lager:info("Create default bucket ~p.", [DefaultBucket]),
            bksw_sql:create_bucket(DefaultBucket)
    end.
