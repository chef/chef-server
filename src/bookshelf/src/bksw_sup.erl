%% -*- erlang-indent-level: 4;indent-tabs-mode: nil; fill-column: 92 -*-
%% ex: ts=4 sw=4 et
%% @author Mark Anderson <mark@chef.io>
%% @author Tim Dysinger <dysinger@opscode.com>
%% Copyright 2012-16 Chef, Inc. All Rights Reserved.
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

-module(bksw_sup).

-behaviour(supervisor).

-export([start_link/0, reconfigure_server/0]).

-export([init/1]).

%%===================================================================
%% API functions
%%===================================================================

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

reconfigure_server() ->
    supervisor:restart_child(?MODULE, bksw_webmachine_sup).

%%===================================================================
%% Supervisor callbacks
%%===================================================================

init(_Args) ->
    bksw_io:ensure_disk_store(),
    bksw_io:upgrade_disk_format(),
    RestartStrategy = one_for_one,
    MaxRestarts = 1000,
    MaxSecondsBetweenRestarts = 3600,

    SupFlags = {RestartStrategy, MaxRestarts, MaxSecondsBetweenRestarts},

    Restart = permanent,

    WebmachineSup = {bksw_webmachine_sup, {bksw_webmachine_sup, start_link, []},
                     Restart, infinity, supervisor, [bksw_webmachine_sup]},
    CleanupTask = {bksw_cleanup_task, {bksw_cleanup_task, start_link, []},
                   permanent, brutal_kill, worker, [bksw_cleanup_task]},

    {ok, {SupFlags, [ WebmachineSup, CleanupTask ]}}.
