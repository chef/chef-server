%% -*- erlang-indent-level: 4; indent-tabs-mode: nil; fill-column: 80 -*-
%% ex: ts=4 sx=4 et
%%
%% Copyright 2013-2018 Chef Software, Inc.
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
-module(depsolver_sup).

-behaviour(supervisor).

-export([
         init/1,
         start_link/0
        ]).

-define(SERVER, ?MODULE).

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

init([]) ->
    RestartStrategy = one_for_one,
    MaxRestarts = 1000,
    MaxSecondsBetweenRestarts = 3600,

    SupFlags = {RestartStrategy, MaxRestarts, MaxSecondsBetweenRestarts},

    Restart = permanent,
    Shutdown = 2000,
    Type = worker,

    WorkerSup = {depsolver_worker_sup, {depsolver_worker_sup, start_link, []},
                 Restart, Shutdown, supervisor, [depsolver_worker_sup]},

    EventLog = {depsolver_event_logger, {depsolver_event_logger, start_link, []},
                Restart, Shutdown, Type, [depsolver_event_logger]},

    {ok, {SupFlags, [EventLog, WorkerSup]}}.
