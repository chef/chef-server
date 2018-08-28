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
%%%-------------------------------------------------------------------
%%% @author Oliver Ferrigni <oliver@chef.io>
%%% @doc
%%%
%%% @end
%%% Created :  5 Jun 2013 by Oliver Ferrigni <oliver@chef.io>
%%%-------------------------------------------------------------------
-module(depsolver_worker_sup).

-behaviour(supervisor).

%% API
-export([
         start_link/0,
         solve/3
        ]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%%===================================================================
%%% API functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the supervisor
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

solve(DepGraph, Goals, Timeout) ->
    {ok, Pid} = supervisor:start_child(?SERVER, []),
    Result = try
                 depsolver_worker:solve(Pid, DepGraph, Goals, Timeout)
             catch
                 exit:{timeout, _} ->
                     {error, resolution_timeout}
             after
                 supervisor:terminate_child(?SERVER, Pid),
                 supervisor:delete_child(?SERVER, Pid)
             end,
    depsolver_event_logger:log(DepGraph, Goals, Result),
    Result.

%%%===================================================================
%%% Supervisor callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Whenever a supervisor is started using supervisor:start_link/[2,3],
%% this function is called by the new process to find out about
%% restart strategy, maximum restart frequency and child
%% specifications.
%%
%% @spec init(Args) -> {ok, {SupFlags, [ChildSpec]}} |
%%                     ignore |
%%                     {error, Reason}
%% @end
%%--------------------------------------------------------------------
init([]) ->
    RestartStrategy = simple_one_for_one,
    MaxRestarts = 1000,
    MaxSecondsBetweenRestarts = 3600,

    SupFlags = {RestartStrategy, MaxRestarts, MaxSecondsBetweenRestarts},

    Restart = permanent,
    Shutdown = 2000,
    Type = worker,

    AChild = {depsolver_worker, {depsolver_worker, start_link, []},
              Restart, Shutdown, Type, [depsolver_worker]},

    {ok, {SupFlags, [AChild]}}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
