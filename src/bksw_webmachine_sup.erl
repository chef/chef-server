%% -*- erlang-indent-level: 4;indent-tabs-mode: nil; fill-column: 92 -*-
%% ex: ts=4 sw=4 et
%% @author Tim Dysinger <dysinger@opscode.com>
%% Copyright 2012 Opscode, Inc. All Rights Reserved.
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

-module(bksw_webmachine_sup).

-behaviour(supervisor).

-export([start_link/0]).

-export([init/1]).

-include("internal.hrl").
%%===================================================================
%% API functions
%%===================================================================
start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%%===================================================================
%% Supervisor callbacks
%%===================================================================

init(_Args) ->
    RestartStrategy = one_for_one,
    MaxRestarts = 1000,
    MaxSecondsBetweenRestarts = 3600,

    error_logger:info_msg("bookshelf using config: ~p~n",
                          [bksw_conf:summarize_config()]),
    %% This will reconfigure the system each time we startup.
    WebConfig = bksw_conf:get_configuration(),
    WebMachine = {webmachine_mochiweb,
                  {webmachine_mochiweb, start, [WebConfig]},
                  permanent, 5000, worker, dynamic},

    {ok,
     {{RestartStrategy, MaxRestarts, MaxSecondsBetweenRestarts},
      [WebMachine]}}.
