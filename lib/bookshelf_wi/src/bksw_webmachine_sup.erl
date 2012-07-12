 %% -*- erlang-indent-level: 4;indent-tabs-mode: nil; fill-column: 92 -*-
%% ex: ts=4 sw=4 et
%% @author Tim Dysinger <dysinger@opscode.com>
%% @copyright Copyright 2012 Opscode, Inc.
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

    %% This will reconfigure the system each time we startup.
    WebConfig = bksw_conf:get_configuration(),

    WebMachine = {webmachine_mochiweb,
                  {webmachine_mochiweb, start, [WebConfig]},
                  permanent, 5000, worker, dynamic},

    {ok,
     {{RestartStrategy, MaxRestarts, MaxSecondsBetweenRestarts},
      [WebMachine]}}.
