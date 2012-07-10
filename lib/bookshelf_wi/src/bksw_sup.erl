%% -*- erlang-indent-level: 4;indent-tabs-mode: nil; fill-column: 92 -*-
%% ex: ts=4 sw=4 et
%% @author Tim Dysinger <dysinger@opscode.com>
%% @copyright Copyright 2012 Opscode, Inc.
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
    RestartStrategy = one_for_one,
    MaxRestarts = 1000,
    MaxSecondsBetweenRestarts = 3600,

    SupFlags = {RestartStrategy, MaxRestarts, MaxSecondsBetweenRestarts},

    Restart = permanent,
    Shutdown = 2000,

    WebmachineSup = {bks_webmachine_sup, {bksw_webmachine_sup, start_link, []},
                     Restart, Shutdown, supervisor, [bksw_webmachine_sup]},

    {ok, {SupFlags, [WebmachineSup]}}.
