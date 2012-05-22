%% -*- erlang-indent-level: 4;indent-tabs-mode: nil; fill-column: 92 -*-
%% ex: ts=4 sw=4 et
%% @author Tim Dysinger <dysinger@opscode.com>
%% @copyright Copyright 2012 Opscode, Inc.
-module(bksw_cowboy_sup).

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
    %% This will reconfigure the system each time we startup.
    Env = bksw_conf:get_configuration(),
    Pool = proplists:get_value(pool, Env),
    RestartStrategy = one_for_one,
    MaxRestarts = 1000,
    MaxSecondsBetweenRestarts = 3600,
    ListenerSup = cowboy:child_spec(bookshelf_http_listener,
                                    Pool, cowboy_tcp_transport, Env,
                                    cowboy_http_protocol, Env),
    {ok,
     {{RestartStrategy, MaxRestarts,
       MaxSecondsBetweenRestarts},
      [ListenerSup]}}.
