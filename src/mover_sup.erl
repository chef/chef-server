%% -*- erlang-indent-level: 4;indent-tabs-mode: nil;fill-column: 92 -*-
%% ex: ts=4 sw=4 et
%% @author Kevin Smith <kevin@opscode.com>
%% @copyright 2011 Opscode, Inc.
-module(mover_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).
-define(CHILD(Module, Args, Shutdown), {Module, {Module, start_link, Args}, permanent,
                                        Shutdown, worker, [Module]}).

-define(CHILD_SUP(Module, Args, Shutdown),
        {Module, {Module, start_link, Args}, permanent,
         Shutdown, supervisor, [Module]}).

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

init([]) ->
    {ok, PreloadCount} = application:get_env(mover, preload_org_count),
    Children = [?CHILD_SUP(mover_worker_sup, [], infinity),
                ?CHILD(mover_manager, [PreloadCount], 5000)],
    {ok, {{one_for_one, 10, 10}, Children}}.
