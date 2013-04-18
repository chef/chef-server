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
-define(CHILD_SUP(Module, Args),
        {Module, {Module, start_link, Args}, permanent,
         infinity, supervisor, [Module]}).

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

init([]) ->
    %% {ok, PreloadCount} = application:get_env(mover, preload_org_count),
    %% Children0 = [?CHILD_SUP(mover_worker_sup, []),
    %%              ?CHILD(mover_manager, [PreloadCount], 5000)],
    %% Children = case application:get_env(mover, dry_run) of
    %%                {ok, true} ->
    %%                    Children0;
    %%                {ok, false} ->
    %%                    {ok, RedisHost} = application:get_env(mover, redis_host),
    %%                    {ok, RedisPort} = application:get_env(mover, redis_port),
    %%                    {ok, RedisConns} = application:get_env(mover, redis_conns),
    %%                    [?CHILD_SUP(erldis_pool_sup, [[{{RedisHost, RedisPort}, RedisConns}]])|Children0]
    %%            end,
    %% FIXME: for now we just want to start and do nothing
    Children = [?CHILD_SUP(mover_org_migrator_sup, [])],
    {ok, {{one_for_one, 10, 10}, Children}}.
