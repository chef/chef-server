%% -*- erlang-indent-level: 4;indent-tabs-mode: nil;fill-column: 92 -*-
%% ex: ts=4 sw=4 et
%% @author Kevin Smith <kevin@chef.io>
%% @copyright 2011 Opscode, Inc.
-module(mover_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).
-define(CHILD_SUP(Module, Args),
        {Module, {Module, start_link, Args}, permanent,
         infinity, supervisor, [Module]}).

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

init([]) ->
    mover_manager:start_link(),
    Children = [?CHILD_SUP(mover_org_migrator_sup, []),
                ?CHILD_SUP(mover_transient_worker_sup, []),
                ?CHILD_SUP(mover_transient_migration_queue_sup, [])
            ],
    % Don't launch the eredis sup (whose client process requires
    % a valid redis instance) if we won't be using redis.
    AllChildren = case envy:get(mover, dry_run, true, boolean) of
        true ->
            Children;
        false ->
            [?CHILD_SUP(mover_eredis_sup, []) | Children]
    end,
    {ok, {{one_for_one, 10, 10}, AllChildren}}.
