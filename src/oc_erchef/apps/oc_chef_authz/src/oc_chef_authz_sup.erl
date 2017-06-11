-module(oc_chef_authz_sup).
-export([
         init/1,
         start_link/0
        ]).

-define(SERVER, ?MODULE).
-include("oc_chef_authz_cleanup.hrl").
start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

init([]) ->
    RestartStrategy = one_for_one,
    MaxRestarts = 1000,
    MaxSecondsBetweenRestarts = 3600,

    Restart = permanent,
    Shutdown = ?CLEANUP_TIMEOUT,
    Type = worker,

    SupFlags = {RestartStrategy, MaxRestarts, MaxSecondsBetweenRestarts},

    CleanupWorker = {oc_chef_authz_cleanup, {oc_chef_authz_cleanup, start_link, []},
            Restart, Shutdown, Type, [oc_chef_authz_cleanup]},

    {ok, {SupFlags, [CleanupWorker]}}.
