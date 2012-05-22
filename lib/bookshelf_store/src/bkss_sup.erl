%%%-------------------------------------------------------------------
%%% @author Eric B Merritt <ericbmerritt@gmail.com>
%%% @copyright (C) 2012, Opscode, Inc.
%%%-------------------------------------------------------------------
-module(bkss_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%%===================================================================
%%% API functions
%%%===================================================================
-spec start_link() -> {ok, pid()} | ignore | {error, Error::term()}.
start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%%===================================================================
%%% Supervisor callbacks
%%%===================================================================
-spec init([]) -> {ok, {SupFlags::term(), ChildSpec::term()}} |
                  ignore |
                  {error, Reason::term()}.

init([]) ->
    RestartStrategy = one_for_one,
    MaxRestarts = 1000,
    MaxSecondsBetweenRestarts = 3600,

    SupFlags = {RestartStrategy, MaxRestarts, MaxSecondsBetweenRestarts},

    Restart = permanent,
    Shutdown = 2000,

    Store = {bkss_store_server, {bkss_store_server, start_link, []},
                Restart, Shutdown, worker, [bkss_store_server]},
    BucketSup = {bkss_bucket_sub, {bkss_bucket_sub, start_link, []},
                 Restart, Shutdown, supervisor, [bkss_bucket_sup]},

    {ok, {SupFlags, [Store, BucketSup]}}.
