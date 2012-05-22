%% -*- erlang-indent-level: 4;indent-tabs-mode: nil; fill-column: 92 -*-
%% ex: ts=4 sw=4 et
%% @author Eric B Merritt <ericbmerritt@gmail.com>
%% @copyright Copyright 2012 Opscode, Inc.
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
-spec init([]) -> {ok, {SupFlags::term(), ChildSpec::term()}}.
init([]) ->
    RestartStrategy = one_for_one,
    MaxRestarts = 1000,
    MaxSecondsBetweenRestarts = 3600,

    SupFlags = {RestartStrategy, MaxRestarts, MaxSecondsBetweenRestarts},

    Restart = permanent,
    Shutdown = 2000,

    Store = {bkss_store_server, {bkss_store_server, start_link, []},
                Restart, Shutdown, worker, [bkss_store_server]},
    BucketSup = {bkss_bucket_sup, {bkss_bucket_sup, start_link, []},
                 Restart, Shutdown, supervisor, [bkss_bucket_sup]},

    {ok, {SupFlags, [BucketSup, Store]}}.
