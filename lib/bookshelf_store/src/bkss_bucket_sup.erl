%% -*- erlang-indent-level: 4;indent-tabs-mode: nil; fill-column: 92 -*-
%% ex: ts=4 sw=4 et
%% @author Eric B Merritt <ericbmerritt@gmail.com>
%% @copyright Copyright 2012 Opscode, Inc.
-module(bkss_bucket_sup).

-behaviour(supervisor).

%% API
-export([start_link/0, start_child/1]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%%===================================================================
%%% API functions
%%%===================================================================

-spec start_link() ->
    {ok, pid()} | ignore | {error, Error::term()}.
start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

-spec start_child(bookshelf_store:bucket_name()) ->
                         {ok, undefined | pid()} |
                         {ok, undefined | pid(), any()} |
                         {error, Reason::any()}.

start_child(BucketName) ->
    supervisor:start_child(?SERVER, [BucketName]).

%%%===================================================================
%%% Supervisor callbacks
%%%===================================================================

-spec init([]) ->
                  {ok, {{supervisor:strategy(), non_neg_integer(), non_neg_integer()},
                        [supervisor:child_spec()]}}.
init([]) ->
    RestartStrategy = simple_one_for_one,
    MaxRestarts = 1000,
    MaxSecondsBetweenRestarts = 3600,

    SupFlags = {RestartStrategy, MaxRestarts, MaxSecondsBetweenRestarts},

    Restart = transient,
    Shutdown = 2000,
    Type = worker,

    BucketServer = {bkss_bucket_server, {bkss_bucket_server, start_link, []},
              Restart, Shutdown, Type, [bkss_bucket_server]},

    {ok, {SupFlags, [BucketServer]}}.
