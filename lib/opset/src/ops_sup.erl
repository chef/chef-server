-module(ops_sup).

-behaviour(supervisor).

%% API
-export([start_link/0,
        start_config/2]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%%===================================================================
%%% API functions
%%%===================================================================

-spec start_link() -> {ok, pid()} | ignore | {error, Error::term()}.
start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

-spec start_config(opset:name(), opset:config()) -> ok.
start_config(Name, Value) ->
    supervisor:start_child(?SERVER, [Name, Value]).

%%%===================================================================
%%% Supervisor callbacks
%%%===================================================================


-spec init(Args::[]) -> {ok, {[SupFlags::term()], ChildSpecs::list()}}.
init([]) ->
    RestartStrategy = simple_one_for_one,
    MaxRestarts = 1000,
    MaxSecondsBetweenRestarts = 3600,

    SupFlags = {RestartStrategy, MaxRestarts, MaxSecondsBetweenRestarts},

    Restart = transient,
    Shutdown = 2000,
    Type = worker,

    AChild = {config, {ops_config_server, start_link, []},
              Restart, Shutdown, Type, [ops_config_server]},

    {ok, {SupFlags, [AChild]}}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
