%%%-------------------------------------------------------------------
%%% @author Oliver Ferrigni <oliver@opscode.come>
%%% @copyright (C) 2013, Oliver Ferrigni
%%% @doc
%%%
%%% @end
%%% Created :  5 Jun 2013 by Oliver Ferrigni <oliver@opscode.com>
%%%-------------------------------------------------------------------
-module(depsolver_supervisor).

-behaviour(supervisor).

%% API
-export([
         start_link/0,
         solve/3
        ]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%%===================================================================
%%% API functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the supervisor
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).
solve(DepGraph, Goals, Timeout) ->
    case erlang:whereis(?SERVER) of
      undefined ->
        start_link();
      _ ->
        ok
    end,
    {ok, Pid} = supervisor:start_child(?SERVER,[]),
    try
        Result = depsolver_worker:solve(Pid, DepGraph, Goals, Timeout),
        depsolver_worker:stop(Pid),
        Result
    catch
        exit:{timeout, _} ->
            supervisor:terminate_child(?SERVER, Pid),
            {error, resolution_timeout}
    end.
%%%===================================================================
%%% Supervisor callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Whenever a supervisor is started using supervisor:start_link/[2,3],
%% this function is called by the new process to find out about
%% restart strategy, maximum restart frequency and child
%% specifications.
%%
%% @spec init(Args) -> {ok, {SupFlags, [ChildSpec]}} |
%%                     ignore |
%%                     {error, Reason}
%% @end
%%--------------------------------------------------------------------
init([]) ->
    RestartStrategy = simple_one_for_one,
    MaxRestarts = 1000,
    MaxSecondsBetweenRestarts = 3600,

    SupFlags = {RestartStrategy, MaxRestarts, MaxSecondsBetweenRestarts},

    Restart = permanent,
    Shutdown = 2000,
    Type = worker,

    AChild = {depsolver_worker, {depsolver_worker, start_link, []},
              Restart, Shutdown, Type, [depsolver_worker]},

    {ok, {SupFlags, [AChild]}}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
