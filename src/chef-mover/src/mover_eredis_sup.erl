%% -*- erlang-indent-level: 4;indent-tabs-mode: nil; fill-column: 92 -*-
%% ex: ts=4 sw=4 et
%%
%% @doc Supervisor for eredis clients.
-module(mover_eredis_sup).

-behaviour(supervisor).

-export([start_link/0]).

%% Supervisor callbacks
-export([init/1,
         eredis_start_link/3
        ]).

-define(SERVER, ?MODULE).

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

init([]) ->
    Host = envy:get(mover, eredis_host, string),
    Port = envy:get(mover, eredis_port, integer),
    {ok, Password} = chef_secrets:get(<<"redis_lb">>, <<"password">>),
    StartUp = {?MODULE, eredis_start_link, [Host, Port, Password]},
    Child = [{eredis, StartUp, permanent, brutal_kill, worker, [eredis]}],
    {ok, {{one_for_one, 60, 10}, Child}}.

eredis_start_link(Host, Port, Password) ->
    gen_server:start_link({local, mover_eredis_client}, eredis_client, [Host, Port, 0, Password, 100, 2000], []).
