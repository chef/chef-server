%% -*- erlang-indent-level: 4;indent-tabs-mode: nil; fill-column: 92 -*-
%% ex: ts=4 sw=4 et
%%
%% @doc Supervisor for eredis clients.
-module(mover_eredis_sup).

-behaviour(supervisor).

-export([start_link/0]).

%% Supervisor callbacks
-export([init/1,
         eredis_start_link/0
        ]).

-define(SERVER, ?MODULE).

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

init([]) ->
    ClientCount = 1, %% envy:get(eredis, client_pool_size, integer),
    error_logger:info_msg("search cache ENABLED with ~B senders~n", [ClientCount]),
    %%Host = envy:get(eredis, host, string),
    %%Port = envy:get(eredis, port, integer),
    StartUp = {?MODULE, eredis_start_link, []},
    Children = [ {make_id(I), StartUp, permanent, brutal_kill, worker, [eredis]}
                 || I <- lists:seq(1, ClientCount) ],
    {ok, {{one_for_one, 60, 10}, Children}}.

eredis_start_link() ->
    gen_server:start_link({local, mover_eredis_client}, eredis_client, ["127.0.0.1", 6379, 0, "", 100], []).

make_id(I) ->
    "eredis_" ++ integer_to_list(I).
