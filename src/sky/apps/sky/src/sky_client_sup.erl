-module(sky_client_sup).
-behaviour(supervisor).

-export([start_link/0, init/1, start_client/4, spawn_all_clients/5]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    gproc:reg({a, l, connected_clients}, undefined),

    SupFlags = #{strategy => simple_one_for_one},
    ChildSpecs = [#{id => sky_client,
                    start => {sky_client, start_link, []},
                    shutdown => brutal_kill}],
    {ok, {SupFlags, ChildSpecs}}.

start_client(Host, Port, Org, Name) ->
    supervisor:start_child(?MODULE, [Host, Port, Org, Name]).

spawn_all_clients(Host, Port, Org, Name, Count) ->
    ClientNames = [ io_lib:format("~s~4..0B", [Name, I]) || I <- lists:seq(1, Count)],
    [ start_client(Host, Port, Org, NameI) || NameI <- ClientNames ].




