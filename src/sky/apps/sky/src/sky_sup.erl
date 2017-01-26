-module(sky_sup).
-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).

start_link() ->
	supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
  SupFlags = #{strategy => one_for_one},
	ChildSpecs = [#{id => sky_client_sup,
                  start => {sky_client_sup, start_link, []},
                  restart => permanent,
                  shutdown => infinity,
                  type => supervisor}],
	{ok, {SupFlags, ChildSpecs}}.
