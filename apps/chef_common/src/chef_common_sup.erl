-module(chef_common_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).


start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

init([]) ->
    Keyring = {chef_keyring, {chef_keyring, start_link, []},
               permanent, 2000, worker, [chef_keyring]},

    {ok, {{one_for_one, 60, 10}, [Keyring]}}.
