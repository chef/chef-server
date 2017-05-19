-module(migrator_sup).

-behaviour(supervisor).
%%====================================================================
%% Internal functions
%%====================================================================


-export([start_link/0, init/1]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([])   ->
    SupFlags = {one_for_one, 10, 10},
    ChildSpec = {migrator_change_listener,
                 { migrator_change_listener, start_link, []},
                 permanent, 5000, worker,
                 [migrator_change_listener]},
    {ok, {SupFlags, [ChildSpec]}}.

