-module(migrator_sup).

-behaviour(supervisor).

-export([start_link/0,
         init/1]).

%
% API
%
start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%
% Callbacks
%
init([])   ->
  % Increased restart frequence from our usual 10 times/10seconds to 50/10 seconds
  % to encourage keeping up on data unless things are really screwed...
    SupFlags = {one_for_one, 50, 10},
    ListenerSpec = {migrator_change_listener,
                    {migrator_change_listener, start_link, []},
                    permanent, 5000, worker,
                    [migrator_change_listener]},
    InserterSpec = {migrator_target_db,
                    {migrator_target_db, start_link, [{}]},
                    permanent, 5000, worker,
                    [migrator_target_db]},
    {ok, {SupFlags, [InserterSpec, ListenerSpec]}}.
