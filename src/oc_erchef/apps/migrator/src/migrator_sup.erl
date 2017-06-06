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
    SupFlags = {one_for_one, 10, 10},
    ChildSpec = {migrator_change_listener,
                 { migrator_change_listener, start_link, []},
                 permanent, 5000, worker,
                 [migrator_change_listener]},
    {ok, {SupFlags, [ChildSpec]}}.

