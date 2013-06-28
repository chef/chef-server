%% @copyright 2011-2012 Opscode, Inc.

% @doc a supervisor for mover_org_dep_validator

-module(mover_org_dep_validator_sup).

-behaviour(supervisor).

-export([init/1,
         start_link/0,
         start_worker/1]).


-define(SERVER, ?MODULE).
start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

init([]) ->
    Spec = {undefined, {mover_org_dep_validator, start_link, []},
                 temporary, 10000, worker, [mover_org_dep_validator]},
    {ok, {{simple_one_for_one, 10, 10}, [Spec]}}.

start_worker({OrgName, _AcctInfo}) ->
    supervisor:start_child(?SERVER, [OrgName]).


