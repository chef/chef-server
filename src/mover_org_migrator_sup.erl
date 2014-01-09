%% -*- erlang-indent-level: 4;indent-tabs-mode: nil;fill-column: 92 -*-
%% ex: ts=4 sw=4 et
%% @author Marc Paradise <marc@opscode.com>
%% @copyright 2011-2012 Opscode, Inc.

% @doc a supervisor for mover_org_migrator

-module(mover_org_migrator_sup).

-behaviour(supervisor).

-export([init/1,
         start_link/0,
         start_worker/3]).


-define(SERVER, ?MODULE).
start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

init([]) ->
    Spec = {undefined, {mover_org_migrator, start_link, []},
                 temporary, 10000, worker, [mover_org_migrator]},
    {ok, {{simple_one_for_one, 10, 10}, [Spec]}}.

start_worker(_CallbackModule, OrgName, AcctInfo) ->
    supervisor:start_child(?SERVER, [{OrgName, AcctInfo}]).

