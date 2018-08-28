%% -*- erlang-indent-level: 4;indent-tabs-mode: nil;fill-column: 92 -*-
%% ex: ts=4 sw=4 et
%% @author Marc Paradise <marc@chef.io>
%% @copyright 2013-2018 Chef Software, Inc.
%%
%% @doc a supervisor for mover_transient_worker

-module(mover_transient_worker_sup).

-behaviour(supervisor).

-export([init/1,
         start_link/0,
         start_worker/3]).

-define(SERVER, ?MODULE).
start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

init([]) ->
    Spec = {undefined, {mover_transient_worker, start_link, []},
                 temporary, 10000, worker, [mover_transient_worker]},
    {ok, {{simple_one_for_one, 10, 10}, [Spec]}}.

start_worker(CallbackModule, ObjectId, MigratorArgs) ->
    supervisor:start_child(?SERVER, [{CallbackModule, ObjectId, MigratorArgs}]).
