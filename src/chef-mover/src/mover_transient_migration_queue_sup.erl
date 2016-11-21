%% -*- erlang-indent-level: 4;indent-tabs-mode: nil; fill-column: 92 -*-
%% ex: ts=4 sw=4 et
%%
%% @author Marc Paradise <marc@chef.io>
%% @copyright 2013, Opscode Inc
%%

-module(mover_transient_migration_queue_sup).

-behaviour(supervisor).

-export([start_link/0]).

-export([init/1]).

-define(SERVER, ?MODULE).

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

init([]) ->
    Spec = {undefined,
            {mover_transient_migration_queue, start_link, []},
                 permanent, brutal_kill, worker, [mover_transient_migration_queue]},
    {ok, {{one_for_one, 10, 10}, [Spec]}}.

