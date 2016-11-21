%% -*- erlang-indent-level: 4;indent-tabs-mode: nil;fill-column: 92 -*-
%% ex: ts=4 sw=4 et
%% @author Tyler Cloke <tyler@chef.io>
%% @copyright 2013 Chef, Inc.
%%
%% A simpler version of mover_batch_migrator for transient queues

-module(mover_transient_queue_batch_migrator).

-export([
	 ping/0,
         migrate_all/1
        ]).

-include_lib("moser/include/moser.hrl").

ping() ->
    pong.

migrate_all(CallbackMod) ->
    % Disable sleep  - for OPC migrations, we are guaranteed no in-flight traffic.
    application:set_env(mover, sleep_time, 0),

    % Build dets tables
    mover_manager:create_account_dets(CallbackMod),

    run_migration(CallbackMod).

run_migration(CallbackMod) ->
    mover_manager:migrate(all, 1, CallbackMod),
    % wait for mover to be ready, and if the migration failed, return failed
    Status = mover_util:wait_for_status(),
    case proplists:get_value(fatal_stop, Status) of
	true ->
	    [{status, {aborted, fatal_error}}];
	false ->
	    [{status, complete}]
    end.

