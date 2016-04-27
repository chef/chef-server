%% -*- erlang-indent-level: 4;indent-tabs-mode: nil; fill-column: 92 -*-
%% ex: ts=4 sw=4 et
%% @author Tyler Cloke <tyler@chef.io>
%%
%% Copyright 2014 Chef, Inc. All Rights Reserved.

-module(mover_global_groups_migration_callback).

-export([
	 migration_init/0,
	 migration_type/0,
	 supervisor/0,
	 migration_start_worker_args/2,
	 error_halts_migration/0,
	 reconfigure_object/2,
	 migration_action/2,
	 next_object/0
	]).

-include("mover.hrl").
-include_lib("moser/include/moser.hrl").

migration_init() ->
    AcctInfo = moser_acct_processor:open_account(),
    mover_transient_migration_queue:initialize_queue(?MODULE, moser_acct_processor:get_global_groups_list(AcctInfo)).

migration_start_worker_args(Object, AcctInfo) ->
    [Object, AcctInfo].

next_object() ->
    mover_transient_migration_queue:next(?MODULE).

migration_action(Object, _AcctInfo) ->
    {Guid, AuthzId, RequesterId, Data} = Object,
    try
        moser_global_object_converter:insert_group(Guid, AuthzId, RequesterId, Data)
    catch
        Exception:Reason ->
	    lager:error("global_groups_error guid: ~p Exception: ~p Reason: ~p Stacktrace: ~p ~n",
			[Guid, Exception, Reason, erlang:get_stacktrace()])
    end,
    ok.

migration_type() ->
    <<"global_groups_migration">>.

supervisor() ->
    mover_transient_worker_sup.

error_halts_migration() ->
    true.

reconfigure_object(_ObjectId, _AcctInfo) ->
    no_op.
