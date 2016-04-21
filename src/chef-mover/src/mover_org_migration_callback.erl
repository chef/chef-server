%% -*- erlang-indent-level: 4;indent-tabs-mode: nil; fill-column: 92 -*-
%% ex: ts=4 sw=4 et
%% @author Tyler Cloke <tyler@chef.io>
%% @copyright 2014 Chef, Inc.
-module(mover_org_migration_callback).

-include("mover.hrl").
-include_lib("moser/include/moser.hrl").

-export([
         migration_init/0,
         migration_start_worker_args/2,
         migration_action/2,
         migration_type/0,
         needs_account_dets/0,
         supervisor/0,
         error_halts_migration/0,
         reconfigure_object/2,
         next_object/0
	 ]).

migration_init() ->
    AcctInfo = moser_acct_processor:open_account(),
    AllOrgs = moser_acct_processor:all_orgs(AcctInfo),
    OrgNames = [ OrgName || #org_info{org_name=OrgName} <- AllOrgs],
    mover_transient_migration_queue:initialize_queue(?MODULE, OrgNames).

migration_start_worker_args(Object, AcctInfo) ->
    [Object, AcctInfo].

migration_action(OrgName, AcctInfo) ->
    try
        {Guid, AuthzId, LastUpdatedBy, RawObject} = moser_acct_processor:get_parsed_org_object_by_name(AcctInfo, OrgName),
        moser_org_converter:insert_org(Guid, AuthzId, LastUpdatedBy, RawObject),
        ok
    catch
        Exception:Reason ->
	    lager:error("org_migration_failure org_name: ~p Exception: ~p Reason: ~p Stacktrace: ~p ~n",
			[OrgName, Exception, Reason, erlang:get_stacktrace()]),
            ok
    end.

needs_account_dets() ->
    true.

migration_type() ->
    <<"org_migration">>.

next_object() ->
    mover_transient_migration_queue:next(?MODULE).

supervisor() ->
    mover_transient_worker_sup.

reconfigure_object(_ObjectId, _AcctInfo) ->
    ok.

error_halts_migration() ->
    true.
