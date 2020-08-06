%% -*- erlang-indent-level: 4;indent-tabs-mode: nil; fill-column: 92 -*-
%% ex: ts=4 sw=4 et
%% @author Tyler Cloke <tyler@chef.io>
%% @copyright 2014 Chef, Inc.
-module(mover_org_user_invites_migration_callback).

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
    AllUserOrgAssoc = moser_acct_processor:all_org_association_data(AcctInfo, association_request),
    mover_transient_migration_queue:initialize_queue(?MODULE, AllUserOrgAssoc).

migration_start_worker_args(Object, AcctInfo) ->
    [Object, AcctInfo].

migration_action(Object, _AcctInfo) ->
    {UserGuid, OrgGuid, LastUpdatedBy, UserBody} = Object,
    try
	moser_org_converter:insert_org_user_invite(UserGuid, OrgGuid, LastUpdatedBy, UserBody)
    catch
        Exception:Reason:Stacktrace ->
            % only warn when foreign_key is thrown, this just means either the user_id or org_id cannot be found,
            % meaning this is an invite for a user or org that no longer exists, so we can safely ignore it.
            case Reason of
                {chef_sql,{{foreign_key, _}, _, _}} ->
                    lager:warning("org_user_invites_warning Foreign key constraint missing, meaning either the org: ~p or user: ~p no longer exists. Ignoring this invite as it is no longer valid. Exception Info: ~p ~n",
                                  [OrgGuid, UserGuid, Reason]);
                _ ->
                    lager:error("org_user_invites_failure org_id: ~p user_id: ~p Exception: ~p Reason: ~p Stacktrace: ~p ~n",
                                [OrgGuid, UserGuid, Exception, Reason, Stacktrace])
            end
    end,
    ok.

next_object() ->
    mover_transient_migration_queue:next(?MODULE).

migration_type() ->
    <<"org_user_invites_migration">>.

supervisor() ->
    mover_transient_worker_sup.

error_halts_migration() ->
    true.

reconfigure_object(_ObjectId, _AcctInfo) ->
    no_op.
