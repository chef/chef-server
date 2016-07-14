%% -*- erlang-indent-level: 4;indent-tabs-mode: nil; fill-column: 92 -*-
%% ex: ts=4 sw=4 et
%% @author Tyler Cloke <tyler@chef.io>
%% @copyright 2015 Chef Software, Inc.
%%
%% This migration iterates through all existing orgs and grants
%% the server-admins global group READ, UPDATE, and DELETE access on them,
%% as having access on the container does not update permissions for
%% existing orgs.

-module(mover_server_admins_existing_orgs_permissions_callback).

-export([
         migration_init/0,
         migration_complete/0,
         migration_type/0,
         supervisor/0,
         migration_start_worker_args/2,
         migration_action/2,
         next_object/0,
         error_halts_migration/0,
         reconfigure_object/2,
         needs_account_dets/0
        ]).

-define(GLOBAL_PLACEHOLDER_ORG_ID, <<"00000000000000000000000000000000">>).

-record(org, {name, id, authz_id}).
-record(group, {authz_id}).

migration_init() ->
    mv_oc_chef_authz_http:create_pool(),
    mover_transient_migration_queue:initialize_queue(?MODULE, get_orgs()).


migration_action(OrgRecord, _AcctInfo) ->
    OrgName = OrgRecord#org.name,
    OrgId = OrgRecord#org.authz_id,
    OrgAuthzId = OrgRecord#org.authz_id,
    BifrostSuperuserId = mv_oc_chef_authz:superuser_id(),
    ServerAdminsAuthzId = get_server_admins_authz_id(),
    %% TODO: how to handle deleted admins group?
    OrgAdminsAuthzId = get_org_admin_authz_id(OrgId),

    %% Grant server-admins permissions on existing org.
    add_permission_to_existing_org_for_server_admins(BifrostSuperuserId, OrgName, ServerAdminsAuthzId, OrgAuthzId, read),
    add_permission_to_existing_org_for_server_admins(BifrostSuperuserId, OrgName, ServerAdminsAuthzId, OrgAuthzId, update),
    add_permission_to_existing_org_for_server_admins(BifrostSuperuserId, OrgName, ServerAdminsAuthzId, OrgAuthzId, delete),
    
    %% Add server-admins to admins group.
    case mv_oc_chef_authz:add_to_group(OrgAdminsAuthzId, group, ServerAdminsAuthzId, superuser) of 
        {error, Error} ->
            lager:error("Failed to add server-admins (~p) to the admins group (~p) for this org (~p)", [ServerAdminsAuthzId, OrgAdminsAuthzId, OrgName]),
            throw(error);
        _ -> 
            ok
    end.
                               
add_permission_to_existing_org_for_server_admins(BifrostSuperuserId, OrgName, ServerAdminsAuthzId, OrgAuthzId, Permission) ->
    case mv_oc_chef_authz:add_ace_for_entity(BifrostSuperuserId, group, ServerAdminsAuthzId, object, OrgAuthzId, Permission) of
        {error, Error} ->
            lager:error("Failed to update ~p permissions for org ~p with error: ~p", [Permission, OrgName, Error]),
            throw(migration_error);
        _ ->
            ok
    end.

get_orgs() ->
    {ok, Orgs} = sqerl:select(get_orgs_sql(), [], rows_as_records, [org, record_info(fields, org)]),
    Orgs.

get_orgs_sql() ->
    <<"SELECT name, id, authz_id FROM orgs">>.

get_org_admin_authz_id(OrgId) ->
    {ok, [ServerAdmin]} = sqerl:select(get_org_admin_authz_id(OrgId), [], rows_as_records, [group], record_info(fields, group)]),
    ServerAdmin#group.authz_id.

get_org_admin_authz_id_sql() ->
    erlang:iolist_to_binary([<<"SELECT authz_id FROM groups WHERE name='admins' AND org_id=$1"]).

get_server_admins_authz_id() ->
    {ok, [ServerAdmin]} = sqerl:select(get_server_admins_authz_id_sql(), [], rows_as_records, [group, record_info(fields, group)]),
    ServerAdmin#group.authz_id.

get_server_admins_authz_id_sql() ->
    erlang:iolist_to_binary([<<"SELECT authz_id FROM groups WHERE name='server-admins' AND org_id='">>, ?GLOBAL_PLACEHOLDER_ORG_ID , <<"'">>]).

migration_complete() ->
    mv_oc_chef_authz_http:delete_pool().

%%
%% Generic mover callback functions for
%% a transient queue migration
%%
needs_account_dets() ->
    false.

migration_start_worker_args(Object, AcctInfo) ->
    [Object, AcctInfo].

next_object() ->
    mover_transient_migration_queue:next(?MODULE).

migration_type() ->
    <<"orgs_access_on_server_admins">>.

supervisor() ->
    mover_transient_worker_sup.

error_halts_migration() ->
    true.

reconfigure_object(_ObjectId, _AcctInfo) ->
    ok.
