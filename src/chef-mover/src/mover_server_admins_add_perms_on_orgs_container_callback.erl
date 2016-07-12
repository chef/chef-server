%% -*- erlang-indent-level: 4;indent-tabs-mode: nil; fill-column: 92 -*-
%% ex: ts=4 sw=4 et
%% @author Tyler Cloke <tyler@chef.io>
%% @copyright 2015 Chef Software, Inc.
%%
%% This migration grants READ, CREATE, UPDATE, and DELETE perms to
%% server admins on the orgs container.
%%
-module(mover_server_admins_add_perms_on_orgs_container_callback).

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

-include("mover.hrl").
-include("mv_oc_chef_authz.hrl").

-define(GLOBAL_PLACEHOLDER_ORG_ID, <<"00000000000000000000000000000000">>).

-record(container, {authz_id}).
-record(group, {authz_id}).

migration_init() ->
    mv_oc_chef_authz_http:create_pool(),
    mover_transient_migration_queue:initialize_queue(?MODULE, [?GLOBAL_PLACEHOLDER_ORG_ID]).

migration_action(_GlobalOrgId, _AcctInfo) ->
    BifrostSuperuserAuthzId = mv_oc_chef_authz:superuser_id(),
    ServerAdminsAuthzId = get_server_admins_authz_id(),

    %% Grant server-admins global group permissions on orgs container
    OrgContainerAuthzId = get_org_container_authz_id(),
    mv_oc_chef_authz:add_ace_for_entity(BifrostSuperuserAuthzId, group, ServerAdminsAuthzId, container, OrgContainerAuthzId, read),
    mv_oc_chef_authz:add_ace_for_entity(BifrostSuperuserAuthzId, group, ServerAdminsAuthzId, container, OrgContainerAuthzId, create),
    mv_oc_chef_authz:add_ace_for_entity(BifrostSuperuserAuthzId, group, ServerAdminsAuthzId, container, OrgContainerAuthzId, update),
    mv_oc_chef_authz:add_ace_for_entity(BifrostSuperuserAuthzId, group, ServerAdminsAuthzId, container, OrgContainerAuthzId, delete),
    ok.

get_server_admins_authz_id() ->
    {ok, [ServerAdmin]} = sqerl:select(get_server_admins_authz_id_sql(), [], rows_as_records, [group, record_info(fields, group)]),
    ServerAdmin#group.authz_id.

get_server_admins_authz_id_sql() ->
    erlang:iolist_to_binary([<<"SELECT authz_id FROM groups WHERE name='server-admins' and org_id='">>, ?GLOBAL_PLACEHOLDER_ORG_ID , <<"'">>]).

get_org_container_authz_id() ->
    {ok, [Container]} = sqerl:select(org_container_query(), [], rows_as_records, [container, record_info(fields, container)]),
    Container#container.authz_id.

org_container_query() ->
    <<"SELECT authz_id FROM containers WHERE name='organizations'">>.

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
    <<"server_admins_add_perms_on_orgs_container_callback">>.

supervisor() ->
    mover_transient_worker_sup.

error_halts_migration() ->
    true.

reconfigure_object(_ObjectId, _AcctInfo) ->
    ok.
