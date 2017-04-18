%% -*- erlang-indent-level: 4;indent-tabs-mode: nil; fill-column: 92 -*-
%% ex: ts=4 sw=4 et
%% @author Tyler Cloke <tyler@chef.io>
%% @copyright 2016 Chef Software, Inc.
%%

-module(mover_actor_keys_access_group_callback).

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

-record(org, {name, id, authz_id}).
-record(group, {name, id, authz_id}).
-record(user, {authz_id}).

migration_init() ->
    mv_oc_chef_authz_http:create_pool(),
    mover_transient_migration_queue:initialize_queue(?MODULE, all_orgs()).

migration_action(#org{name = OrgName, id=OrgId} = Org, _) ->
    AccessGroupName = <<"public_key_read_access">>,
    AccessGroupAuthzId = create_group(AccessGroupName, OrgId),
    UserGroup = user_group(Org),
    AdminsGroup = admins_group(Org),
    AdminsGroupAuthzId = AdminsGroup#group.authz_id,
    ClientGroup = client_group(Org),
    case process_group(UserGroup, AccessGroupAuthzId, OrgName) of
	{error, Error} ->
	    Error;
	ok ->
	    Result = process_group(ClientGroup, AccessGroupAuthzId, OrgName),
            add_permissions(Result, AccessGroupAuthzId, AdminsGroupAuthzId)
    end.

add_permissions({error, Error}, _AccessGroupAuthzId, _AdminsGroupAuthzId) ->
    {error, Error};
add_permissions(ok, AccessGroupAuthzId, AdminsGroupAuthzId) ->
    BifrostSuperuserAuthzId = mv_oc_chef_authz:superuser_id(),
    ErchefSuperuserAuthzId = get_erchef_superuser_authz_id(),
    % add pivotal to every ace
    mv_oc_chef_authz:add_ace_for_entity(BifrostSuperuserAuthzId, actor, ErchefSuperuserAuthzId, group, AccessGroupAuthzId, read),
    mv_oc_chef_authz:add_ace_for_entity(BifrostSuperuserAuthzId, actor, ErchefSuperuserAuthzId, group, AccessGroupAuthzId, create),
    mv_oc_chef_authz:add_ace_for_entity(BifrostSuperuserAuthzId, actor, ErchefSuperuserAuthzId, group, AccessGroupAuthzId, update),
    mv_oc_chef_authz:add_ace_for_entity(BifrostSuperuserAuthzId, actor, ErchefSuperuserAuthzId, group, AccessGroupAuthzId, delete),
    mv_oc_chef_authz:add_ace_for_entity(BifrostSuperuserAuthzId, actor, ErchefSuperuserAuthzId, group, AccessGroupAuthzId, grant),
    % add the admins group to read and update
    mv_oc_chef_authz:add_ace_for_entity(BifrostSuperuserAuthzId, group, AdminsGroupAuthzId, group, AccessGroupAuthzId, read),
    mv_oc_chef_authz:add_ace_for_entity(BifrostSuperuserAuthzId, group, AdminsGroupAuthzId, group, AccessGroupAuthzId, update).

process_group(Group, AccessGroupAuthzId, OrgName) ->
    case add_group_to_group(Group, AccessGroupAuthzId) of
        ok ->
            ok;
        {error, failure_creating_group} ->
            lager:warn("Failed to create keys access group for Organization ~p so cannot be migrated", [OrgName]),
            ok;
        {error, no_user_group} ->
            lager:warn("Organization ~p has no user group and cannot be migrated.", [OrgName]),
            ok;
        {error, no_admins_group} ->
            lager:warn("Organization ~p has no admins group and cannot be migrated.", [OrgName]),
            ok;
        {error, no_client_group} ->
            lager:warn("Organization ~p has no client group and cannot be migrated.", [OrgName]),
            ok;
        {error, not_found} ->
            lager:warn("Organization ~p is missing bifrost data for either the users or clients group and cannot be migrated.", [OrgName]),
            ok;
        {error, Error} ->
            lager:error("Organization ~p failed during group addition.", [OrgName]),
            {error, Error}
    end.

users_group_query() ->
    <<"SELECT name, id, authz_id FROM groups WHERE name = 'users' AND org_id = $1">>.

admins_group_query() ->
    <<"SELECT name, id, authz_id FROM groups WHERE name = 'admins' AND org_id = $1">>.

select_group_query() ->
    <<"SELECT name, id, authz_id FROM groups WHERE name = $1 AND org_id = $2">>.

clients_group_query() ->
    <<"SELECT name, id, authz_id FROM groups WHERE name = 'clients' AND org_id = $1">>.

group_create_query() ->
    <<"INSERT INTO groups (id, org_id, authz_id, name, last_updated_by, created_at, updated_at) VALUES ($1, $2, $3, $4, $5, $6, $7)">>.

get_erchef_superuser_query() ->
    <<"SELECT authz_id FROM users WHERE username='pivotal'">>.

get_erchef_superuser_authz_id() ->
    {ok, [User]} = sqerl:select(get_erchef_superuser_query(), [], rows_as_records, [user, record_info(fields, user)]),
    User#user.authz_id.

user_group(#org{id = OrgId}) ->
    case sqerl:select(users_group_query(), [OrgId], rows_as_records, [group, record_info(fields, group)]) of
        {ok, [Group]} ->
            Group;
        {ok, none} ->
            {error, no_user_group}
    end.

admins_group(#org{id = OrgId}) ->
    case sqerl:select(admins_group_query(), [OrgId], rows_as_records, [group, record_info(fields, group)]) of
        {ok, [Group]} ->
            Group;
        {ok, none} ->
            {error, no_admins_group}
    end.

client_group(#org{id = OrgId}) ->
    case sqerl:select(clients_group_query(), [OrgId], rows_as_records, [group, record_info(fields, group)]) of
        {ok, [Group]} ->
            Group;
        {ok, none} ->
            {error, no_client_group}
    end.

add_group_to_group({error, no_user_group} = Error, _AccessGroup) ->
    Error;
add_group_to_group({error, no_client_group} = Error, _AccessGroup) ->
    Error;
add_group_to_group(_UserGroup, {error, failure_creating_group} = Error) ->
    Error;
add_group_to_group(#group{authz_id = IdToAdd}, TargetId) ->
    mv_oc_chef_authz:add_to_group(TargetId, group, IdToAdd, superuser).

create_group(Name, OrgId) ->
    case sqerl:select(select_group_query(), [Name, OrgId], rows_as_scalars, [authz_id]) of
        {ok, none} ->
            Now = os:timestamp(),
            SuperuserId = mv_oc_chef_authz:superuser_id(),
            {ok, AuthzId} = mv_oc_chef_authz:create_resource(SuperuserId, group),
            case sqerl:execute(group_create_query(), [
                                                      chef_object_base_make_org_prefix_id(OrgId, Name),
                                                      OrgId,
                                                      AuthzId,
                                                      Name,
                                                      SuperuserId,
                                                      Now,
                                                      Now
                                                     ]) of
                {ok, _} ->
                    AuthzId;
                {error, _} ->
                    {error, failure_creating_group}
            end;
        {ok, [AuthzId | _]} ->
            AuthzId;
        _ ->
            {error, failure_creating_group}
    end.

all_orgs_query() ->
    <<"SELECT name, id, authz_id FROM orgs">>.

all_orgs() ->
    %% TODO: Will this be Bad(TM) in Hosted?
    {ok, Orgs} = sqerl:select(all_orgs_query(), [], rows_as_records, [org, record_info(fields, org)]),
    Orgs.

%% vendored from chef_object_base
chef_object_base_make_org_prefix_id(OrgId, Name) ->
    %% assume couchdb guid where trailing part has uniqueness
    <<_:20/binary, OrgSuffix:12/binary>> = OrgId,
    Bin = iolist_to_binary([OrgId, Name, crypto:rand_bytes(6)]),
    <<ObjectPart:80, _/binary>> = crypto:hash(md5, Bin),
    iolist_to_binary(io_lib:format("~s~20.16.0b", [OrgSuffix, ObjectPart])).

%%
%% Generic mover callback functions for
%% a transient queue migration
%%
migration_complete() ->
    mv_oc_chef_authz_http:delete_pool().

needs_account_dets() ->
    false.

migration_start_worker_args(Object, AcctInfo) ->
    [Object, AcctInfo].

next_object() ->
    mover_transient_migration_queue:next(?MODULE).

migration_type() ->
    <<"actor_keys_access_group">>.

supervisor() ->
    mover_transient_worker_sup.

error_halts_migration() ->
    false.

reconfigure_object(_ObjectId, _AcctInfo) ->
    ok.
