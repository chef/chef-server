%% -*- erlang-indent-level: 4;indent-tabs-mode: nil; fill-column: 92 -*-
%% ex: ts=4 sw=4 et
%% @author Tyler Cloke <tyler@chef.io>
%% @copyright 2015 Chef Software, Inc.
%%
%% This migration sets up a global group called server-admins.
%% It also grants READ and CREATE permissions on the users container
%% to the server-admins group.
%%
-module(mover_server_admins_global_group_callback).

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
-record(user, {authz_id}).
-record(group, {authz_id}).
-record(mover_chef_group, {
          id,
          org_id,	  
          authz_id,
          name,
          last_updated_by,
          created_at,
          updated_at
         }).


migration_init() ->
    mv_oc_chef_authz_http:create_pool(),
    mover_transient_migration_queue:initialize_queue(?MODULE, [?GLOBAL_PLACEHOLDER_ORG_ID]).

migration_action(GlobalOrgId, _AcctInfo) ->
    BifrostSuperuserAuthzId = mv_oc_chef_authz:superuser_id(),
    ErchefSuperuserAuthzId = get_erchef_superuser_authz_id(),

    %% if server-admins is already created, skip this step
    ServerAdminsAuthzId =
        case get_server_admins_authz_group_if_exists() of
            needs_created ->
                lager:info("Global group server-admins does not exist, creating."),
                CreatedServerAdminsAuthzId =
                    case create_server_admins_authz_group(BifrostSuperuserAuthzId) of
                        AuthzId when is_binary(AuthzId) ->
                            AuthzId;
                        AuthzError ->
                            lager:error("Could not create new authz group for server-admins."),
                            throw(AuthzError)
                    end,

                case create_server_admins_global_group(CreatedServerAdminsAuthzId,
                                                       GlobalOrgId) of
                    {chef_sql, {GroupError, _}} ->
                        lager:error("Could not create new erchef group for server-admins."),
                        throw(GroupError);
                    _ -> true
                end,
                CreatedServerAdminsAuthzId;
            ExistingServerAdminsAuthzId ->
                lager:warning("Global group server-admins already exists, skipping creation."),
                ExistingServerAdminsAuthzId
        end,

    %% Put pivotal in the global group
    mv_oc_chef_authz:add_to_group(ServerAdminsAuthzId, actor, ErchefSuperuserAuthzId, BifrostSuperuserAuthzId),

    %% Grant server-admins global group permissions on users container 
    UserContainerAuthzId = get_user_container_authz_id(),
    mv_oc_chef_authz:add_ace_for_entity(BifrostSuperuserAuthzId, group, ServerAdminsAuthzId, container, UserContainerAuthzId, read),
    mv_oc_chef_authz:add_ace_for_entity(BifrostSuperuserAuthzId, group, ServerAdminsAuthzId, container, UserContainerAuthzId, create),
    mv_oc_chef_authz:add_ace_for_entity(BifrostSuperuserAuthzId, group, ServerAdminsAuthzId, container, UserContainerAuthzId, update),
    mv_oc_chef_authz:add_ace_for_entity(BifrostSuperuserAuthzId, group, ServerAdminsAuthzId, container, UserContainerAuthzId, delete),

    %% Grant server-admins read permissions on itself
    mv_oc_chef_authz:add_ace_for_entity(BifrostSuperuserAuthzId, group, ServerAdminsAuthzId, group, ServerAdminsAuthzId, read),

    %% Grant pivotal permissions on server-admins global group
    mv_oc_chef_authz:add_ace_for_entity(BifrostSuperuserAuthzId, actor, ErchefSuperuserAuthzId, group, ServerAdminsAuthzId, create),
    mv_oc_chef_authz:add_ace_for_entity(BifrostSuperuserAuthzId, actor, ErchefSuperuserAuthzId, group, ServerAdminsAuthzId, read),
    mv_oc_chef_authz:add_ace_for_entity(BifrostSuperuserAuthzId, actor, ErchefSuperuserAuthzId, group, ServerAdminsAuthzId, update),
    ok.

%% Vendored from chef_objects.erl
chef_object_flatten_group(ObjectRec) ->
    [_RecName|Tail] = tuple_to_list(ObjectRec),
    %% We detect if any of the fields in the record have not been set
    %% and throw an error
    case lists:any(fun is_undefined/1, Tail) of
        true -> error({undefined_in_record, ObjectRec});
        false -> ok
    end,
    Tail.

throw_not_found(Type) ->
    lager:error("Bifrost could not find the " ++ Type ++ " permission on the users container."),
    throw(not_found).

throw_server_error() ->
    lager:error("There was an error communicating with bifrost."),
    throw(server_error).

create_server_admins_authz_group(SuperuserAuthzId) ->
    case mv_oc_chef_authz:create_resource(SuperuserAuthzId, group) of
        {ok, AuthzId} ->
            AuthzId;
        {error, _} = Error ->
            Error
    end.

get_user_container_authz_id() ->
    {ok, [Container]} = sqerl:select(users_container_query(), [], rows_as_records, [container, record_info(fields, container)]),
    Container#container.authz_id.

users_container_query() ->
    <<"SELECT authz_id FROM containers WHERE name='users'">>.

get_erchef_superuser_authz_id() ->
    {ok, [User]} = sqerl:select(get_erchef_superuser_authz_id_sql(), [], rows_as_records, [user, record_info(fields, user)]),
    User#user.authz_id.

get_erchef_superuser_authz_id_sql() ->
    <<"SELECT authz_id FROM users WHERE username='pivotal'">>.

%% for idempotence, returns authz_id id or needs_created.
get_server_admins_authz_group_if_exists() ->
    case sqerl:select(get_server_admins_authz_group_if_exists_sql(), [], rows_as_records, [group, record_info(fields, group)]) of
        {ok, [Group]} -> Group#group.authz_id;
        {ok, none}   -> needs_created
    end.

get_server_admins_authz_group_if_exists_sql() ->
    erlang:iolist_to_binary([<<"SELECT authz_id FROM groups WHERE name='server-admins' and org_id='">>, ?GLOBAL_PLACEHOLDER_ORG_ID , <<"'">>]).

create_server_admins_global_group(ServerAdminsAuthzId, GlobalOrgId) ->
    RequestorId = mv_oc_chef_authz:superuser_id(),                                                      
    Object = new_group_record(GlobalOrgId, ServerAdminsAuthzId, <<"server-admins">>, RequestorId),
    create_insert(Object, ServerAdminsAuthzId, RequestorId).

insert_group_sql() ->
    <<"INSERT INTO groups (id, org_id, authz_id, name,"
      " last_updated_by, created_at, updated_at) VALUES"
      " ($1, $2, $3, $4, $5, $6, $7)">>.

%% Similar to mover_policies_containers_creation_callback.erl
new_group_record(OrgId, AuthzId, Name, RequestorId) ->
    Now = os:timestamp(),
    Id = chef_object_base_make_org_prefix_id(OrgId, Name),
    #mover_chef_group{id = Id,
                      authz_id = AuthzId,
                      org_id = OrgId,
                      name = Name,
                      last_updated_by = RequestorId,
                      created_at = Now,
                      updated_at = Now}.

is_undefined(undefined) ->
    true;
is_undefined(_) ->
    false.

create_insert(#mover_chef_group{} = Object, AuthzId, _RequestorId) ->
    case chef_sql_create_group(chef_object_flatten_group(Object)) of
        {ok, 1} ->
            AuthzId;
        Error ->
            {chef_sql, {Error, Object}}
    end.

chef_sql_create_group(Args) ->
    sqerl:execute(insert_group_sql(), Args).

%% vendored from chef_object_base
chef_object_base_make_org_prefix_id(OrgId, Name) ->
    %% assume couchdb guid where trailing part has uniqueness
    <<_:20/binary, OrgSuffix:12/binary>> = OrgId,
    Bin = iolist_to_binary([OrgId, Name, crypto:strong_rand_bytes(6)]),
    <<ObjectPart:80, _/binary>> = erlang:md5(Bin),
    iolist_to_binary(io_lib:format("~s~20.16.0b", [OrgSuffix, ObjectPart])).

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
    <<"server_admins">>.

supervisor() ->
    mover_transient_worker_sup.

error_halts_migration() ->
    true.

reconfigure_object(_ObjectId, _AcctInfo) ->
    ok.
