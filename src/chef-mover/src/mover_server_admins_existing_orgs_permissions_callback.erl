%% -*- erlang-indent-level: 4;indent-tabs-mode: nil; fill-column: 92 -*-
%% ex: ts=4 sw=4 et
%% @author Tyler Cloke <tyler@chef.io>
%% @copyright 2015 Chef Software, Inc.
%%
%% This migration iterates through all existing orgs and grants
%% the server-admins global group READ, UPDATE, and DELETE access on them,
%% as having access on the container does not update permissions for
%% existing orgs.
%%
%% Should be idempotent. If the admins group doesn't exist for an org,
%% it creates an empty one to throw server-admins in with a warning.

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
    mover_transient_migration_queue:initialize_queue(?MODULE, get_orgs()).


migration_action(OrgRecord, _AcctInfo) ->
    OrgName = OrgRecord#org.name,
    OrgId = OrgRecord#org.id,
    OrgAuthzId = OrgRecord#org.authz_id,
    BifrostSuperuserId = mv_oc_chef_authz:superuser_id(),
    ServerAdminsAuthzId = get_server_admins_authz_id(),

    % Get the admins group authz id for this org, or if it doesn't exist, create it with a warning.
    OrgAdminsAuthzId =
        case get_org_admins_authz_group_if_exists(OrgId) of
            admins_group_missing ->
                lager:warn("There was no admins group for org ~p with id ~p. Creating one from scratch.", [OrgName, OrgId]),
                lager:warn("This probably means that this org is broken as it has no admins, but now you can add a new one via server-admins!"),
                NewAdminsGroupAuthzId = case create_group_in_authz(BifrostSuperuserId) of
                                            AuthzId when is_binary(AuthzId) ->
                                                AuthzId;
                                            AuthzError ->
                                                lager:error("Could not create new authz group for admins for org ~p with id ~p.", [OrgName, OrgId]),
                                                throw(AuthzError)
                                        end,

                case create_admins_group(NewAdminsGroupAuthzId, OrgId) of
                    {chef_sql, {GroupError, _}} ->
                        lager:error("Could not create new admins group for org ~p with id ~p.", [OrgName, OrgId]),
                        throw(GroupError);
                    _ -> true
                end,
                NewAdminsGroupAuthzId;
            ExistingServerAdminsOrgId ->
                ExistingServerAdminsOrgId
        end,


    %% Grant server-admins permissions on existing org.
    add_permission_to_existing_org_for_server_admins(BifrostSuperuserId, OrgName, ServerAdminsAuthzId, OrgAuthzId, read),
    add_permission_to_existing_org_for_server_admins(BifrostSuperuserId, OrgName, ServerAdminsAuthzId, OrgAuthzId, update),
    add_permission_to_existing_org_for_server_admins(BifrostSuperuserId, OrgName, ServerAdminsAuthzId, OrgAuthzId, delete),
    
    %% Add server-admins to admins group.
    case mv_oc_chef_authz:add_to_group(OrgAdminsAuthzId, group, ServerAdminsAuthzId, superuser) of 
        {error, Error} ->
            lager:error("Failed to add server-admins ~p to the admins group ~p for org ~p with id ~p.", [ServerAdminsAuthzId, OrgAdminsAuthzId, OrgName, OrgId]),
            throw(Error);
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

create_group_in_authz(SuperuserAuthzId) ->
    case mv_oc_chef_authz:create_resource(SuperuserAuthzId, group) of
        {ok, AuthzId} ->
            AuthzId;
        {error, _} = Error ->
            Error
    end.

get_orgs() ->
    {ok, Orgs} = sqerl:select(get_orgs_sql(), [], rows_as_records, [org, record_info(fields, org)]),
    Orgs.

get_orgs_sql() ->
    <<"SELECT name, id, authz_id FROM orgs">>.

get_org_admins_authz_group_if_exists(OrgId) ->
    case sqerl:select(get_org_admins_authz_group_if_exists_sql(OrgId),
                      [], rows_as_records, [group, record_info(fields, group)]) of
        {ok, [ServerAdmin]} ->
            ServerAdmin#group.authz_id;
        {ok, none} ->
            admins_group_missing            
    end.

get_org_admins_authz_group_if_exists_sql(OrgId) ->
    erlang:iolist_to_binary([<<"SELECT authz_id FROM groups WHERE name='admins' AND org_id='">>, OrgId, <<"'">>]).

get_server_admins_authz_id() ->
    {ok, [ServerAdmin]} = sqerl:select(get_server_admins_authz_id_sql(), [], rows_as_records, [group, record_info(fields, group)]),
    ServerAdmin#group.authz_id.

get_server_admins_authz_id_sql() ->
    erlang:iolist_to_binary([<<"SELECT authz_id FROM groups WHERE name='server-admins' AND org_id='">>, ?GLOBAL_PLACEHOLDER_ORG_ID , <<"'">>]).

migration_complete() ->
    mv_oc_chef_authz_http:delete_pool().

%%
%% Vendored or slightly modified from mover_server_admins_global_group_callback.
%%
create_admins_group(GroupAuthzId, OrgId) ->
    RequestorId = mv_oc_chef_authz:superuser_id(),                                                      
    Object = new_group_record(OrgId, GroupAuthzId, <<"admins">>, RequestorId),
    create_insert(Object, GroupAuthzId, RequestorId).

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

chef_sql_create_group(Args) ->
    sqerl:execute(insert_group_sql(), Args).

insert_group_sql() ->
    <<"INSERT INTO groups (id, org_id, authz_id, name,"
      " last_updated_by, created_at, updated_at) VALUES"
      " ($1, $2, $3, $4, $5, $6, $7)">>.

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

%%
%% Vendored from chef_object_base.erl.
%%
chef_object_base_make_org_prefix_id(OrgId, Name) ->
    %% assume couchdb guid where trailing part has uniqueness
    <<_:20/binary, OrgSuffix:12/binary>> = OrgId,
    Bin = iolist_to_binary([OrgId, Name, crypto:rand_bytes(6)]),
    <<ObjectPart:80, _/binary>> = crypto:hash(md5, Bin),
    iolist_to_binary(io_lib:format("~s~20.16.0b", [OrgSuffix, ObjectPart])).

%%
%% Vendored from chef_objects.erl.
%%
chef_object_flatten_group(ObjectRec) ->
    [_RecName|Tail] = tuple_to_list(ObjectRec),
    %% We detect if any of the fields in the record have not been set
    %% and throw an error
    case lists:any(fun is_undefined/1, Tail) of
        true -> error({undefined_in_record, ObjectRec});
        false -> ok
    end,
    Tail.


%%
%% Generic mover callback functions for
%% a transient queue migration.
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
