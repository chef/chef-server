%% -*- erlang-indent-level: 4;indent-tabs-mode: nil; fill-column: 92 -*-
%% ex: ts=4 sw=4 et
%% @author Stephen Delano <stephen@chef.io>
%% Copyright 2013 Opscode, Inc. All Rights Reserved.

-module(oc_chef_group).

-include("oc_chef_types.hrl").

-behaviour(chef_object).

-define(DEFAULT_HEADERS, []).

-export([
         parse_binary_json/1,
         assemble_group_ejson/2,
         delete/2,
         handle_error_for_update_ops/2,
         create_record/4,
         add_user_member/2,
         remove_user_member/2,
         add_group_member/2,
         remove_group_member/2

        ]).

%% chef_object behaviour callbacks
-export([
         authz_id/1,
         bulk_get_query/1,
         create_query/1,
         delete_query/1,
         ejson_for_indexing/2,
         fields_for_fetch/1,
         fields_for_update/1,
         fields_for_insert/1,
         find_query/1,
         id/1,
         is_indexed/1,
         list/2,
         list_query/1,
         name/1,
         new_record/4,
         org_id/1,
         record_fields/1,
         set_created/2,
         set_updated/2,
         set_api_version/2,
         type_name/1,
         update_from_ejson/2,
         update_query/1,
         update/2,
         fetch/2
        ]).

% TODO: move these somewhere generic; also used by oc_chef_wm_acl
-export([
         fetch_bare/2,
         fetch_new/2
        ]).

name(#oc_chef_group{name = Name}) ->

    Name.
id(#oc_chef_group{id = Id}) ->
    Id.

type_name(#oc_chef_group{}) ->
    group.

authz_id(#oc_chef_group{authz_id = AuthzId}) ->
    AuthzId.

org_id(#oc_chef_group{org_id = OrgId}) ->
    OrgId.

add_user_member(#oc_chef_group{users = Users} = Group, NewUser) ->
    Group#oc_chef_group{users = [NewUser] ++ Users}.

remove_user_member(#oc_chef_group{users = Users} = Group, UserToDelete) ->
    Group#oc_chef_group{users =  lists:delete(UserToDelete, Users)}.

add_group_member(#oc_chef_group{groups = Groups} = Group, NewGroup) ->
    Group#oc_chef_group{groups = [NewGroup] ++ Groups}.

remove_group_member(#oc_chef_group{groups = Groups} = Group, GroupToDelete) ->
    Group#oc_chef_group{groups = lists:delete(GroupToDelete, Groups)}.

create_query(_ObjectRec) ->
    insert_group.

update_query(_ObjectRec) ->
    update_group_by_id.

delete_query(_ObjectRec) ->
    delete_group_by_id.

find_query(_ObjectRec) ->
    find_group_by_orgid_name.

list_query(_ObjectRec) ->
    list_groups_for_org.

bulk_get_query(_ObjectRec) ->
    %% TODO: do we need this?
    ok.

new_record(ApiVersion, OrgId, AuthzId, GroupData) ->
    Name = ej:get({<<"id">>}, GroupData, ej:get({<<"groupname">>}, GroupData)),
    Id = chef_object_base:make_org_prefix_id(OrgId, Name),
    #oc_chef_group{server_api_version = ApiVersion,
                   id = Id,
                   authz_id = AuthzId,
                   org_id = OrgId,
                   name = Name}.

create_record(ApiVersion, OrgId, Name, RequestingActorId) ->
    Group = #oc_chef_group{server_api_version = ApiVersion,
                           id = chef_object_base:make_org_prefix_id(OrgId, Name),
                           org_id = OrgId,
                           name = Name},
    set_created(Group, RequestingActorId).

set_created(#oc_chef_group{} = Object, ActorId) ->
    Now = chef_object_base:sql_date(now),
    Object#oc_chef_group{created_at = Now, updated_at = Now, last_updated_by = ActorId}.

set_updated(#oc_chef_group{} = Object, ActorId) ->
    Now = chef_object_base:sql_date(now),
    Object#oc_chef_group{updated_at = Now, last_updated_by = ActorId}.

is_indexed(_ObjectRec) ->
    false.

ejson_for_indexing(#oc_chef_group{}, _EjsonTerm) ->
   {[]}.

update_from_ejson(#oc_chef_group{name = OrigName, clients = OrigClients, users = OrigUsers, groups = OrigGroups} = Group, GroupData) ->
    Name = ej:get({<<"groupname">>}, GroupData, OrigName),
    Clients = ej:get({<<"actors">>, <<"clients">>}, GroupData, OrigClients),
    Groups = ej:get({<<"actors">>, <<"groups">>}, GroupData, OrigGroups),
    Users = ej:get({<<"actors">>, <<"users">>}, GroupData, OrigUsers),
    Group#oc_chef_group{name = Name, clients = Clients, groups = Groups, users = Users}.

fields_for_update(#oc_chef_group{last_updated_by = LastUpdatedBy,
                                     updated_at = UpdatedAt,
                                     name = Name,
                                     id = Id}) ->
    [LastUpdatedBy, UpdatedAt, Name, Id].


fields_for_fetch(#oc_chef_group{org_id = OrgId,
                                    name = Name}) ->
    [OrgId, Name].

record_fields(_ApiVersion) ->
    record_info(fields, oc_chef_group).

list(#oc_chef_group{org_id = OrgId} = Group, CallbackFun) ->
    CallbackFun({list_query(Group), [OrgId], [name]}).

%%The process for updating a group is as follows
%% 1. Update the group in sql for renames
%% 2. Collect all authz_ids for the names
%%    in the list of clients/users/groups
%%    to add to the group.
%% 3. Update bifrost with new actor/group
%%    authz ids
%% 4. Diff the authz ids previously in
%%    bifrost with the list of authz ids
%%    from the user.
%% 5. Delete from the group in bifrost
%%    the authz ids thare now removed in
%%    bifrost.

update(#oc_chef_group{
                      org_id = OrgId,
                      authz_id = GroupAuthzId,
                      last_updated_by = AuthzId,
                      clients = Clients,
                      users = Users,
                      groups = Groups,
                      auth_side_actors = AuthSideActors,
                      auth_side_groups = AuthSideGroups
                     } = Record, CallbackFun) ->
    case chef_object_default_callbacks:update(Record, CallbackFun) of
        %% If the group exists, N should be 1.
        N when is_integer(N) andalso N > 0 ->
            %% Alright, here we go:
            %%
            %% The main problem here is that we are trying to measure time inside a fun call that
            %% is already being measured, yes! Right here we are being call from chef_db:update()
            %% that itself is calling chef_sql:update() that itself (you still with me?) is calling
            %% chef_object:update() wich would be this case right here. So the question woulb be:
            %%
            %% => Do we wanna measure time inside a call that is already being measured?
            %%
            %% I don't think so, therefor we are proposing to create a different change to address
            %% the problem globally. Why? Well we are going to modify core modules that might or might
            %% not require modifications to a few other modules.
            %%
            %% Until we have this conversation I'm mocking the ReqId in YOLO mode.
            ReqId = <<"mock">>,
            LookupContext = oc_chef_authz_scoped_name:initialize_context(ReqId, OrgId, CallbackFun),
            ClientAuthzIds = oc_chef_authz_scoped_name:find_client_authz_ids(Clients, LookupContext),
            UserAuthzIds = oc_chef_authz_scoped_name:find_user_authz_ids(Users, LookupContext),
            GroupAuthzIds = oc_chef_authz_scoped_name:find_group_authz_ids(Groups, LookupContext),
            UserSideActorsAuthzIds = UserAuthzIds ++ ClientAuthzIds,
            %% Subtract from the Authz returned ids, the list of known good ids.
            %% The remainder are stale ids in authz that should be removed.
            %% These stale authz ids were orphaned due to missing cleanup during
            %% delete.
            ActorsToRemove = subtract(default_to_empty(AuthSideActors), UserSideActorsAuthzIds),
            GroupsToRemove = subtract(default_to_empty(AuthSideGroups), GroupAuthzIds),
            Paths = build_paths(GroupAuthzId),
            OpsResults = [
              add_new_authz_ids(Paths, UserSideActorsAuthzIds, GroupAuthzIds, AuthzId),
              remove_deleted_authz_ids(Paths, ActorsToRemove, GroupsToRemove, AuthzId)],
            handle_error_for_update_ops(OpsResults, N);
       Other  ->
            Other
    end.
-spec subtract(list(), list()) -> list().
subtract(List1, List2) ->
    sets:to_list(sets:subtract(sets:from_list(List1), sets:from_list(List2))).

build_paths(GroupAuthzId) ->
    BasePath = authz_id_path("/groups/", GroupAuthzId),
    ActorsPath = BasePath ++ "/actors/",
    GroupsPath = BasePath ++ "/groups/",
    {ActorsPath, GroupsPath}.

remove_deleted_authz_ids({ActorsPath, GroupsPath}, ActorsToRemove, GroupsToRemove, AuthzId) ->
    [delete_authz_ids(ActorsPath, ActorsToRemove, AuthzId),
     delete_authz_ids(GroupsPath, GroupsToRemove, AuthzId)].


add_new_authz_ids({ActorsPath, GroupsPath}, UserSideActorsAuthzIds, GroupAuthzIds, AuthzId) ->
    [put_authz_ids(ActorsPath, UserSideActorsAuthzIds, AuthzId),
     put_authz_ids(GroupsPath, GroupAuthzIds, AuthzId)].

handle_error_for_update_ops(OpsResults, N) ->
    %% If there are forbidden results for any reason, kick off a 403 and a list
    %% of AuthzIds that are forbidden
    case all_errors(lists:flatten(OpsResults)) of
        [] ->
            length(OpsResults) + N;
        Errors ->
            error_logger:error_report({oc_chef_group, update, error_in_bifrost, Errors}),
            {error, error_in_bifrost}
    end.

default_to_empty(List) when is_list(List) ->
    List;
default_to_empty(_) ->
    [].

%% Updates a collection of authz_ids. If there are any errors, stash the error and continue.
%% Partial updates are OK, so we want to be greedy.
put_authz_ids(Path, UpdateAuthzIds, RequestorId) ->
    [ sync_authz_id(put, Path, UpdateAuthzId, RequestorId) || UpdateAuthzId <- UpdateAuthzIds ].

%% Deletes a collection of authz_ids. If there are any errors, stash the error and continue.
%% Partial deletes are OK, so we want to be greedy.
delete_authz_ids(Path, UpdateAuthzIds, RequestorId) ->
    [ sync_authz_id(delete, Path, UpdateAuthzId, RequestorId) || UpdateAuthzId <- UpdateAuthzIds ].

%% Makes the request, then injects the return status with the authz id
sync_authz_id(Method, Path, AuthzId, RequestorId) ->
    case oc_chef_authz_http:request(authz_id_path(Path, AuthzId), Method, ?DEFAULT_HEADERS, [], RequestorId) of
        ok              -> {ok, AuthzId};
        {error, Reason} -> {error, Reason, AuthzId}
    end.

authz_id_path(Path, AuthzId) ->
    Path ++ binary_to_list(AuthzId).

all_errors(Results) ->
    [ Error || {error, _, _} = Error <- Results ].

parse_binary_json(Bin) ->
    {ok, chef_json:decode_body(Bin)}.


fetch_base(#oc_chef_group{}=Record, TransformFun, CallbackFun) ->
    case chef_object_default_callbacks:fetch(Record, CallbackFun) of
        #oc_chef_group{} = GroupRecord ->
            TransformFun(GroupRecord);
        not_found ->
            not_found;
        Other ->
            Other
    end.

fetch_bare(Record, CallBackFun) ->
    fetch_base(Record, fun(x) -> x end, CallBackFun).

fetch_new(#oc_chef_group{org_id = OrgId,
                         for_requestor_id = RequestorId} = Record,
          CallbackFun) ->
    FetchMembers =
        fun(#oc_chef_group{authz_id = GroupAuthzId} = GroupRecord) ->
                %% Modking the ReaId
                ReqId = <<"mock">>,
                LookupContext = oc_chef_authz_scoped_name:initialize_context(ReqId, OrgId, CallbackFun),
                maybe_find_names(fetch_authz_ids(GroupAuthzId, RequestorId), GroupRecord, LookupContext)
        end,
    fetch_base(Record, FetchMembers, CallbackFun).

fetch(#oc_chef_group{org_id = OrgId, for_requestor_id = RequestorId} = Record, CallbackFun) ->
    case chef_object_default_callbacks:fetch(Record, CallbackFun) of
        #oc_chef_group{authz_id = GroupAuthzId} = GroupRecord ->
            %% Modking the ReaId
            ReqId = <<"mock">>,
            LookupContext = oc_chef_authz_scoped_name:initialize_context(ReqId, OrgId, CallbackFun),
            maybe_find_names(fetch_authz_ids(GroupAuthzId, RequestorId), GroupRecord, LookupContext);
        not_found ->
            not_found;
        Other ->
            Other
    end.
 
maybe_find_names(forbidden, _, _) ->
    forbidden;
maybe_find_names({ActorAuthzIds, GroupAuthzIds}, GroupRecord, Context) ->
    {ClientNames, Usernames, GroupNames} = oc_chef_authz_scoped_name:convert_ids_to_names(ActorAuthzIds, GroupAuthzIds, Context),
    Result = GroupRecord#oc_chef_group{clients = ClientNames,
                                       users =  Usernames,
                                       groups =  GroupNames,
                                       auth_side_actors = ActorAuthzIds,
                                       auth_side_groups = GroupAuthzIds},
    Result.

fetch_authz_ids(GroupAuthzId, RequestorId) ->
    Result = oc_chef_authz_http:request("/groups/" ++ binary_to_list(GroupAuthzId), get, ?DEFAULT_HEADERS, [], RequestorId),
    case Result of
        {ok, DecodedJson} ->
            {ej:get({<<"actors">>}, DecodedJson), ej:get({<<"groups">>}, DecodedJson)};
        {error, forbidden} ->
            forbidden;
        Other ->
            Other
    end.


fields_for_insert(#oc_chef_group{id = Id,
          authz_id = AuthzId,
          org_id = OrgId,
          name = Name,
          last_updated_by = LastUpdatedBy,
          created_at = CreatedAt,
          updated_at = UpdatedAt}) ->
    [Id, AuthzId, OrgId, Name, LastUpdatedBy, CreatedAt, UpdatedAt].


assemble_group_ejson(#oc_chef_group{name = Name, clients = Clients, users = Users, groups = Groups}, OrgName) ->
    {[
      {<<"actors">>, Clients ++ Users},
      {<<"users">>, Users},
      {<<"clients">>, Clients},
      {<<"groups">>, Groups},
      {<<"orgname">>, OrgName},
      {<<"name">>, Name},
      {<<"groupname">>, Name}
     ]}.

delete(ObjectRec = #oc_chef_group{last_updated_by = AuthzId, authz_id = GroupAuthzId}, CallbackFun) ->
    case oc_chef_authz:delete_resource(AuthzId, group, GroupAuthzId) of
        ok ->
            CallbackFun({delete_query(ObjectRec), [id(ObjectRec)]});
        Error ->
            Error
    end.

set_api_version(ObjectRec, Version) ->
    ObjectRec#oc_chef_group{server_api_version = Version}.
