%% -*- erlang-indent-level: 4;indent-tabs-mode: nil; fill-column: 92 -*-
%% ex: ts=4 sw=4 et
%% @author Stephen Delano <stephen@opscode.com>
%% Copyright 2013 Opscode, Inc. All Rights Reserved.

-module(oc_chef_group).

-include("oc_chef_types.hrl").
-include_lib("mixer/include/mixer.hrl").

-behaviour(chef_object).

-define(DEFAULT_HEADERS, []).

-export([
         parse_binary_json/1,
         flatten/1,
         assemble_group_ejson/2,
         delete/2
        ]).

%% chef_object behaviour callbacks
-export([
         authz_id/1,
         bulk_get_query/0,
         create_query/0,
         delete_query/0,
         ejson_for_indexing/2,
         fields_for_fetch/1,
         fields_for_update/1,
         find_query/0,
         id/1,
         is_indexed/0,
         list/2,
         list_query/0,
         name/1,
         new_record/3,
         org_id/1,
         record_fields/0,
         set_created/2,
         set_updated/2,
         type_name/1,
         update_from_ejson/2,
         update_query/0,
         update/2,
         fetch/2
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

create_query() ->
    insert_group.

update_query() ->
    update_group_by_id.

delete_query() ->
    delete_group_by_id.

find_query() ->
    find_group_by_orgid_name.

list_query() ->
    list_groups_for_org.

bulk_get_query() ->
    %% TODO: do we need this?
    ok.

new_record(OrgId, AuthzId, GroupData) ->
    Name = ej:get({<<"id">>}, GroupData, ej:get({<<"groupname">>}, GroupData)),
    Id = chef_object_base:make_org_prefix_id(OrgId, Name),
    #oc_chef_group{id = Id,
                       authz_id = AuthzId,
                       org_id = OrgId,
                       name = Name}.

set_created(#oc_chef_group{} = Object, ActorId) ->
    Now = chef_object_base:sql_date(now),
    Object#oc_chef_group{created_at = Now, updated_at = Now, last_updated_by = ActorId}.

set_updated(#oc_chef_group{} = Object, ActorId) ->
    Now = chef_object_base:sql_date(now),
    Object#oc_chef_group{updated_at = Now, last_updated_by = ActorId}.

is_indexed() ->
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

record_fields() ->
    record_info(fields, oc_chef_group).

list(#oc_chef_group{org_id = OrgId}, CallbackFun) ->
    CallbackFun({list_query(), [OrgId], [name]}).

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
    case chef_object:default_update(Record, CallbackFun) of
        N when is_integer(N) andalso N > 0 ->
            ClientAuthzIds = find_client_authz_ids(Clients, OrgId, CallbackFun),
            UserAuthzIds = find_user_authz_ids(Users, CallbackFun),
            GroupAuthzIds = find_group_authz_ids(Groups, OrgId, CallbackFun),
            BasePath = "/groups/" ++ binary_to_list(GroupAuthzId),
            UserSideActorsAuthzIds = UserAuthzIds ++ ClientAuthzIds,
            update_bifrost_with_added_authz_ids(BasePath, UserSideActorsAuthzIds, GroupAuthzIds, AuthzId),
            ActorsToRemove = default_to_empty(AuthSideActors) -- UserSideActorsAuthzIds,
            GroupsToRemove = default_to_empty(AuthSideGroups) -- GroupAuthzIds,
            update_bifrost_with_removed_authz_ids(BasePath, ActorsToRemove, GroupsToRemove, AuthzId),
            1;
       Other  ->
            Other
    end.

update_bifrost_with_added_authz_ids(BasePath, UserSideActorsAuthzIds, GroupAuthzIds, AuthzId) ->
            ActorsPath = BasePath ++ "/actors/",
            GroupsPath = BasePath ++ "/groups/",
            put_authz_ids(ActorsPath, UserSideActorsAuthzIds, AuthzId),
            put_authz_ids(GroupsPath, GroupAuthzIds, AuthzId).

update_bifrost_with_removed_authz_ids(BasePath, ActorsToRemove, GroupsToRemove, AuthzId) ->
            ActorsPath = BasePath ++ "/actors/",
            GroupsPath = BasePath ++ "/groups/",
            delete_authz_ids(ActorsPath, ActorsToRemove, AuthzId),
            delete_authz_ids(GroupsPath, GroupsToRemove, AuthzId).


default_to_empty(List) when is_list(List) ->
    List;
default_to_empty(_) ->
    [].

    

put_authz_ids(Path, UpdateAuthzIds, AuthzId) ->
    [ok = oc_chef_authz_http:request(Path ++ binary_to_list(UpdateAuthzId), put, ?DEFAULT_HEADERS, [], AuthzId) || UpdateAuthzId <- UpdateAuthzIds].
    
delete_authz_ids(Path, UpdateAuthzIds, AuthzId) ->
    [ok = oc_chef_authz_http:request(Path ++ binary_to_list(UpdateAuthzId), delete, ?DEFAULT_HEADERS, [], AuthzId) || UpdateAuthzId <- UpdateAuthzIds].
    

parse_binary_json(Bin) ->
    {ok, chef_json:decode_body(Bin)}.

fetch(Record, CallbackFun) ->
    case chef_object:default_fetch(Record, CallbackFun) of
        #oc_chef_group{authz_id = GroupAuthzId, last_updated_by = LastUpdatedBy} = GroupRecord ->
            {ActorAuthzIds, GroupAuthzIds} = fetch_authz_ids(GroupAuthzId, LastUpdatedBy),
            {ClientNames, RemainingAuthzIds} = find_clients_names(ActorAuthzIds, CallbackFun),
            {Usernames, DefunctAuthzIds} = find_users_names(RemainingAuthzIds, CallbackFun),
            {GroupNames, DefunctGroupAuthzIds} = find_groups_names(GroupAuthzIds, CallbackFun),
            extra_mile(DefunctAuthzIds ++ DefunctGroupAuthzIds),
            Result = GroupRecord#oc_chef_group{clients = ClientNames, users =  Usernames, groups =  GroupNames, auth_side_actors = ActorAuthzIds, auth_side_groups = GroupAuthzIds},
            Result;
        not_foud ->
            not_found;
        Other ->
            Other
    end.

fetch_authz_ids(GroupAuthzId, LastUpdatedBy) ->
    {ok,  DecodedJson} = oc_chef_authz_http:request("/groups/" ++ binary_to_list(GroupAuthzId), get, ?DEFAULT_HEADERS, [], LastUpdatedBy),
    {ej:get({<<"actors">>}, DecodedJson), ej:get({<<"groups">>}, DecodedJson)}.

find_clients_names(ActorsAuthzIds, CallbackFun) ->
    query_and_diff_authz_ids(find_client_name_in_authz_ids, ActorsAuthzIds, CallbackFun).

find_users_names(UsersAuthzIds, CallbackFun) ->
    query_and_diff_authz_ids(find_user_name_in_authz_ids, UsersAuthzIds, CallbackFun).

find_groups_names(GroupsAuthzIds, CallbackFun) ->
    query_and_diff_authz_ids(find_group_name_in_authz_ids, GroupsAuthzIds, CallbackFun).

find_client_authz_ids(ClientNames, OrgId, CallbackFun) ->
    find_authz_id_in_names(find_client_authz_id_in_names, [OrgId, ClientNames], CallbackFun).

find_group_authz_ids(GroupNames, OrgId, CallbackFun) ->
    find_authz_id_in_names(find_group_authz_id_in_names, [OrgId, GroupNames], CallbackFun).

find_user_authz_ids(UserNames, CallbackFun) ->
    find_authz_id_in_names(find_user_authz_id_in_names, [UserNames], CallbackFun).

find_authz_id_in_names(QueryName, Args, CallbackFun) ->
    case CallbackFun({QueryName, Args}) of
        List when is_list(List) ->
            Flattened = lists:flatten(List),
            proplists:get_all_values(<<"authz_id">>, Flattened);
        not_found ->
            [];
        Other ->
            Other
    end.

query_and_diff_authz_ids(QueryName, AuthzIds, CallbackFun) ->
    case CallbackFun({QueryName, [AuthzIds]}) of
        not_found ->
            {[], AuthzIds};
        Results when is_list(Results)->
            {ResultNames, FoundAuthzIds} = lists:foldl(
                                             fun([{_NameKey, Name},
                                                  {<<"authz_id">>, AuthzId}],
                                                 {NamesIn, AuthzIdsIn}) ->
                                                     {[Name | NamesIn], [AuthzId | AuthzIdsIn]}
                                             end, {[],[]}, Results),
            DiffedList = sets:to_list(sets:subtract(sets:from_list(AuthzIds), sets:from_list(FoundAuthzIds))),
            {lists:reverse(ResultNames), DiffedList};
        _Other ->
            {[], []}
    end.

extra_mile(_AuthzIdsToBeDeleted) ->
    [].

flatten(#oc_chef_group{id = Id,
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
    case oc_chef_authz_http:request("/groups/" ++ binary_to_list(GroupAuthzId), delete, ?DEFAULT_HEADERS, [], AuthzId) of
        ok ->
            CallbackFun({delete_query(), [id(ObjectRec)]});
        Error ->
            Error
    end.
        

