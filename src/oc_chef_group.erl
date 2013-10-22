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
         flatten/1
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
    Name = ej:get({<<"groupname">>}, GroupData),
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

update_from_ejson(#oc_chef_group{} = Group, GroupData) ->
    Name = ej:get({<<"groupname">>}, GroupData),
    Clients = ej:get({<<"clients">>}, GroupData),
    Groups = ej:get({<<"groups">>}, GroupData),
    Users = ej:get({<<"users">>}, GroupData),
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

update(#oc_chef_group{authz_id = AuthzId,
                      clients = Clients,
                      users = Users,
                      groups = Groups,
                      auth_side_actors = AuthSideActors,
                      auth_side_groups = AuthSideGroups
                     } = Record, CallbackFun) ->
    chef_object:default_update(Record, CallbackFun),
    ClientAuthzIds = find_client_authz_ids(Clients, CallbackFun),
    UserAuthzIds = find_user_authz_ids(Users, CallbackFun),
    GroupAuthzIds = find_group_authz_ids(Groups, CallbackFun),
    BasePath = "/groups/" ++ binary_to_list(AuthzId),
    ActorsPath = BasePath ++ "/actors/",
    GroupsPath = BasePath ++ "/groups/",
    UserSideActorsAuthzIds = UserAuthzIds ++ ClientAuthzIds,
    put_authz_ids(ActorsPath, UserSideActorsAuthzIds, AuthzId),
    put_authz_ids(GroupsPath, GroupAuthzIds, AuthzId),
    ActorsToRemove = default_to_empty(AuthSideActors) -- UserSideActorsAuthzIds,
    GroupsToRemove = default_to_empty(AuthSideGroups) -- GroupAuthzIds,
    delete_authz_ids(ActorsPath, ActorsToRemove, AuthzId),
    delete_authz_ids(GroupsPath, GroupsToRemove, AuthzId),
    ok.

default_to_empty(List) when is_list(List) ->
    List;
default_to_empty(_) ->
    [].

    

put_authz_ids(Path, UpdateAuthzIds, AuthzId) ->
    [oc_chef_authz_http:request(Path ++ binary_to_list(UpdateAuthzId), put, ?DEFAULT_HEADERS, [], AuthzId) || UpdateAuthzId <- UpdateAuthzIds].
    
delete_authz_ids(Path, UpdateAuthzIds, AuthzId) ->
    [oc_chef_authz_http:request(Path ++ binary_to_list(UpdateAuthzId), delete, ?DEFAULT_HEADERS, [], AuthzId) || UpdateAuthzId <- UpdateAuthzIds].
    

parse_binary_json(Bin) ->
    {ok, chef_json:decode_body(Bin)}.

fetch(Record, CallbackFun) ->
    case chef_object:default_fetch(Record, CallbackFun) of
        #oc_chef_group{authz_id = GroupAuthzId} = GroupRecord ->
            {ActorAuthzIds, GroupAuthzIds} = fetch_authz_ids(GroupAuthzId),
            {ClientNames, RemainingAuthzIds} = find_clients_names(ActorAuthzIds, CallbackFun),
            {Usernames, DefunctAuthzIds} = find_users_names(RemainingAuthzIds, CallbackFun),
            {GroupNames, DefunctGroupAuthzIds} = find_groups_names(GroupAuthzIds, CallbackFun),
            extra_mile(DefunctAuthzIds ++ DefunctGroupAuthzIds),
            {GroupRecord, ClientNames, Usernames, GroupNames};
        Error ->
            erlang:error(Error)
    end.

fetch_authz_ids(GroupAuthzId) ->
    {ok, "200", _, GroupJsonBody} = oc_chef_authz_http:request("/groups/" ++ binary_to_list(GroupAuthzId), get, ?DEFAULT_HEADERS, [], GroupAuthzId),
    DecodedJson = ejson:decode(GroupJsonBody),
    {ej:get({<<"actors">>}, DecodedJson), ej:get({<<"groups">>}, DecodedJson)}.

find_clients_names(ActorsAuthzIds, CallbackFun) ->
    query_and_diff_authz_ids(find_client_name_in_authz_ids, ActorsAuthzIds, <<"name">>, CallbackFun).

find_users_names(UsersAuthzIds, CallbackFun) ->
    query_and_diff_authz_ids(find_user_name_in_authz_ids, UsersAuthzIds, <<"username">>, CallbackFun).

find_groups_names(GroupsAuthzIds, CallbackFun) ->
    query_and_diff_authz_ids(find_group_name_in_authz_ids, GroupsAuthzIds, <<"name">>, CallbackFun).

find_client_authz_ids(ClientNames, CallbackFun) ->
    find_authz_id_in_names(find_client_authz_id_in_names, ClientNames, CallbackFun).

find_group_authz_ids(GroupNames, CallbackFun) ->
    find_authz_id_in_names(find_group_authz_id_in_names, GroupNames, CallbackFun).

find_user_authz_ids(UserNames, CallbackFun) ->
    find_authz_id_in_names(find_user_authz_id_in_names, UserNames, CallbackFun).

find_authz_id_in_names(QueryName, Names, CallbackFun) ->
    case CallbackFun({QueryName, [Names]}) of
        List when is_list(List) ->
            Flattened = lists:flatten(List),
            proplists:get_all_values(<<"authz_id">>, Flattened);
        not_found ->
            [];
        Other ->
            Other
    end.

query_and_diff_authz_ids(QueryName, AuthzIds, Key, CallbackFun) ->
    error_logger:info_msg({QueryName, AuthzIds}), 
    case CallbackFun({QueryName, [AuthzIds]}) of
        not_found ->
            {[], []};
        Results when is_list(Results)->
            Flattened = lists:flatten(Results),
            ResultNames = proplists:get_all_values(Key, Flattened),
            FoundAuthzIds = proplists:get_all_values(<<"authz_id">>, Flattened),
            {ResultNames, sets:to_list(sets:subtract(sets:from_list(AuthzIds), sets:from_list(FoundAuthzIds)))};
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
