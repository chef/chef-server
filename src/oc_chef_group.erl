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
         parse_binary_json/1
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
    error(not_implemented).

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
                       authz_id = chef_object_base:maybe_stub_authz_id(AuthzId, Id),
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

update_from_ejson(#oc_chef_group{}, _) ->
    error(not_implemented).

fields_for_update(#oc_chef_group{}) ->
    error(not_implemented).

fields_for_fetch(#oc_chef_group{org_id = OrgId,
                                    name = Name}) ->
    [OrgId, Name].

record_fields() ->
    record_info(fields, oc_chef_group).

list(#oc_chef_group{org_id = OrgId}, CallbackFun) ->
    CallbackFun({list_query(), [OrgId], [name]}).

update(_,_) ->
    error(not_implemented).

parse_binary_json(Bin) ->
    {ok, chef_json:decode_body(Bin)}.

fetch(#oc_chef_group{name = GroupName} = Record, CallbackFun) ->
    case chef_object:default_fetch(Record, CallbackFun) of
        #oc_chef_group{authz_id = GroupAuthzId} = GroupRecord ->
            {ActorAuthzIds, GroupAuthzIds} = fetch_authz_ids(GroupAuthzId),
            {ClientNames, RemainingAuthzIds} = find_clients_names(ActorAuthzIds),
            {Usernames, DefunctAuthzIds} = find_users_names(RemainingAuthzIds),
            GroupNames = find_groups_names(GroupAuthzIds),
            extra_mile(DefunctAuthzIds),
            {GroupRecord, ClientNames, Usernames, GroupNames};
        Error ->
            erlang:error(Error)
    end.

fetch_authz_ids(GroupAuthzId) ->
    {ok, "200", _, GroupJsonBody} = oc_chef_authz_http:request("/groups/" ++ binary_to_list(GroupAuthzId), get, ?DEFAULT_HEADERS, [], GroupAuthzId),
    DecodedJson = ejson:decode(GroupJsonBody),
    {ej:get({<<"actors">>}, DecodedJson), ej:get({<<"groups">>}, DecodedJson)}.

find_clients_names(ActorsAuthzIds) ->
    error_logger:info_msg({find_clients_names, ActorsAuthzIds}),   
    case sqerl:select(find_client_name_in_authz_ids, [ActorsAuthzIds]) of
        {ok, none} ->
            {[], []};
        {ok, Results} ->
            Flattened = lists:flatten(Results),
            ClientNames = proplists:get_all_values(<<"name">>, Flattened),
            FoundAuthzIds = proplists:get_all_values(<<"authz_id">>, Flattened),
            {ClientNames, sets:to_list(sets:subtract(sets:from_list(ActorsAuthzIds), sets:from_list(FoundAuthzIds)))};
        _Other ->
            {[], []}
    end.

find_users_names(UsersAuthzIds) ->
    error_logger:info_msg({find_users_names, UsersAuthzIds}),
    case sqerl:select(find_user_name_in_authz_ids, [UsersAuthzIds]) of
        {ok, none} ->
            {[], []};
        {ok, Results} ->
            Flattened = lists:flatten(Results),
            UserNames = proplists:get_all_values(<<"username">>, Flattened),
            FoundAuthzIds = proplists:get_all_values(<<"authz_id">>, Flattened),
            {UserNames, sets:to_list(sets:subtract(sets:from_list(UsersAuthzIds), sets:from_list(FoundAuthzIds)))};
        _Other ->
            {[], []}
    end.

find_groups_names(GroupsAuthzIds) ->
    error_logger:info_msg({find_groups_names, GroupsAuthzIds}),
    case sqerl:select(find_group_name_in_authz_ids, [GroupsAuthzIds]) of
        {ok, none} ->
            {[], []};
        {ok, Results} ->
            Flattened = lists:flatten(Results),
            GroupNames = proplists:get_all_values(<<"name">>, Flattened),
            GroupNames;
        _Other ->
            {[], []}
    end.


extra_mile(_AuthzIdsToBeDeleted) ->
    [].
