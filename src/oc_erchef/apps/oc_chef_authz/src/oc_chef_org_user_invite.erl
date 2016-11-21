%% -*- erlang-indent-level: 4;indent-tabs-mode: nil; fill-column: 92 -*-
%% ex: ts=4 sw=4 et
%% @author Tyler Cloke <tyler@chef.io>
%% @author Marc Paradise <marc@chef.io>
%% Copyright 2014 Chef Software, Inc. All Rights Reserved.

-module(oc_chef_org_user_invite).

-include("oc_chef_types.hrl").
-include_lib("mixer/include/mixer.hrl").
-include("chef_types.hrl").

-behaviour(chef_object).

-export([
         parse_binary_json/2,
         authz_id/1,
         is_indexed/1,
         ejson_for_indexing/2,
         update_from_ejson/2,
         set_created/2,
         create_query/1,
         set_updated/2,
         set_api_version/2,
         update_query/1,
         delete_query/1,
         find_query/1,
         list_query/1,
         ejson_from_list/2,
         to_ejson/1,
         bulk_get_query/1,
         fields_for_fetch/1,
         fields_for_update/1,
         record_fields/1,
         list/2,
         fields_for_insert/1,
         new_record/4,
         name/1,
         id/1,
         org_id/1,
         type_name/1
        ]).

-mixin([{chef_object_default_callbacks, [fetch/2, update/2]}]).

authz_id(#oc_chef_org_user_invite{}) ->
    erlang:error(not_implemented).

valid_response(Response) when is_list(Response) ->
    valid_response(list_to_binary(Response));
valid_response(Response) when Response == <<"accept">>;
                              Response == <<"reject">> ->
    ok;
valid_response(_) ->
    error.

is_indexed(_ObjectRec) ->
    false.

ejson_from_list(Invitations, DescFieldName) ->
    [   {[{<<"id">>, InviteId}, {DescFieldName, DescFieldValue}]}  || [InviteId, DescFieldValue] <- Invitations].


% NOTE: this is not compatible with the old record, which returned a
% complete org object include all internals, the authz id of the user
% and the authz id of the inviting admin.
to_ejson(#oc_chef_org_user_invite{id = Id, user_name = UserName, org_name = OrgName}) ->
    {[{<<"id">>, Id},
      % Replacements for what we're removing, this is the data
      % we are OK exposiing
      {<<"orgname">>, OrgName},
      {<<"username">>, UserName},
      % Notify that none of these values are here anymore
      {<<"organization">>, {[{<<"deprecated">>, true}, {<<"name">>, OrgName}]}},
      {<<"user">>, <<"deprecated">>},
      {<<"organization_admin_actor_id">>, <<"deprecated">>}]}.

ejson_for_indexing(#oc_chef_org_user_invite{}, _EjsonTerm) ->
   erlang:error(not_indexed).

update_from_ejson(#oc_chef_org_user_invite{}, _OrganizationData) ->
    erlang:error(not_implemented).

set_created(#oc_chef_org_user_invite{} = Object, ActorId) ->
    Now = chef_object_base:sql_date(now),
    Object#oc_chef_org_user_invite{created_at = Now, updated_at = Now, last_updated_by = ActorId}.

set_updated(#oc_chef_org_user_invite{} = Object, ActorId) ->
    Now = chef_object_base:sql_date(now),
    Object#oc_chef_org_user_invite{updated_at = Now, last_updated_by = ActorId}.

create_query(_ObjectRec) ->
    insert_org_user_invite.

update_query(_ObjectRec) ->
    erlang:error(not_implemented).

delete_query(_ObjectRec) ->
    delete_org_user_invite_by_id.

find_query(_ObjectRec) ->
    find_org_user_invite_by_id.

list_query(_ObjectRec) ->
    erlang:error(not_implemented).

list_query(_ObjectRec, by_org) ->
    list_org_user_invites;
list_query(_ObjectRec, by_user) ->
    list_user_org_invites.

bulk_get_query(_ObjectRec) ->
    erlang:error(not_implemented).

-spec parse_binary_json(binary(), create | response) -> {ok, jiffy:json_value()}. %% or throw
parse_binary_json(Bin, Type) ->
    EJ = chef_json:decode(Bin),
    case ej:valid(validation_spec(Type), EJ) of
        ok ->
            {ok, EJ};
    BadSpec ->
          throw(BadSpec)
    end.

validation_spec(create) ->
    {[ {<<"user">>, string} ]};
validation_spec(response) ->
    {[
        {<<"response">>,{fun_match, {fun valid_response/1, string, <<"Param response must be either 'accept' or 'reject'">>}}}
    ]}.

fields_for_update(#oc_chef_org_user_invite{}) ->
    % invitations cannot be updated.
    erlang:error(not_implemented).

fields_for_fetch(#oc_chef_org_user_invite{id = Id}) ->
    [Id].

fields_for_insert(#oc_chef_org_user_invite{ id = Id,
                                  org_id = OrgId,
                                  user_id = UserId,
                                  last_updated_by = LastUpdatedBy,
                                  created_at = CreatedAt,
                                  updated_at = UpdatedAt} ) ->
    [Id, OrgId, UserId, LastUpdatedBy, CreatedAt, UpdatedAt].

record_fields(_ApiVersion) ->
    record_info(fields, oc_chef_org_user_invite).

list(#oc_chef_org_user_invite{org_id = OrgId, user_id = undefined} = Invite, CallbackFun) ->
    CallbackFun({list_query(Invite, by_org), [OrgId], rows});
list(#oc_chef_org_user_invite{user_id = UserId, org_id = undefined} = Invite, CallbackFun) ->
    CallbackFun({list_query(Invite, by_user), [UserId], rows}).

new_record(ApiVersion, OrgId, unset, Data) ->
    new_record(ApiVersion, OrgId, ej:get({<<"user">>}, Data), Data);
new_record(ApiVersion, OrgId, UserId, _Data) ->
    Id = chef_object_base:make_org_prefix_id(OrgId),
    #oc_chef_org_user_invite{server_api_version = ApiVersion,
                             id = Id,
                             org_id = OrgId,
                             user_id = UserId}.

name(#oc_chef_org_user_invite{}) ->
    erlang:error(not_implemented).

id(#oc_chef_org_user_invite{id = Id}) ->
    Id.

org_id(#oc_chef_org_user_invite{org_id = OrgId}) ->
    OrgId.

type_name(#oc_chef_org_user_invite{}) ->
    invite.

set_api_version(ObjectRec, Version) ->
    ObjectRec#oc_chef_org_user_invite{server_api_version = Version}.
