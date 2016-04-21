%% -*- erlang-indent-level: 4;indent-tabs-mode: nil; fill-column: 92 -*-
%% ex: ts=4 sw=4 et
%% @author Tyler Cloke <tyler@chef.io>
%% Copyright 2014 Opscode, Inc. All Rights Reserved.

-module(oc_chef_org_user_association).

-include("oc_chef_types.hrl").
-include_lib("mixer/include/mixer.hrl").
-include("chef_types.hrl").

-behaviour(chef_object).

-export([
         parse_binary_json/1,
         authz_id/1,
         is_indexed/0,
         ejson_for_indexing/2,
         update_from_ejson/2,
         set_created/2,
         set_updated/2,
         flatten/1,
         delete/2,
         create_query/0,
         update_query/0,
         delete_query/0,
         find_query/0,
         list_query/0,
         ejson_from_list/1,
         bulk_get_query/0,
         fields_for_update/1,
         fields_for_fetch/1,
         record_fields/0,
         list/2,
         new_record/3,
         name/1,
         id/1,
         org_id/1,
         type_name/1
        ]).

-mixin([
        {chef_object, [{default_fetch/2, fetch},
                       {default_update/2, update}]}
       ]).

org_user_association_spec() ->
    {[
        {<<"username">>, string}
    ]}.

authz_id(#oc_chef_org_user_association{}) ->
    erlang:error(not_implemented).

is_indexed() ->
    false.

ejson_for_indexing(#oc_chef_org_user_association{}, _EjsonTerm) ->
    % An association cannot be indexed.
   erlang:error(not_indexed).

update_from_ejson(#oc_chef_org_user_association{}, _OrganizationData) ->
    % An association cannot be updated.
    erlang:error(not_implemented).

set_created(#oc_chef_org_user_association{} = Object, ActorId) ->
    Now = chef_object_base:sql_date(now),
    Object#oc_chef_org_user_association{created_at = Now, updated_at = Now, last_updated_by = ActorId}.

set_updated(#oc_chef_org_user_association{} = Object, ActorId) ->
    Now = chef_object_base:sql_date(now),
    Object#oc_chef_org_user_association{updated_at = Now, last_updated_by = ActorId}.

create_query() ->
    insert_org_user_association.

update_query() ->
    erlang:error(not_implemented).

delete_query() ->
    delete_org_user_association_by_ids.

find_query() ->
    find_org_user_association_by_ids.

list_query() ->
    erlang:error(unused).

list_query(by_org) ->
    list_org_user_associations;
list_query(by_user) ->
    list_user_org_associations.

ejson_from_list(Associations) ->
   [ {[{ <<"user">>, {[{<<"username">>, Name}]} }]} || Name <- Associations ].


flatten(#oc_chef_org_user_association{ org_id = OrgId, user_id = UserId,
                                       last_updated_by = LastUpdatedBy,
                                       created_at = CreatedAt,
                                       updated_at = UpdatedAt} ) ->
    [OrgId, UserId, LastUpdatedBy, CreatedAt, UpdatedAt].

bulk_get_query() ->
    erlang:error(not_implemented).

parse_binary_json(Bin) ->
    EJ = chef_json:decode(Bin),
    case ej:valid(org_user_association_spec(), EJ) of
        ok ->
            {ok, EJ};
    BadSpec ->
          throw(BadSpec)
    end.

fields_for_update(#oc_chef_org_user_association{}) ->
    % An association cannot be updated.
    erlang:error(not_implemented).

fields_for_fetch(#oc_chef_org_user_association{org_id = OrgId, user_id = UserId}) ->
    [OrgId, UserId].

record_fields() ->
    record_info(fields, oc_chef_org_user_association).

list(#oc_chef_org_user_association{org_id = OrgId, user_id = undefined}, CallbackFun) ->
    CallbackFun({list_query(by_org), [OrgId], [user_name]});
list(#oc_chef_org_user_association{user_id = UserId, org_id = undefined}, CallbackFun) ->
    CallbackFun({list_query(by_user), [UserId],  rows}).


% Minor hack, will revisit - not an authz id:
% Record creation via API. Temporary hack here to
% capture user id instead of  authz id, so we can
% use the existing framework.
new_record(OrgId, {authz_id, UserId},  Data) ->
    UserName = ej:get({<<"username">>}, Data),
    #oc_chef_org_user_association{org_id = OrgId,
                                  user_name = UserName,
                                  user_id = UserId};
new_record(OrgId, _AuthzId, Data) ->
    % Used for record creation during migrations -
    % user_name ignored here since it's not persisted.
    UserId = ej:get({<<"user">>}, Data),
    #oc_chef_org_user_association{org_id = OrgId,
                                  user_id = UserId}.

name(#oc_chef_org_user_association{user_name = Name}) ->
    Name.

id(#oc_chef_org_user_association{org_id = OrgId, user_id = UserId}) ->
    [OrgId, UserId].

org_id(#oc_chef_org_user_association{org_id = OrgId}) ->
    OrgId.

type_name(#oc_chef_org_user_association{}) ->
    association.

delete(#oc_chef_org_user_association{org_id = OrgId, user_id = UserId}, CallbackFun) ->
    CallbackFun({delete_query(), [OrgId, UserId]}).

