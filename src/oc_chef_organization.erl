%% -*- erlang-indent-level: 4;indent-tabs-mode: nil; fill-column: 92 -*-
%% ex: ts=4 sw=4 et
%% @author Tyler Cloke <tyler@getchef.com>
%% Copyright 2014 Opscode, Inc. All Rights Reserved.

-module(oc_chef_organization).

-include("oc_chef_types.hrl").
-include_lib("mixer/include/mixer.hrl").
-include_lib("chef_objects/include/chef_types.hrl").

-behaviour(chef_object).

-export([
         authz_id/1,
         is_indexed/0,
         ejson_for_indexing/2,
         update_from_ejson/2,
         set_created/2,
         set_updated/2,
         create_query/0,
         update_query/0,
         delete_query/0,
         find_query/0,
         list_query/0,
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

authz_id(#oc_chef_organization{authz_id = AuthzId}) ->
    AuthzId.

is_indexed() ->
    false.

ejson_for_indexing(#oc_chef_organization{}, _EjsonTerm) ->
   erlang:error(not_indexed).

update_from_ejson(#oc_chef_organization{} = _Organization, _OrganizationData) ->
    erlang:error(not_implemented).

set_created(#oc_chef_organization{} = Organization, ActorId) ->
    Now = chef_object_base:sql_date(now),
    Organization#oc_chef_organization{created_at = Now, updated_at = Now, last_updated_by = ActorId}.

set_updated(#oc_chef_organization{} = Organization, ActorId) ->
    Now = chef_object_base:sql_date(now),
    Organization#oc_chef_organization{updated_at = Now, last_updated_by = ActorId}.

create_query() ->
    insert_organization.

update_query() ->
    update_organization_by_id.

delete_query() ->
    delete_organization_by_id.

find_query() ->
    find_organization_by_id.

list_query() ->
    list_organizations.

bulk_get_query() ->
    erlang:error(not_implemented).

fields_for_update(#oc_chef_organization{last_updated_by = LastUpdatedBy,
                                        updated_at = UpdatedAt,
                                        name = Name,
                                        full_name = FullName,
                                        id = Id}) ->
    [LastUpdatedBy, UpdatedAt, Name, FullName, Id].

fields_for_fetch(#oc_chef_organization{id = Id}) ->
    [Id].

record_fields() ->
    record_info(fields, oc_chef_organization).

list(#oc_chef_organization{id = Id}, CallbackFun) ->
    CallbackFun({list_query(), [Id], [name]}).

new_record(null, AuthzId, OrganizationData) ->
    % TODO: write id generator for org-less objects (see chef_object_base:make_org_prefix_id
    % and oc_chef_group:new_record for examples)
    Id = null,

    Name = ej:get({<<"name">>}, OrganizationData),
    FullName = ej:get({<<"full_name">>}, OrganizationData),
    AssignedAt = ej:get({<<"assigned_at">>}, OrganizationData),
    #oc_chef_organization{
       id = Id,
       authz_id = AuthzId,
       name = Name,
       full_name = FullName,
       assigned_at = AssignedAt
      }.

name(#oc_chef_organization{name = Name}) ->
    Name.

id(#oc_chef_organization{id = Id}) ->
    Id.

org_id(#oc_chef_organization{}) ->
    erlang:error(not_implemented).

type_name(#oc_chef_organization{}) ->
    organization.
