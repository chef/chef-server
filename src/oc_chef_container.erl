%% -*- erlang-indent-level: 4;indent-tabs-mode: nil; fill-column: 92 -*-
%% ex: ts=4 sw=4 et
%% @author Stephen Delano <stephen@opscode.com>
%% Copyright 2013 Opscode, Inc. All Rights Reserved.

-module(oc_chef_container).

-include("oc_chef_types.hrl").
-include_lib("mixer/include/mixer.hrl").

-behaviour(chef_object).

-export([
         assemble_container_ejson/1,
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
         update_query/0
        ]).

-mixin([
        {chef_object, [{default_fetch/2, fetch}]}
       ]).

name(#oc_chef_container{name = Name}) ->
    Name.

id(#oc_chef_container{id = Id}) ->
    Id.

type_name(#oc_chef_container{}) ->
    container.

authz_id(#oc_chef_container{authz_id = AuthzId}) ->
    AuthzId.

org_id(#oc_chef_container{org_id = OrgId}) ->
    OrgId.

create_query() ->
    insert_container.

update_query() ->
    update_container_by_id.

delete_query() ->
    delete_container_by_id.

find_query() ->
    find_container_by_orgid_name.

list_query() ->
    list_containers_for_org.

bulk_get_query() ->
    %% TODO: do we need this?
    ok.

new_record(OrgId, AuthzId, ContainerData) ->
    Name = ej:get({<<"containername">>}, ContainerData),
    Id = chef_object_base:make_org_prefix_id(OrgId, Name),
    #oc_chef_container{id = Id,
                       authz_id = chef_object_base:maybe_stub_authz_id(AuthzId, Id),
                       org_id = OrgId,
                       name = Name}.

set_created(#oc_chef_container{} = Object, ActorId) ->
    Now = chef_object_base:sql_date(now),
    Object#oc_chef_container{created_at = Now, updated_at = Now, last_updated_by = ActorId}.

set_updated(#oc_chef_container{} = Object, ActorId) ->
    Now = chef_object_base:sql_date(now),
    Object#oc_chef_container{updated_at = Now, last_updated_by = ActorId}.

is_indexed() ->
    false.

ejson_for_indexing(#oc_chef_container{}, _EjsonTerm) ->
   {[]}.

update_from_ejson(#oc_chef_container{} = Container, ContainerData) ->
    Name = ej:get({<<"containername">>}, ContainerData),
    Container#oc_chef_container{name = Name}.

fields_for_update(#oc_chef_container{last_updated_by = LastUpdatedBy,
                                     updated_at = UpdatedAt,
                                     name = Name,
                                     id = Id}) ->
    [LastUpdatedBy, UpdatedAt, Name, Id].

fields_for_fetch(#oc_chef_container{org_id = OrgId,
                                    name = Name}) ->
    [OrgId, Name].

record_fields() ->
    record_info(fields, oc_chef_container).

list(#oc_chef_container{org_id = OrgId}, CallbackFun) ->
    CallbackFun({list_query(), [OrgId], [name]}).

parse_binary_json(Bin) ->
    {ok, chef_json:decode_body(Bin)}.

assemble_container_ejson(#oc_chef_container{name = ContainerName}) ->
    {[{<<"containername">>, ContainerName},
      {<<"containerpath">>, ContainerName}]}.
