%% -*- erlang-indent-level: 4;indent-tabs-mode: nil; fill-column: 92 -*-
%% ex: ts=4 sw=4 et
%% @author Stephen Delano <stephen@chef.io>
%% Copyright 2013 Opscode, Inc. All Rights Reserved.

-module(oc_chef_container).

-include("oc_chef_types.hrl").
-include_lib("mixer/include/mixer.hrl").
-include("chef_types.hrl").

-behaviour(chef_object).

-export([
         assemble_container_ejson/1,
         parse_binary_json/1
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
         update_query/1
        ]).

-mixin([{chef_object_default_callbacks, [ fetch/2, update/2 ]}]).

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

create_query(_ObjectRec) ->
    insert_container.

update_query(_ObjectRec) ->
    update_container_by_id.

delete_query(_ObjectRec) ->
    delete_container_by_id.

find_query(_ObjectRec) ->
    find_container_by_orgid_name.

list_query(_ObjectRec) ->
    list_containers_for_org.

bulk_get_query(_ObjectRec) ->
    %% TODO: do we need this?
    ok.

new_record(ApiVersion, OrgId, AuthzId, ContainerData) ->
    Name = ej:get({<<"containername">>}, ContainerData),
    Id = chef_object_base:make_org_prefix_id(OrgId, Name),
    #oc_chef_container{server_api_version = ApiVersion,
                       id = Id,
                       authz_id = AuthzId,
                       org_id = OrgId,
                       name = Name}.

set_created(#oc_chef_container{} = Object, ActorId) ->
    Now = chef_object_base:sql_date(now),
    Object#oc_chef_container{created_at = Now, updated_at = Now, last_updated_by = ActorId}.

set_updated(#oc_chef_container{} = Object, ActorId) ->
    Now = chef_object_base:sql_date(now),
    Object#oc_chef_container{updated_at = Now, last_updated_by = ActorId}.

is_indexed(_ObjectRec) ->
    false.

ejson_for_indexing(#oc_chef_container{}, _EjsonTerm) ->
   erlang:error(not_indexed).

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

record_fields(_ApiVersion) ->
    record_info(fields, oc_chef_container).

list(#oc_chef_container{org_id = OrgId} = Container, CallbackFun) ->
    CallbackFun({list_query(Container), [OrgId], [name]}).

parse_binary_json(Bin) ->
    InputEjson = chef_json:decode_body(Bin),

    %% The pedant tests explicitly state that the id should win in
    %% the case of both fields being available. This is the way that
    %% opscode-account is implemented as well. In order to be backward
    %% compatible, we'll adopt the same behavior as well, but when
    %% both 'id' and 'containername' are missing, we'll inform the user
    %% that the latter is missing. This is intended to keep the input
    %% to the API consistent with the output, which returns 'containername'
    %% as the field.
    Name = case ej:get({"id"}, InputEjson) of
               undefined ->
                   case ej:get({"containername"}, InputEjson) of
                       undefined -> throw({missing, <<"containername">>});
                       ContainerName -> ContainerName
                   end;
               ContainerId -> ContainerId
           end,

    %% validation functions return 'ok' or throw exceptions
    valid_name(Name),

    {ok, {[{<<"containername">>, Name},
           {<<"containerpath">>, Name}]}}.

assemble_container_ejson(#oc_chef_container{name = ContainerName}) ->
    {[{<<"containername">>, ContainerName},
      {<<"containerpath">>, ContainerName}]}.

%% TODO: functions from chef_regex need to get refactored out into the
%%       chef object behaviours, this is a hack and a copy of that
%%       logic, otherwise we could make the chef_regex code more generic
%%       and decouple the error messages from the regular expressions used
-define(ANCHOR_REGEX(Regex), "^" ++ Regex ++ "$").
-define(NAME_REGEX, "[.[:alnum:]_-]+").

valid_name(Name) ->
    {ok, Regex} = re:compile(?ANCHOR_REGEX(?NAME_REGEX)),
    Msg = <<"Malformed container name. Must only contain A-Z, a-z, 0-9, _, -, or .">>,
    case re:run(Name, Regex) of
        nomatch ->
            throw({bad_object_name, Name, Msg});
        _ ->
            ok
    end.

set_api_version(ObjectRec, Version) ->
    ObjectRec#oc_chef_container{server_api_version = Version}.
