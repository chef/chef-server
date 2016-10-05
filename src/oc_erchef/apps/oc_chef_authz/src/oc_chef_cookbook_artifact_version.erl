%% -*- erlang-indent-level: 4;indent-tabs-mode: nil; fill-column: 92 -*-
%% ex: ts=4 sw=4 et
%% @author Jean Rouge <jean@chef.io>
%% Copyright 2015 Chef Software, Inc. Some Rights Reserved.
%%
%% This file is provided to you under the Apache License,
%% Version 2.0 (the "License"); you may not use this file
%% except in compliance with the License.  You may obtain
%% a copy of the License at
%%
%%   http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing,
%% software distributed under the License is distributed on an
%% "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
%% KIND, either express or implied.  See the License for the
%% specific language governing permissions and limitations
%% under the License.
%%

-module(oc_chef_cookbook_artifact_version).

-include("chef_types.hrl").
-include("oc_chef_types.hrl").
-include_lib("mixer/include/mixer.hrl").
-include_lib("ej/include/ej.hrl").

-behaviour(chef_object).

-mixin([{chef_object_default_callbacks, [ fetch/2 ]}]).

%% chef_object behaviour callbacks
-export([id/1,
         name/1,
         org_id/1,
         type_name/1,
         authz_id/1,
         identifier/1,
         create_query/1,
         update_query/1,
         delete_query/1,
         find_query/1,
         list_query/1,
         bulk_get_query/1,
         is_indexed/1,
         ejson_for_indexing/2,
         update_from_ejson/2,
         new_record/4,
         set_created/2,
         set_updated/2,
         set_api_version/2,
         fields_for_fetch/1,
         fields_for_update/1,
         update/2,
         list/2,
         record_fields/1,
         fields_for_insert/1]).

-export([parse_binary_json/1,
         to_json/2]).

id(#oc_chef_cookbook_artifact_version{id = Id}) ->
    Id.

name(#oc_chef_cookbook_artifact_version{name = Name}) ->
    Name.

org_id(#oc_chef_cookbook_artifact_version{org_id = OrgId}) ->
    OrgId.

type_name(#oc_chef_cookbook_artifact_version{}) ->
    cookbook_artifact_version.

authz_id(#oc_chef_cookbook_artifact_version{authz_id = AuthzId}) ->
    AuthzId.

identifier(#oc_chef_cookbook_artifact_version{identifier = Identifier}) ->
    Identifier.

create_query(_ObjectRec) ->
    insert_cookbook_artifact_version.

update_query(_ObjectRec) ->
    %% we never update a cookbook artifact version
    erlang:error(not_supported).

delete_query(_ObjectRec) ->
    delete_cookbook_artifact_version_by_id.

find_query(_ObjectRec) ->
    find_cookbook_artifact_version_by_org_name_identifier.

list_query(_ObjectRec) ->
    erlang:error(not_supported).

bulk_get_query(_ObjectRec) ->
    erlang:error(not_supported).

is_indexed(_ObjectRec) ->
    %% TODO: we propably want this to be true?
    false.

ejson_for_indexing(#oc_chef_cookbook_artifact_version{}, _EjsonTerm) ->
   erlang:error(not_supported).

-spec update_from_ejson(#oc_chef_cookbook_artifact_version{}, ejson_term()) -> #oc_chef_cookbook_artifact_version{}.
update_from_ejson(#oc_chef_cookbook_artifact_version{} = Old, CAVData) ->
    Name = ej:get({<<"name">>}, CAVData),
    Identifier = ej:get({<<"identifier">>}, CAVData),
    Metadata = compress(cookbook_artifact_metadata,
                        ej:get({<<"metadata">>}, CAVData)),
    SerializedObject = compress(chef_cookbook_artifact_version,
                        ej:delete({<<"metadata">>}, CAVData)),
    Checksums = chef_cookbook_version:extract_checksums(CAVData),

    Old#oc_chef_cookbook_artifact_version{identifier = Identifier,
                                          metadata = Metadata,
                                          serialized_object = SerializedObject,
                                          name = Name,
                                          checksums = Checksums}.

compress(Type, Data) ->
    chef_db_compression:compress(Type, chef_json:encode(Data)).

-spec new_record(api_version(), object_id(), object_id(), ejson_term()) -> #oc_chef_cookbook_artifact_version{}.
new_record(ApiVersion, OrgId, AuthzId, CAVData) ->
    Now = chef_object_base:sql_date(now),
    Base = #oc_chef_cookbook_artifact_version{server_api_version = ApiVersion,
                                              authz_id = AuthzId,
                                              org_id = OrgId,
                                              created_at = Now},
    update_from_ejson(Base, CAVData).

set_created(#oc_chef_cookbook_artifact_version{} = Object, ActorId) ->
    Object#oc_chef_cookbook_artifact_version{created_by = ActorId}.

set_updated(#oc_chef_cookbook_artifact_version{}, _ActorId) ->
    erlang:error(not_supported).

fields_for_update(#oc_chef_cookbook_artifact_version{}) ->
    erlang:error(not_supported).

fields_for_fetch(#oc_chef_cookbook_artifact_version{org_id = OrgId,
                                                    name = Name,
                                                    identifier = Identifier}) ->
    [OrgId, Name, Identifier].

list(#oc_chef_cookbook_artifact_version{}, _CallbackFun) ->
    erlang:error(not_supported).

record_fields(_ApiVersion) ->
    record_info(fields, oc_chef_cookbook_artifact_version).

update(#oc_chef_cookbook_artifact_version{}, _CallbackFun) ->
	erlang:error(not_supported).

fields_for_insert(#oc_chef_cookbook_artifact_version{} = Rec) ->
    %% doesn't have a char(32) ID like other chef objects, but
    %% rather a DB serial, so we can afford the ID to be undefined
    %% Indeed, the ID for this field is generated by the database, so we don't
    %% provide it as an input to our create query
    [oc_chef_cookbook_artifact_version, _ApiVersion, _Id | Rest] = erlang:tuple_to_list(Rec),
    Rest.

-spec parse_binary_json(binary()) -> ejson_term().
parse_binary_json(Bin) ->
    Ejson = chef_json:decode_body(Bin),
    case ej:valid(validation_constraints(), Ejson) of
        ok ->
			Ejson;
        Bad ->
            throw(Bad)
    end.

validation_constraints() ->
    {[
     %% TODO: this should be more comprehensive
     {<<"name">>, {string_match, chef_regex:regex_for(cookbook_name)}},
     {<<"identifier">>, {string_match, chef_regex:regex_for(policy_identifier)}}
    ]}.

%% @doc Re-constructs the JSON representation; can be seen as the reverse
%% operation from `update_from_ejson/2'
-spec to_json(#oc_chef_cookbook_artifact_version{}, string()) -> ejson_term().
to_json(#oc_chef_cookbook_artifact_version{metadata = RawMetadata,
                                           serialized_object = SerializedObject,
                                           org_id = OrgId},
        ExternalUrl) ->
    BaseJson = ej:set({<<"metadata">>},
                      chef_db_compression:decompress_and_decode(SerializedObject),
                      chef_db_compression:decompress_and_decode(RawMetadata)),
    JSONWithoutHacks = ej:delete({<<"json_class">>}, BaseJson),
    %% add the download URLs
    chef_cookbook_version:annotate_with_s3_urls(JSONWithoutHacks, OrgId, ExternalUrl).

set_api_version(ObjectRec, Version) ->
    ObjectRec#oc_chef_cookbook_artifact_version{server_api_version = Version}.
