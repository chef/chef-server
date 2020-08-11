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

-module(oc_chef_cookbook_artifact).

-include("chef_types.hrl").
-include("oc_chef_types.hrl").
-include_lib("mixer/include/mixer.hrl").

-behaviour(chef_object).

-mixin([{chef_object_default_callbacks,[{fetch/2, fetch}]}]).

%% chef_object behaviour callbacks
-export([id/1,
         name/1,
         org_id/1,
         type_name/1,
         authz_id/1,
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

-export([exists_by_authz_id/1,
         filter_checksums_to_delete/2]).

id(#oc_chef_cookbook_artifact{id = Id}) ->
    Id.

name(#oc_chef_cookbook_artifact{name = Name}) ->
    Name.

org_id(#oc_chef_cookbook_artifact{org_id = OrgId}) ->
    OrgId.

type_name(#oc_chef_cookbook_artifact{}) ->
    oc_chef_cookbook_artifact.

authz_id(#oc_chef_cookbook_artifact{authz_id = AuthzId}) ->
    AuthzId.

create_query(_ObjectRec) ->
    %% created when creating a cookbook artifact version
    erlang:error(not_supported).

update_query(_ObjectRec) ->
    erlang:error(not_supported).

delete_query(_ObjectRec) ->
    erlang:error(not_supported).

find_query(_ObjectRec) ->
    find_cookbook_artifact_by_org_id_name.

list_query(_ObjectRec) ->
    erlang:error(not_supported).

bulk_get_query(_ObjectRec) ->
    erlang:error(not_supported).

is_indexed(_ObjectRec) ->
    false.

ejson_for_indexing(#oc_chef_cookbook_artifact{}, _EjsonTerm) ->
   erlang:error(not_supported).

update_from_ejson(#oc_chef_cookbook_artifact{}, _Ejson) ->
    erlang:error(not_supported).

new_record(_ApiVersion, _OrgId, _AuthzId, _Ejson) ->
    erlang:error(not_supported).

set_created(#oc_chef_cookbook_artifact{}, _ActorId) ->
    erlang:error(not_supported).

set_updated(#oc_chef_cookbook_artifact{}, _ActorId) ->
    erlang:error(not_supported).

fields_for_update(#oc_chef_cookbook_artifact{}) ->
    erlang:error(not_supported).

fields_for_fetch(#oc_chef_cookbook_artifact{org_id = OrgId,
                                            name = Name}) ->
    [OrgId, Name].

-spec list(#oc_chef_cookbook_artifact{},
           chef_object:select_callback()) -> chef_object:select_return().
list(#oc_chef_cookbook_artifact{org_id = OrgId} = CBA, CallbackFun) when is_function(CallbackFun) ->
    try CallbackFun({list_query(CBA), [OrgId], rows}) of
        Results -> Results
    catch
        error:E ->
            {error, E}
    end.

record_fields(_ApiVersion) ->
    record_info(fields, oc_chef_cookbook_artifact).

update(#oc_chef_cookbook_artifact{}, _CallbackFun) ->
    erlang:error(not_supported).

fields_for_insert(#oc_chef_cookbook_artifact{}) ->
    erlang:error(not_supported).

%% @doc Checks if a cookbook artifact with the given `AuthzId' exists
-spec exists_by_authz_id(AuthzId :: object_id()) -> boolean() | {error, _Why}.
exists_by_authz_id(AuthzId) ->
    case chef_sql:select_rows({check_cookbook_artifact_exists_by_authz_id,
                               [AuthzId]}) of
        not_found -> false;
        [[{<<"authz_id">>, AuthzId}]] -> true;
        {error, _Why} = Error -> Error
    end.

%% @doc This is meant to be called when deleting files from
%% storage after deleting or updating a cookbook version
%% This will remove from `Checksums' all those that are still
%% referenced by a cookbook artifact; and then return the list
%% of checksums that should indeed be deleted from storage.
%% Note that the reverse check (i.e. not deleting files that
%% are still referenced by a cookbook version when deleting a
%% cookbook artifact version) is handled directly in the DB:
%% https://github.com/chef/enterprise-chef-server-schema/blob/2.8.0/deploy/delete_cookbook_artifact_version.sql#L68-L72
-spec filter_checksums_to_delete(OrgId :: object_id(),
                                 Checksums :: [binary()]) -> [binary()] | {error, _Why}.
filter_checksums_to_delete(OrgId, Checksums) ->
    case sqerl:select(checksums_referenced_by_cookbook_artifact_versions,
                      [OrgId, Checksums],
                      rows_as_scalars,
                      [checksum]) of
        {ok, none} ->
            Checksums;
        {ok, ChecksumsStillReferenced} ->
            ChecksumsToDelete = sets:subtract(sets:from_list(Checksums),
                                              sets:from_list(ChecksumsStillReferenced)),
            sets:to_list(ChecksumsToDelete);
        {error, _Why} = Error ->
            Error
    end.

set_api_version(ObjectRec, Version) ->
    ObjectRec#oc_chef_cookbook_artifact{server_api_version = Version}.
