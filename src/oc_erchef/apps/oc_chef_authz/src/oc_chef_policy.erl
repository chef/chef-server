%% -*- erlang-indent-level: 4;indent-tabs-mode: nil; fill-column: 92 -*-
%% ex: ts=4 sw=4 et
%% @author Oliver Ferrigni <oliver@chef.io>
%% Copyright 2012-2018 Chef Software, Inc.
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

-module(oc_chef_policy).

-include("oc_chef_types.hrl").
-include_lib("mixer/include/mixer.hrl").

-behaviour(chef_object).

-export([
         parse_binary_json/1,
         delete/2,
         create_record/4
        ]).

%% chef_object behaviour callbacks
-export([
         id/1,
         authz_id/1,
         bulk_get_query/1,
         create_query/1,
         delete_query/1,
         ejson_for_indexing/2,
         fields_for_fetch/1,
         fields_for_update/1,
         fields_for_insert/1,
         find_query/1,
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

-mixin([{chef_object_default_callbacks, [fetch/2,
                                         update/2]}]).

-define(VALIDATION_CONSTRAINTS,
        {[{<<"revision_id">>, {string_match, chef_regex:regex_for(policy_file_revision_id)}},

          {<<"name">>, {string_match, chef_regex:regex_for(policy_file_name)}},

          {<<"run_list">>, chef_json_validator:policy_run_list_spec()},

          {<<"cookbook_locks">>,
           {object_map, {
              {keys, {string_match, chef_regex:regex_for(cookbook_name)}},
              {values,
               %% apparently there's no cleaner way to do that with `ej:valid' (??)
               {fun_match, {fun valid_cookbook_lock/1, object,
                            <<"Invalid cookbook_lock constraint">>}}}}}}
         ]}).

-define(COOKBOOK_LOCK_VAIDATION_CONSTRAINTS,
        {[{<<"identifier">>,
           {string_match, chef_regex:regex_for(policy_identifier)}},

          {{opt, <<"dotted_decimal_identifier">>},
           valid_cookbook_version_spec(<<"Field 'dotted_decimal_identifier' is not a valid version">>)},

          {<<"version">>,
           valid_cookbook_version_spec(<<"Field 'version' is not a valid version">>)}]}).

valid_cookbook_version_spec(Message) ->
    {fun_match, {fun chef_cookbook_version:valid_cookbook_constraint/1, string,
                 Message}}.


valid_cookbook_lock(CookbookLockJson) ->
    case ej:valid(?COOKBOOK_LOCK_VAIDATION_CONSTRAINTS, CookbookLockJson) of
        ok -> ok;
        Bad -> throw(Bad)
    end.

id(#oc_chef_policy{id = Id}) ->
    Id.

name(#oc_chef_policy{name = Name}) ->
    Name.

org_id(#oc_chef_policy{org_id = OrgId}) ->
    OrgId.

type_name(#oc_chef_policy{}) ->
    policy.

authz_id(#oc_chef_policy{authz_id = AuthzId}) ->
    AuthzId.


create_query(_ObjectRec) ->
    insert_policy.

update_query(_ObjectRec) ->
    update_policy_by_id.

delete_query(_ObjectRec) ->
    delete_policy_by_id.

find_query(_ObjectRec) ->
    find_policy_by_orgid_name.

list_query(_ObjectRec) ->
    list_policies_for_org.

bulk_get_query(_ObjectRec) ->
    %% TODO: do we need this?
    ok.

new_record(ApiVersion, OrgId, AuthzId, Name) ->
    Id = chef_object_base:make_org_prefix_id(OrgId, Name),
    #oc_chef_policy{
        server_api_version = ApiVersion,
        id = Id,
        authz_id = AuthzId,
        org_id = OrgId,
        name = Name}.

create_record(ApiVersion, OrgId, Name, RequestingActorId) ->
    Policy = #oc_chef_policy{
                           server_api_version = ApiVersion,
                           org_id = OrgId,
                           name = Name},
    set_created(Policy, RequestingActorId).

set_created(#oc_chef_policy{} = Object, ActorId) ->
    Object#oc_chef_policy{last_updated_by = ActorId}.

set_updated(#oc_chef_policy{} = Object, ActorId) ->
    Object#oc_chef_policy{last_updated_by = ActorId}.

is_indexed(_ObjectRec) ->
    false.

ejson_for_indexing(#oc_chef_policy{}, _EjsonTerm) ->
   {[]}.

update_from_ejson(#oc_chef_policy{} = Policy, PolicyData) ->
    Name = ej:get({<<"name">>}, PolicyData, name(Policy)),
    Policy#oc_chef_policy{name = Name}.

fields_for_update(#oc_chef_policy{
                     id = Id,
                     last_updated_by = LastUpdatedBy
                                 } = Policy) ->
    [LastUpdatedBy, name(Policy), Id].


fields_for_fetch(#oc_chef_policy{org_id = OrgId} = Policy) ->
    [name(Policy), OrgId].

record_fields(_ApiVersion) ->
    record_info(fields, oc_chef_policy).

list(#oc_chef_policy{org_id = OrgId} = P, CallbackFun) ->
    CallbackFun({list_query(P), [OrgId], rows}).



parse_binary_json(Bin) ->
    Policy = chef_json:decode_body(Bin),
    case ej:valid(?VALIDATION_CONSTRAINTS, Policy) of
        ok ->
            {ok, Policy};
        Bad ->
            throw(Bad)
    end.

fields_for_insert(#oc_chef_policy{
          id = Id,
          authz_id = AuthzId,
          org_id = OrgId,
          name = Name,
          last_updated_by = LastUpdatedBy}) ->
    [Id, AuthzId, OrgId, Name, LastUpdatedBy].


delete(ObjectRec = #oc_chef_policy{id = ID}, CallbackFun) ->
    CallbackFun({delete_query(ObjectRec), [ID]}).

set_api_version(ObjectRec, Version) ->
    ObjectRec#oc_chef_policy{server_api_version = Version}.
