%% -*- erlang-indent-level: 4;indent-tabs-mode: nil; fill-column: 92-*-
%% ex: ts=4 sw=4 et
%% @author Christopher Maier <cm@chef.io>
%% @copyright 2012 Chef Software, Inc.
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


-module(chef_role).


-include_lib("mixer/include/mixer.hrl").
-include("chef_types.hrl").

-export([
         authz_id/1,
         ejson_for_indexing/2,
         fields_for_fetch/1,
         fields_for_update/1,
         id/1,
         is_indexed/1,
         name/1,
         environments/1,
         new_record/4,
         org_id/1,
         parse_binary_json/2,
         record_fields/1,
         set_created/2,
         set_updated/2,
         set_api_version/2,
         type_name/1,
         update_from_ejson/2,
         list/2
        ]).

%% database named queries
-export([
         bulk_get_query/1,
         create_query/1,
         delete_query/1,
         find_query/1,
         list_query/1,
         update_query/1
        ]).

-mixin([{chef_object_default_callbacks, [ fetch/2, update/2 ]}]).

-ifdef(TEST).
-compile(export_all).
-endif.

-behaviour(chef_object).

-define(DEFAULT_FIELD_VALUES,
        [
         {<<"json_class">>, <<"Chef::Role">>},
         {<<"chef_type">>, <<"role">>},
         {<<"default_attributes">>,  ?EMPTY_EJSON_HASH},
         {<<"override_attributes">>,?EMPTY_EJSON_HASH},
         {<<"run_list">>, []},
         {<<"env_run_lists">>, ?EMPTY_EJSON_HASH}
        ]).

-define(VALIDATION_CONSTRAINTS,
        {[
          {<<"name">>, {string_match, chef_regex:regex_for(role_name)}},

          {<<"json_class">>, <<"Chef::Role">>},
          {<<"chef_type">>, <<"role">>},

          {<<"default_attributes">>, chef_json_validator:attribute_spec()},
          {<<"override_attributes">>, chef_json_validator:attribute_spec()},

          {<<"run_list">>, chef_json_validator:run_list_spec()},

          {<<"env_run_lists">>, chef_json_validator:env_run_lists_spec()}
         ]}).

-define(VALID_KEYS,
        [<<"chef_type">>, <<"default_attributes">>, <<"description">>,
         <<"env_run_lists">>, <<"json_class">>, <<"name">>, <<"override_attributes">>,
         <<"run_list">> ]).

-type role_action() :: create | { update, Name::binary() }.

-spec name(#chef_role{}) -> binary().
name(#chef_role{name = Name}) ->
    Name.

-spec id(#chef_role{}) -> object_id().
id(#chef_role{id = Id}) ->
    Id.

%% TODO: this doesn't need an argument
type_name(#chef_role{}) ->
    role.

-spec authz_id(#chef_role{}) -> object_id().
authz_id(#chef_role{authz_id = AuthzId}) ->
    AuthzId.

-spec org_id(#chef_role{}) -> object_id().
org_id(#chef_role{org_id = OrgId}) ->
    OrgId.

-spec new_record(api_version(), object_id(), object_id(), ejson_term()) -> #chef_role{}.
new_record(ApiVersion, OrgId, AuthzId, RoleData) ->
    Name = ej:get({<<"name">>}, RoleData),
    Id = chef_object_base:make_org_prefix_id(OrgId, Name),
    Data = chef_db_compression:compress(chef_role, chef_json:encode(RoleData)),
    #chef_role{server_api_version = ApiVersion,
               id = Id,
               authz_id = chef_object_base:maybe_stub_authz_id(AuthzId, Id),
               org_id = OrgId,
               name = Name,
               serialized_object = Data}.

is_indexed(_ObjectRec) ->
    true.

-spec ejson_for_indexing(#chef_role{}, ejson_term()) -> ejson_term().
ejson_for_indexing(#chef_role{}, Role) ->
    EnvironmentRunLists0 = ej:get({<<"env_run_lists">>}, Role, ?EMPTY_EJSON_HASH),
    EnvironmentRunLists = ej:delete({<<"_default">>}, EnvironmentRunLists0),
    ej:set({<<"env_run_lists">>}, Role, EnvironmentRunLists).

-spec update_from_ejson(#chef_role{}, ejson_term()) -> #chef_role{}.
update_from_ejson(#chef_role{} = Role, RoleData) ->
    Name = ej:get({<<"name">>}, RoleData),
    Data = chef_db_compression:compress(chef_role, chef_json:encode(RoleData)),
    Role#chef_role{name = Name, serialized_object = Data}.

%% @doc Given the EJSON representation of a role, return a sorted list of the environment names
%% present in the role's `env_run_list` hash.
-spec environments(ej:json_object()) -> [binary()].
environments(Role) ->
    {Items} = ej:get({<<"env_run_lists">>}, Role),
    %% always include the _default environment. Use usort to ensure no dups if _default
    %% actually appears as a key in the hash.
    lists:usort([ Key || {Key, _} <- [{<<"_default">>, ignored} | Items] ]).

%% @doc Convert a binary JSON string representing a Chef Role into an
%% EJson-encoded Erlang data structure.
%% @end
-spec parse_binary_json( binary(), role_action() ) -> { ok, ejson_term() }. % or throw
parse_binary_json(Bin, Action) ->
    %% TODO: invalid_json will get logged by do_malformed_request, but
    %% currently without any additional information.  Do we want to
    %% emit the JSON we recieved (size limited) or some details of the
    %% parse error from ejson if we can extract it?
    Role0 = chef_json:decode(Bin),
    Role = set_default_values(Role0),
    {ok, ValidRole} = validate_role(Role, Action),
    %% We validate then normalize, because the normalization code assumes there are valid
    %% entries to normalize in the first place.
    Normalized = normalize(ValidRole),
    {ok, Normalized}.

%% TODO: merge set_default_values and validate_role?

%% @doc If certain fields are missing from a Role, fill them in with
%% sane default values.
-spec set_default_values( ejson_term() ) -> ejson_term().
set_default_values(Role) ->
    chef_object_base:set_default_values(Role, ?DEFAULT_FIELD_VALUES).

-spec validate(jiffy:json_value()) -> {ok, jiffy:json_value()}.
validate(Role) ->
    case chef_object_base:strictly_valid(?VALIDATION_CONSTRAINTS, ?VALID_KEYS, Role) of
        ok ->
            {ok, Role};
        Bad ->
            throw(Bad)
    end.

-spec validate_role(ejson_term(), role_action()) -> {ok, ejson_term()}.
validate_role(Role, create) ->
    validate(Role);
validate_role(Role, {update, UrlName}) ->
    %% For update, name in URL must match name, if provided, in JSON.  Missing name is ok
    %% (you know the name by the URL you're hitting), but name mismatch is not.
    case ej:get({<<"name">>}, Role) of
        Name when Name =:= UrlName orelse
                  Name =:= undefined ->
            %% Go ahead and set the name, since it can only be one thing, and 'name' is a
            %% required validation
            RoleWithName = ej:set({<<"name">>}, Role, UrlName),
            validate(RoleWithName);
        Mismatch ->
            throw({url_json_name_mismatch, {UrlName, Mismatch, "Role"}})
    end.

%% @doc Normalizes the run lists of an EJson Role.  All run lists are put into canonical
%% form (all bare recipes are qualified with "recipe[...]"), and exact duplicates are
%% removed.
-spec normalize(ej:json_object()) -> ej:json_object().
normalize(RoleEjson) ->
    RunListKey = <<"run_list">>,
    EnvRunListsKey = <<"env_run_lists">>,

    RunList = ej:get({RunListKey}, RoleEjson, []),
    NormalizedRunList = chef_object_base:normalize_run_list(RunList),

    %% Roles have a hash of {environment -> run list} that need to be normalized as well.
    {EnvRunLists} = ej:get({EnvRunListsKey}, RoleEjson, ?EMPTY_EJSON_HASH),
    NormalizedEnvRunLists = {[{Env, chef_object_base:normalize_run_list(List)} || {Env, List} <- EnvRunLists]},

    lists:foldl(fun({Key, Value}, Role) ->
                        ej:set({Key}, Role, Value)
                end,
                RoleEjson,
                [{RunListKey, NormalizedRunList},
                 {EnvRunListsKey, NormalizedEnvRunLists}]).

-spec set_created(#chef_role{}, object_id()) -> #chef_role{}.
set_created(#chef_role{} = Object, ActorId) ->
    Now = chef_object_base:sql_date(now),
    Object#chef_role{created_at = Now, updated_at = Now, last_updated_by = ActorId}.

-spec set_updated(#chef_role{}, object_id()) -> #chef_role{}.
set_updated(#chef_role{} = Object, ActorId) ->
    Now = chef_object_base:sql_date(now),
    Object#chef_role{updated_at = Now, last_updated_by = ActorId}.

create_query(_ObjectRec) ->
    insert_role.

update_query(_ObjectRec) ->
    update_role_by_id.

delete_query(_ObjectRec) ->
    delete_role_by_id.

find_query(_ObjectRec) ->
    find_role_by_orgid_name.

list_query(_ObjectRec) ->
    list_roles_for_org.

bulk_get_query(_ObjectRec) ->
    bulk_get_roles.

fields_for_update(#chef_role{last_updated_by = LastUpdatedBy,
                             updated_at = UpdatedAt,
                             serialized_object = Object,
                             id = Id}) ->
    [LastUpdatedBy, UpdatedAt, Object, Id].

fields_for_fetch(#chef_role{org_id = OrgId,
                            name = Name}) ->
    [OrgId, Name].

record_fields(_ApiVersion) ->
    record_info(fields, chef_role).

list(#chef_role{org_id = OrgId} = Rec, CallbackFun) ->
    CallbackFun({list_query(Rec), [OrgId], [name]}).


set_api_version(ObjectRec, Version) ->
    ObjectRec#chef_role{server_api_version = Version}.
