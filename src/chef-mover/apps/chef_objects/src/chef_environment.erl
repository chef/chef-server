%% -*- erlang-indent-level: 4;indent-tabs-mode: nil; fill-column: 92-*-
%% ex: ts=4 sw=4 et
%% @author John Keiser <jkeiser@chef.io>
%% @author Doug Triggs <doug@chef.io>
%% Copyright 2012 Opscode, Inc. All Rights Reserved.
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


-module(chef_environment).

-export([
         authz_id/1,
         ejson_for_indexing/2,
         fields_for_fetch/1,
         fields_for_update/1,
         id/1,
         is_indexed/0,
         name/1,
         org_id/1,
         new_record/3,
         parse_binary_json/1,
         record_fields/0,
         set_created/2,
         set_default_values/1,
         set_updated/2,
         type_name/1,
         update_from_ejson/2
        ]).

%% database named queries
-export([
         bulk_get_query/0,
         create_query/0,
         delete_query/0,
         find_query/0,
         list_query/0,
         update_query/0
        ]).

-include_lib("mixer/include/mixer.hrl").
-mixin([{chef_object,[
                      {default_fetch/2, fetch},
                      {default_update/2, update}
                     ]}]).
-export([
         list/2
         ]).
-include_lib("ej/include/ej.hrl").

-include("chef_types.hrl").

-define(DEFAULT_FIELD_VALUES,
        [
         {<<"description">>, <<"">>},
         {<<"json_class">>, <<"Chef::Environment">>},
         {<<"chef_type">>, <<"environment">>},
         {<<"default_attributes">>, ?EMPTY_EJSON_HASH},
         {<<"override_attributes">>, ?EMPTY_EJSON_HASH},
         {<<"cookbook_versions">>, ?EMPTY_EJSON_HASH}
        ]).

-define(VALID_KEYS,
        [<<"name">>, <<"description">>, <<"json_class">>, <<"chef_type">>,
         <<"default_attributes">>, <<"override_attributes">>, <<"cookbook_versions">>]).

-behaviour(chef_object).

new_record(OrgId, AuthzId, EnvData) ->
    Name = ej:get({<<"name">>}, EnvData),
    Id = chef_object_base:make_org_prefix_id(OrgId, Name),
    Data = chef_db_compression:compress(chef_environment, chef_json:encode(EnvData)),
    #chef_environment{id = Id,
                      authz_id = chef_object_base:maybe_stub_authz_id(AuthzId, Id),
                      org_id = OrgId,
                      name = Name,
                      serialized_object = Data}.

id(#chef_environment{id = Id}) ->
    Id.

-spec org_id(#chef_environment{}) -> object_id().
org_id(#chef_environment{org_id = OrgId}) ->
    OrgId.

name(#chef_environment{name = Name}) ->
    Name.

type_name(#chef_environment{}) ->
    environment.

environment_spec() ->
    {[
      {<<"name">>, {string_match, chef_regex:regex_for(environment_name)}},
      {{opt, <<"description">>}, string},
      {{opt, <<"json_class">>}, <<"Chef::Environment">>},
      {{opt, <<"chef_type">>}, <<"environment">>},
      {{opt, <<"default_attributes">>}, chef_json_validator:attribute_spec()},
      {{opt, <<"override_attributes">>}, chef_json_validator:attribute_spec()},
      {{opt, <<"cookbook_versions">>}, chef_cookbook_version:constraint_map_spec(cookbook_name)}
     ]}.

%% @doc If certain fields are missing from a Environment, fill them in with
%% sane default values.
-spec set_default_values( ejson_term() ) -> ejson_term().
set_default_values(Environment) ->
    chef_object_base:set_default_values(Environment, ?DEFAULT_FIELD_VALUES).

%% @doc Convert a binary JSON string representing a Chef Environment into an
%% EJson-encoded Erlang data structure
%% @end
-spec parse_binary_json( binary() ) -> {'ok', {[{_, _}]}}. % or throw
%% The ejson_term() type is a fake out type anyhow. In this instance, Dialyzer tells us that
%% we return a more specific thing.
parse_binary_json(Bin) ->
    Environment = chef_json:decode_body(Bin),
    FinalEnvironment = set_default_values(Environment),
    validate_environment(FinalEnvironment).

validate_environment(Environment) ->
    case ej:valid(environment_spec(), Environment) of
        ok ->
            {validate_keys(Environment), Environment};
        BadSpec -> throw(BadSpec)
    end.

validate_keys([]) ->
    ok;
validate_keys({List}) when is_list(List) ->
    validate_keys(List);
validate_keys([{Item, _}|Rest]) ->
    case lists:member(Item, ?VALID_KEYS) of
        true -> validate_keys(Rest);
        _ ->
            throw({invalid_key, Item})
    end.

update_from_ejson(#chef_environment{} = Env, EnvData) ->
    Name = ej:get({<<"name">>}, EnvData),
    Data = chef_db_compression:compress(chef_environment, chef_json:encode(EnvData)),
    Env#chef_environment{name = Name, serialized_object = Data}.

is_indexed() ->
    true.

ejson_for_indexing(#chef_environment{}, Environment) ->
    Environment.

-spec authz_id(#chef_environment{}) -> object_id().
authz_id(#chef_environment{authz_id = AuthzId}) ->
    AuthzId.

-spec set_created(#chef_environment{}, object_id()) -> #chef_environment{}.
set_created(#chef_environment{} = Object, ActorId) ->
    Now = chef_object_base:sql_date(now),
    Object#chef_environment{created_at = Now, updated_at = Now, last_updated_by = ActorId}.

-spec set_updated(#chef_environment{}, object_id()) -> #chef_environment{}.
set_updated(#chef_environment{} = Object, ActorId) ->
    Now = chef_object_base:sql_date(now),
    Object#chef_environment{updated_at = Now, last_updated_by = ActorId}.

create_query() ->
    insert_environment.

update_query() ->
    update_environment_by_id.

delete_query() ->
    delete_environment_by_id.

find_query() ->
    find_environment_by_orgid_name.

list_query() ->
    list_environments_for_org.

bulk_get_query() ->
    bulk_get_environments.

fields_for_update(#chef_environment{last_updated_by = LastUpdatedBy,
                                    updated_at = UpdatedAt,
                                    name = Name,
                                    serialized_object = Object,
                                    id = Id}) ->
    [LastUpdatedBy, UpdatedAt, Name, Object, Id].

fields_for_fetch(#chef_environment{org_id = OrgId,
                                   name = Name}) ->
    [OrgId, Name].

record_fields() ->
    record_info(fields, chef_environment).

list(#chef_environment{org_id = OrgId}, CallbackFun) ->
    CallbackFun({list_query(), [OrgId], [name]}).
