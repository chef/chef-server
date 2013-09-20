%% -*- erlang-indent-level: 4;indent-tabs-mode: nil; fill-column: 92-*-
%% ex: ts=4 sw=4 et
%% @author John Keiser <jkeiser@opscode.com>
%% @author Doug Triggs <doug@opscode.com>
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
         id/1,
         name/1,
         new_record/3,
         parse_binary_json/1,
         set_default_values/1,
         type_name/1
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
    set_default_values(Environment, ?DEFAULT_FIELD_VALUES).
set_default_values(Environment, Defaults) ->
    lists:foldl(fun({Key, Default}, Current) ->
                        case ej:get({Key}, Current) of
                            undefined ->
                                ej:set({Key}, Current, Default);
                            _ -> Current
                        end
                end,
                Environment,
                Defaults).

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
