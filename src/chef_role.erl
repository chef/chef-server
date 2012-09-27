%% -*- erlang-indent-level: 4;indent-tabs-mode: nil; fill-column: 92-*-
%% ex: ts=4 sw=4 et
%% @author Christopher Maier <cm@opscode.com>
%% @copyright 2012 Opscode, Inc.
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


-module(chef_role).

-export([
         environments/1,
         parse_binary_json/2
        ]).

-ifdef(TEST).
-compile(export_all).
-endif.

-include("chef_types.hrl").

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

-type role_action() :: create | { update, Name::binary() }.

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
    validate_role(Role, Action).


%% TODO: merge set_default_values and validate_role?

%% @doc If certain fields are missing from a Role, fill them in with
%% sane default values.
-spec set_default_values( ejson_term() ) -> ejson_term().
set_default_values(Role) ->
    set_default_values(Role, ?DEFAULT_FIELD_VALUES).
set_default_values(Role, Defaults) ->
    lists:foldl(fun({Key, Default}, Current) ->
                        case ej:get({Key}, Current) of
                            undefined ->
                                ej:set({Key}, Current, Default);
                            _ -> Current
                        end
                end,
                Role,
                Defaults).

-spec validate(ej:ejson_object()) -> {ok, ej:ejson_object()}.
validate(Role) ->
    case ej:valid(?VALIDATION_CONSTRAINTS, Role) of
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
