%% -*- erlang-indent-level: 4;indent-tabs-mode: nil -*-
%% ex: ts=4 sw=4 et
%% @author Seth Falcon <seth@opscode.com>
%% Copyright 2011-2012 Opscode, Inc. All Rights Reserved.
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


-module(chef_node).

-export([extract_recipes/1,
         extract_roles/1,
         validate_json_node/2,
         insert_autofill_fields/1,
         parse_check_binary_as_json_node/2]).


-include("chef_types.hrl").
-include_lib("eunit/include/eunit.hrl").

-define(VALIDATION_CONSTRAINTS,
        {[{<<"name">>, {string_match, chef_regex:regex_for(node_name)}},
          {<<"chef_environment">>, {string_match, chef_regex:regex_for(environment_name)}},
          {<<"json_class">>, <<"Chef::Node">>},
          {<<"chef_type">>, <<"node">>},

          {<<"normal">>, chef_json_validator:attribute_spec()},
          {<<"default">>, chef_json_validator:attribute_spec()},
          {<<"override">>, chef_json_validator:attribute_spec()},
          {<<"automatic">>, chef_json_validator:attribute_spec()},

          {<<"run_list">>, chef_json_validator:run_list_spec()}
         ]}).

-define(create_if_missing_fields,
        [{<<"chef_environment">>, <<"_default">>},
         {<<"json_class">>,       <<"Chef::Node">>},
         {<<"chef_type">>,        <<"node">>},
         {<<"normal">>,           ?EMPTY_EJSON_HASH},
         {<<"default">>,          ?EMPTY_EJSON_HASH},
         {<<"override">>,         ?EMPTY_EJSON_HASH},
         {<<"automatic">>,        ?EMPTY_EJSON_HASH},
         {<<"run_list">>,         []}
        ]).

extract_recipes(RunList) ->
    [ binary:part(Item, {0, byte_size(Item) - 1})
      || <<"recipe[", Item/binary>> <- RunList ].

extract_roles(RunList) ->
    [ binary:part(Item, {0, byte_size(Item) - 1})
      || <<"role[", Item/binary>> <- RunList ].

validate(Node) ->
    case ej:valid(?VALIDATION_CONSTRAINTS, Node) of
        ok ->
            {ok, Node};
        Bad ->
            throw(Bad)
    end.

-spec validate_json_node(ejson_term(), {update, binary()} | create) -> {ok, ejson_term()}.
validate_json_node(Node, create) ->
    validate(Node);
validate_json_node(Node, {update, UrlName}) ->
    %% For update, name in URL must match name, if provided, in JSON.
    %% Missing name is ok, but name mismatch is not.
    case ej:get({<<"name">>}, Node) of
        Name when Name =:= UrlName orelse
                  Name =:= undefined ->
            WithName = ej:set({<<"name">>}, Node, UrlName),
            validate(WithName);
        Mismatch ->
            throw({url_json_name_mismatch, {UrlName, Mismatch, "Node"}})
    end.

insert_autofill_fields(JsonNode) ->
    insert_autofill_fields(JsonNode, ?create_if_missing_fields).

%% TODO this also looks like a good candidate for move into
%% chef_json_validator
insert_autofill_fields(JsonNode, Fields) ->
    lists:foldl(
      fun({Key, Default}, CurNode) ->
              case ej:get({Key}, CurNode) of
                  undefined ->
                      ej:set({Key}, CurNode, Default);
                  _ -> CurNode
              end
      end,
      JsonNode,
      Fields).

-spec parse_check_binary_as_json_node(binary(), {update, binary()} | create) ->
                                             {ok, ejson_term()}.
%% @doc Parses, validates, and normalizes node JSON binary.
parse_check_binary_as_json_node(NodeBin, Action) ->
    %% NOTE: chef_json:decode/1 will throw({invalid_json, _}) and we rely
    %% on that behavior as the calling code is only catching based on
    %% a `throw:Why' pattern.
    Json = chef_json:decode(NodeBin),
    JsonFilled = insert_autofill_fields(Json),
    validate_json_node(JsonFilled, Action).
