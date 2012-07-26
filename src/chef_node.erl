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

-define(node_validation_constraints,
        [{<<"name">>,             {match_if_exists, "^[[:alnum:]_\:\.\-]+$"}},
         {<<"chef_environment">>, {match_if_exists, "^[[:alnum:]_\-]+$"}},
         {<<"json_class">>,       {match_if_exists, "Chef::Node"}},
         {<<"normal">>,           is_ejson_proplist},
         {<<"default">>,          is_ejson_proplist},
         {<<"override">>,         is_ejson_proplist},
         {<<"automatic">>,        is_ejson_proplist},
         {<<"run_list">>,         is_run_list}
        ]).

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

%%%
%%% Some of this begs to be factored out into a separate file.
%%%

%%%
%%%
%%%
-spec validate_json_node(ejson_term(), {update, binary()} | create) -> {ok, ejson_term()}.
validate_json_node(Node, {update, UrlName}) ->
    %% For update, name in URL must match name, if provided, in JSON.
    %% Missing name is ok, but name mismatch is not.
    case ej:get({<<"name">>}, Node) of
        Name when Name =:= UrlName orelse
                  Name =:= undefined ->
            WithName = ej:set({<<"name">>}, Node, UrlName),
            case chef_json_validator:validate_json_by_regex_constraints(WithName,
                                                                        ?node_validation_constraints) of
                ok -> {ok, WithName};
                Bad -> throw(Bad)
            end;
        Mismatch ->
            throw({url_json_name_mismatch, {UrlName, Mismatch, "Node"}})
    end;
validate_json_node(Node, create) ->
    case chef_json_validator:validate_json_by_regex_constraints(Node,
                                                                [<<"name">> | ?node_validation_constraints ]) of
        ok -> {ok, Node};
        Bad -> throw(Bad)
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
    %% NOTE: ejson:decode/1 will throw({invalid_json, _}) and we rely
    %% on that behavior as the calling code is only catching based on
    %% a `throw:Why' pattern.
    Json = ejson:decode(NodeBin),
    JsonFilled = insert_autofill_fields(Json),
    validate_json_node(JsonFilled, Action).
