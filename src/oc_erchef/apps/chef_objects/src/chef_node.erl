%% -*- erlang-indent-level: 4;indent-tabs-mode: nil -*-
%% ex: ts=4 sw=4 et
%% @author Seth Falcon <seth@chef.io>
%% Copyright 2011-2018 Chef Software, Inc.
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

-include("chef_types.hrl").
-include_lib("mixer/include/mixer.hrl").

-export([
         authz_id/1,
         ejson_for_indexing/2,
         extract_recipes/1,
         extract_roles/1,
         fields_for_insert/1,
         fields_for_fetch/1,
         fields_for_update/1,
         id/1,
         is_indexed/1,
         insert_autofill_fields/1,
         name/1,
         org_id/1,
         new_record/4,
         parse_check_binary_as_json_node/2,
         record_fields/1,
         set_created/2,
         set_updated/2,
         set_api_version/2,
         type_name/1,
         update_from_ejson/2,
         validate_json_node/2,
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


-define(VALIDATION_CONSTRAINTS,
        {[{<<"name">>, {string_match, chef_regex:regex_for(node_name)}},
          {<<"chef_environment">>, {string_match, chef_regex:regex_for(environment_name)}},
          {<<"json_class">>, <<"Chef::Node">>},
          {<<"chef_type">>, <<"node">>},

          {<<"normal">>, chef_json_validator:attribute_spec()},
          {<<"default">>, chef_json_validator:attribute_spec()},
          {<<"override">>, chef_json_validator:attribute_spec()},
          {<<"automatic">>, chef_json_validator:attribute_spec()},

          {<<"run_list">>, chef_json_validator:run_list_spec()},

          {{opt, <<"policy_name">> }, {string_match, chef_regex:regex_for(policy_file_name)}},
          {{opt, <<"policy_group">> }, {string_match, chef_regex:regex_for(policy_file_name)}}
         ]}).

-define(VALID_KEYS,
        [<<"name">>, <<"chef_environment">>, <<"json_class">>, <<"chef_type">>,
         <<"normal">>, <<"default">>, <<"override">>, <<"automatic">>, <<"run_list">>,
         <<"policy_name">>, <<"policy_group">> ]).

-define(CREATE_IF_MISSING_FIELDS,
        [{<<"chef_environment">>, <<"_default">>},
         {<<"json_class">>,       <<"Chef::Node">>},
         {<<"chef_type">>,        <<"node">>},
         {<<"normal">>,           ?EMPTY_EJSON_HASH},
         {<<"default">>,          ?EMPTY_EJSON_HASH},
         {<<"override">>,         ?EMPTY_EJSON_HASH},
         {<<"automatic">>,        ?EMPTY_EJSON_HASH},
         {<<"run_list">>,         []}
        ]).

-behaviour(chef_object).

-spec new_record(api_version(),
                 OrgId :: object_id(),
                 AuthzId :: object_id() | unset,
                 ObjectEjson :: ejson_term()) ->
                        #chef_node{}.
new_record(ApiVersion, OrgId, AuthzId, NodeData) ->
    Name = ej:get({<<"name">>}, NodeData),
    Environment = ej:get({<<"chef_environment">>}, NodeData),
    PolicyName = ej:get({<<"policy_name">>}, NodeData),
    PolicyGroup = ej:get({<<"policy_group">>}, NodeData),
    Id = chef_object_base:make_org_prefix_id(OrgId, Name),
    Data = chef_db_compression:compress(chef_node, chef_json:encode(NodeData)),
    #chef_node{server_api_version = ApiVersion,
               id = Id,
               authz_id = chef_object_base:maybe_stub_authz_id(AuthzId, Id),
               org_id = OrgId,
               name = Name,
               environment = Environment,
               policy_name = PolicyName,
               policy_group = PolicyGroup,
               serialized_object = Data}.

-spec name(#chef_node{}) -> binary().
name(#chef_node{name = Name}) ->
    Name.

-spec id(#chef_node{}) -> object_id().
id(#chef_node{id = Id}) ->
    Id.

-spec org_id(#chef_node{}) -> object_id().
org_id(#chef_node{org_id = OrgId}) ->
    OrgId.

-spec type_name(#chef_node{}) -> node.
type_name(#chef_node{}) ->
    node.

-spec authz_id(#chef_node{}) -> object_id().
authz_id(#chef_node{authz_id = AuthzId}) ->
    AuthzId.

is_indexed(_ObjectRec) ->
    true.

-spec ejson_for_indexing(#chef_node{}, ejson_term()) -> ejson_term().
ejson_for_indexing(#chef_node{name = Name,
                              environment = Environment,
                              policy_name = PName,
                              policy_group = PGroup}, Node) ->
    Defaults = ej:get({<<"default">>}, Node, ?EMPTY_EJSON_HASH),
    Normal = ej:get({<<"normal">>}, Node, ?EMPTY_EJSON_HASH),
    Override = ej:get({<<"override">>}, Node, ?EMPTY_EJSON_HASH),
    %% automatic may not always be present
    Automatic = ej:get({<<"automatic">>}, Node, ?EMPTY_EJSON_HASH),
    DefaultNormal = chef_deep_merge:merge(Defaults, Normal),
    DefaultNormalOverride = chef_deep_merge:merge(DefaultNormal, Override),
    {Merged} = chef_deep_merge:merge(DefaultNormalOverride, Automatic),
    RunList = ej:get({<<"run_list">>}, Node, []),
    %% We transform to a dict to ensure we override the top-level keys
    %% with the appropriate values and don't introduce any duplicate
    %% keys
    NodeDict = dict:from_list(Merged),
    TopLevelList = [{<<"name">>, Name},
                    {<<"chef_type">>, <<"node">>},
                    %% FIXME: nodes may have environment in the db, but not in JSON
                    %% or not set at all (pre-environments nodes).
                    {<<"chef_environment">>, Environment},
                    {<<"recipe">>, extract_recipes(RunList)},
                    {<<"role">>, extract_roles(RunList)},
                    {<<"run_list">>, RunList}],
    %% Only add policy_name and policy_group key and value if
    %% value is defined for each.
    TopLevelListWithPName = case PName of
                                undefined ->
                                    TopLevelList;
                                _ ->
                                    TopLevelList ++ [{<<"policy_name">>, PName}]
                            end,
    TopLevelListFinal = case PGroup of
                            undefined ->
                                TopLevelListWithPName;
                            _ ->
                                TopLevelListWithPName ++ [{<<"policy_group">>, PGroup}]
                        end,
    TopLevelDict = dict:from_list(TopLevelListFinal),
    NodeDict1 = dict:merge(fun(_Key, TopVal, _AttrVal) ->
                                   TopVal
                           end, TopLevelDict, NodeDict),
    {dict:to_list(NodeDict1)}.

-spec update_from_ejson(#chef_node{}, ejson_term()) -> #chef_node{}.
update_from_ejson(#chef_node{} = Node, NodeJson) ->
    Name = ej:get({<<"name">>}, NodeJson),
    %% We expect that the insert_autofill_fields call will insert default when necessary
    Environment = ej:get({<<"chef_environment">>}, NodeJson),
    PolicyName = ej:get({<<"policy_name">>}, NodeJson),
    PolicyGroup = ej:get({<<"policy_group">>}, NodeJson),
    Data = chef_db_compression:compress(chef_node, chef_json:encode(NodeJson)),
    Node#chef_node{name = Name, environment = Environment, policy_name = PolicyName, policy_group = PolicyGroup, serialized_object = Data}.

-spec set_created(#chef_node{}, object_id()) -> #chef_node{}.
set_created(#chef_node{} = Object, ActorId) ->
    Now = chef_object_base:sql_date(now),
    Object#chef_node{created_at = Now, updated_at = Now, last_updated_by = ActorId}.

-spec set_updated(#chef_node{}, object_id()) -> #chef_node{}.
set_updated(#chef_node{} = Object, ActorId) ->
    Now = chef_object_base:sql_date(now),
    Object#chef_node{updated_at = Now, last_updated_by = ActorId}.

bulk_get_query(_ObjectRec) ->
    bulk_get_nodes.

create_query(_ObjectRec) ->
    insert_node.

delete_query(_ObjectRec) ->
    delete_node_by_id.

find_query(_ObjectRec) ->
    find_node_by_orgid_name.

list_query(_ObjectRec) ->
    list_nodes_for_org.

update_query(_ObjectRec) ->
    update_node_by_id.

%% This is different than the default behavior in
%% chef_object_default_callbacks:fields_for_insert, where having a value of
%% 'undefined' causes an error. In this case we _do_ want to insert NULL for
%% the policy_name and policy_group fields if they're not present, so we allow
%% 'undefined' to pass through.
fields_for_insert(Rec) ->
    [_RecName, _ApiVersion|Tail] = tuple_to_list(Rec),
    Tail.

fields_for_update(#chef_node{environment = Environment,
                             policy_name = PolicyName,
                             policy_group = PolicyGroup,
                             last_updated_by = LastUpdatedBy,
                             updated_at = UpdatedAt,
                             serialized_object = Object,
                             id = Id}) ->
    [Environment, PolicyName, PolicyGroup, LastUpdatedBy, UpdatedAt, Object, Id].

fields_for_fetch(#chef_node{org_id = OrgId,
                            name = Name}) ->
    [OrgId, Name].

record_fields(_ApiVersion) ->
    record_info(fields, chef_node).

extract_recipes(RunList) ->
    [ binary:part(Item, {0, byte_size(Item) - 1})
      || <<"recipe[", Item/binary>> <- RunList ].

extract_roles(RunList) ->
    [ binary:part(Item, {0, byte_size(Item) - 1})
      || <<"role[", Item/binary>> <- RunList ].

validate(Node) ->
    case chef_object_base:strictly_valid(?VALIDATION_CONSTRAINTS, ?VALID_KEYS, Node) of
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
    insert_autofill_fields(JsonNode, ?CREATE_IF_MISSING_FIELDS).

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
    {ok, ValidNode} = validate_json_node(JsonFilled, Action),
    %% We validate then normalize, because the normalization code assumes there are valid
    %% entries to normalize in the first place.
    Normalized = normalize(ValidNode),
    {ok, Normalized}.

%% @doc Normalizes the run list of an EJson Node, putting it into canonical form (all bare
%% recipes are qualified with "recipe[...]"), and exact duplicates are removed.
-spec normalize(ej:json_object()) -> ej:json_object().
normalize(NodeEjson) ->
    RunList = ej:get({<<"run_list">>}, NodeEjson, []),
    Normalized = chef_object_base:normalize_run_list(RunList),
    ej:set({<<"run_list">>}, NodeEjson, Normalized).


list(#chef_node{environment = undefined, org_id = OrgId}, CallbackFun) ->
    CallbackFun({list_nodes_for_org, [OrgId], [name]});
list(#chef_node{environment = EnvName, org_id = OrgId}, CallbackFun) ->
    CallbackFun({list_env_nodes_for_org, [OrgId, EnvName], [name]}).

set_api_version(ObjectRec, Version) ->
    ObjectRec#chef_node{server_api_version = Version}.
