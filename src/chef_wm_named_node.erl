%% -*- erlang-indent-level: 4;indent-tabs-mode: nil; fill-column: 92 -*-
%% ex: ts=4 sw=4 et
%% @author Seth Falcon <seth@opscode.com>
%% @author Mark Mzyk <mmzyk@opscode.com>
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


-module(chef_wm_named_node).

-include("chef_wm.hrl").

-mixin([{chef_wm_base, [content_types_accepted/2,
                        content_types_provided/2,
                        finish_request/2,
                        malformed_request/2,
                        ping/2]}]).

-mixin([{?BASE_RESOURCE, [forbidden/2,
                          is_authorized/2,
                          service_available/2]}]).

%% chef_wm behavior callbacks
-behavior(chef_wm).
-export([auth_info/2,
         init/1,
         init_resource_state/1,
         malformed_request_message/3,
         request_type/0,
         validate_request/3]).

-export([allowed_methods/2,
         delete_resource/2,
         from_json/2,
         to_json/2]).

init(Config) ->
    chef_wm_base:init(?MODULE, Config).

init_resource_state(_Config) ->
    {ok, #node_state{}}.

request_type() ->
  "nodes".

allowed_methods(Req, State) ->
    {['GET', 'PUT', 'DELETE'], Req, State}.

validate_request(Method, Req, State) when Method == 'GET';
                                          Method == 'DELETE' ->
    {Req, State};
validate_request('PUT', Req, #base_state{resource_state = NodeState} = State) ->
    Name = chef_wm_util:object_name(node, Req),
    Body = wrq:req_body(Req),
    {ok, Node} = chef_node:parse_check_binary_as_json_node(Body, {update, Name}),
    {Req, State#base_state{resource_state = NodeState#node_state{node_data = Node}}}.

%% Memoize the container id so we don't hammer the database
auth_info(Req, #base_state{chef_db_context = DbContext,
                           organization_guid = OrgId,
                           resource_state = NodeState} = State) ->
    NodeName = chef_wm_util:object_name(node, Req),
    case chef_db:fetch(#chef_node{org_id = OrgId, name = NodeName}, DbContext) of
        not_found ->
            Message= chef_wm_util:not_found_message(node, NodeName),
            Req1 = chef_wm_util:set_json_body(Req, Message),
            {{halt, 404}, Req1, State#base_state{log_msg = node_not_found}};
        #chef_node{authz_id = AuthzId} = Node ->
            NodeState1 = NodeState#node_state{chef_node = Node},
            State1 = State#base_state{resource_state = NodeState1},
            {{object, AuthzId}, Req, State1}
    end.

to_json(Req, #base_state{resource_state = NodeState} = State) ->
    #node_state{chef_node = Node} = NodeState,
    {chef_db_compression:decompress(Node#chef_node.serialized_object), Req, State}.

from_json(Req, #base_state{resource_state = #node_state{chef_node = Node,
                                                        node_data = NodeData}} = State) ->
    chef_wm_base:update_from_json(Req, State, Node, NodeData).

delete_resource(Req, #base_state{chef_db_context = DbContext,
                                 requestor_id = RequestorId,
                                 resource_state = #node_state{
                                                     chef_node = Node}
                                } = State) ->

    ok = ?BASE_RESOURCE:delete_object(DbContext, Node, RequestorId),
    Json = chef_db_compression:decompress(Node#chef_node.serialized_object),
    {true, wrq:set_resp_body(Json, Req), State}.

%% Private utility functions
malformed_request_message(Any, _Req, _State) ->
    error({unexpected_malformed_request_message, Any}).
