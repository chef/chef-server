%% -*- erlang-indent-level: 4;indent-tabs-mode: nil; fill-column: 92 -*-
%% ex: ts=4 sw=4 et
%% @author  Marc Paradise <marc@chef.io>
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

%% @doc The internal object identifier resource.
%% This resource maps a Chef object type and name to the internal identifiers for the
%% object. It is not routed at present in OSC, but is used in OPC so that federated services
%% can track Chef objects using the internal ids.
%%
%% A request to obtain the identifiers for node web1 looks like `GET
%% /nodes/web1/_identifiers'. If the object is found, the JSON response will have the
%% following format:
%% ```
%% {
%%   "id": ID,
%%   "authz_id": AUTHZID,
%%   "org_id": ORGID
%% }
%% '''
%%
%% NOTE: only implemented for node objects
%%

-module(chef_wm_object_identifiers).

-include("oc_chef_wm.hrl").

%% Webmachine resource callbacks
-mixin([{oc_chef_wm_base, [content_types_provided/2,
                           finish_request/2,
                           ping/2,
                           validate_request/3,
                           service_available/2]}]).

-export([allowed_methods/2,
         init/1,
         init_resource_state/1,
         resource_exists/2,
         request_type/0,
         to_json/2]).

-record(object_identifier_state, {id :: object_id(),
                                  authz_id :: object_id(),
                                  org_id :: object_id()}).

init(Config) ->
    oc_chef_wm_base:init(?MODULE, Config).

init_resource_state(_Config) ->
    {ok, #object_identifier_state{}}.

request_type() ->
    "object_identifiers".

allowed_methods(Req, State) ->
    {['GET'], Req, State}.

resource_exists(Req, State) ->
    ObjType = chef_wm_util:extract_from_path(object_type, Req),
    ObjName = chef_wm_util:extract_from_path(object_name, Req),
    case object_identifiers(ObjType, ObjName, State) of
        #object_identifier_state{} = IdState ->
            State1 = State#base_state{resource_state = IdState},
            {true, Req, State1};
        Error ->
            Message = error_message(Error, ObjType, ObjName),
            Req1 = chef_wm_util:set_json_body(Req, Message),
            {false, Req1, State#base_state{log_msg = {Error, ObjType, ObjName}}}
    end.

to_json(Req, #base_state{resource_state = #object_identifier_state{
                                             id = Id,
                                             authz_id = AuthzId,
                                             org_id = OrgId} } = State) ->
    Response = {[{<<"id">>, Id}, {<<"authz_id">>, AuthzId}, {<<"org_id">>, OrgId}]},
    {chef_json:encode(Response), Req, State}.

%% Internal Functions

-spec object_identifiers(binary(), binary(), #base_state{}) -> #object_identifier_state{} |
                                                               not_found |
                                                               unknown_object_type.
object_identifiers(<<"nodes">>,  NodeName, #base_state{chef_db_context = DbContext,
                                                       organization_name = OrgName,
                                                       resource_state = ObjectIdState}) ->
    OrgId = chef_db:fetch_org_id(DbContext, OrgName),
    do_node_fetch(NodeName, OrgId, ObjectIdState, DbContext);
object_identifiers(_Type, _Name, _State) ->
    unknown_object_type.

do_node_fetch(_NodeName, not_found, _ObjectIdState, _DbContext) ->
    not_found;
do_node_fetch(NodeName, OrgId, ObjectIdState, DbContext) ->
    case chef_db:fetch(#chef_node{org_id = OrgId, name = NodeName}, DbContext) of
        not_found ->
            not_found;
        #chef_node{} = Node ->
            ObjectIdState#object_identifier_state{id = Node#chef_node.id,
                                                  authz_id = Node#chef_node.authz_id,
                                                  org_id = Node#chef_node.org_id}
    end.

error_message(not_found, ObjType, ObjName) ->
    chef_wm_util:error_message_envelope(iolist_to_binary([<<"No such '">>, ObjType,
                                                          <<"': '">>, ObjName, <<"'.">>]));
error_message(unknown_object_type, ObjType, _ObjName) ->
    chef_wm_util:error_message_envelope(iolist_to_binary([<<"Unsupported object type: '">>,
                                                          ObjType, <<"'.">>])).
