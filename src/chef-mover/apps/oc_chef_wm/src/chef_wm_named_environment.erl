%% -*- erlang-indent-level: 4;indent-tabs-mode: nil; fill-column: 92-*-
%% ex: ts=4 sw=4 et
%% @author John Keiser <jkeiser@chef.io>
%% @author Douglas Triggs <doug@chef.io>
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


-module(chef_wm_named_environment).

-include("oc_chef_wm.hrl").

-mixin([{oc_chef_wm_base, [content_types_accepted/2,
                           content_types_provided/2,
                           finish_request/2,
                           malformed_request/2,
                           ping/2,
                           forbidden/2,
                           is_authorized/2,
                           service_available/2]}]).

%% chef_wm behavior callbacks
-behaviour(chef_wm).
-export([auth_info/2,
         init/1,
         init_resource_state/1,
         malformed_request_message/3,
         request_type/0,
         validate_request/3]).

-export([allowed_methods/2,
         conflict_message/1,
         delete_resource/2,
         from_json/2,
         resource_exists/2,
         to_json/2]).

init(Config) ->
    oc_chef_wm_base:init(?MODULE, Config).

init_resource_state(_Config) ->
    {ok, #environment_state{}}.

request_type() ->
    "environments".

allowed_methods(Req, State) ->
    Methods = case chef_wm_util:object_name(environment, Req) of
                  <<"_default">> ->
                      ['GET'];
                  _EnvName ->
                      ['GET', 'PUT', 'DELETE']
              end,
    Msg = <<"The '_default' environment cannot be modified.">>,
    Req1 = chef_wm_util:set_json_body(Req, {[{<<"error">>, [Msg]}]}),
    {Methods, Req1, State#base_state{resource_state = #environment_state{}}}.

validate_request('PUT', Req, State) ->
    Body = wrq:req_body(Req),
    {ok, Environment} = chef_environment:parse_binary_json(Body),
    {Req, State#base_state{resource_state =
                               #environment_state{environment_data =
                                                      Environment}}};
validate_request(_Other, Req, State) ->
    {Req, State#base_state{resource_state = #environment_state{}}}.

auth_info(Req, #base_state{chef_db_context = DbContext,
                           organization_guid = OrgId,
                           resource_state = EnvironmentState} = State) ->
    Name = chef_wm_util:object_name(environment, Req),
    case chef_db:fetch(#chef_environment{org_id = OrgId, name = Name}, DbContext) of
        not_found ->
            Message = chef_wm_util:not_found_message(environment, Name),
            Req1 = chef_wm_util:set_json_body(Req, Message),
            {{halt, 404}, Req1, State#base_state{log_msg = environment_not_found}};
        #chef_environment{authz_id = AuthzId} = Environment ->
            %% check authz here
            EnvironmentState1 = EnvironmentState#environment_state{chef_environment =
                                                                       Environment},
            State1 = State#base_state{resource_state = EnvironmentState1},
            {{object, AuthzId}, Req, State1}
    end.

resource_exists(Req, State) ->
    {true, Req, State}.

to_json(Req, #base_state{resource_state = #environment_state{
                           chef_environment = #chef_environment{
                             serialized_object = JSON}}} = State) ->
    {chef_db_compression:decompress(JSON), Req, State}.

from_json(Req, #base_state{resource_state =
                               #environment_state{chef_environment = Environment,
                                                  environment_data = EnvironmentData}} =
              State) ->
    oc_chef_wm_base:update_from_json(Req, State, Environment, EnvironmentData).

delete_resource(Req, #base_state{chef_db_context = DbContext,
                                 requestor_id = RequestorId,
                                 resource_state = #environment_state{
                                                     chef_environment = Environment}
                                } = State) ->

    ok = oc_chef_wm_base:delete_object(DbContext, Environment, RequestorId),
    Json = chef_db_compression:decompress(Environment#chef_environment.serialized_object),
    {true, wrq:set_resp_body(Json, Req), State}.

malformed_request_message(Any, _Req, _State) ->
    error({unexpected_malformed_request_message, Any}).

-spec conflict_message(binary()) -> ejson_term().
conflict_message(_Name) ->
    {[{<<"error">>, [<<"Environment already exists">>]}]}.
