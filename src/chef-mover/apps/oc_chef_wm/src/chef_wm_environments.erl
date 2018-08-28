%% -*- erlang-indent-level: 4;indent-tabs-mode: nil; fill-column: 92 -*-
%% ex: ts=4 sw=4 et
%% @author John Keiser <jkeiser@chef.io>
%% @author Douglas Triggs
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


-module(chef_wm_environments).

-include("oc_chef_wm.hrl").

%% Webmachine resource callbacks
-mixin([{oc_chef_wm_base, [content_types_accepted/2,
                           content_types_provided/2,
                           finish_request/2,
                           malformed_request/2,
                           ping/2,
                           post_is_create/2,
                           {list_objects_json/2, to_json},
                           forbidden/2,
                           is_authorized/2,
                           service_available/2]}]).

-export([allowed_methods/2,
         conflict_message/1,
         create_path/2,
         from_json/2,
         resource_exists/2]).

%% chef_wm behavior callbacks
-behaviour(chef_wm).
-export([auth_info/2,
         init/1,
         init_resource_state/1,
         malformed_request_message/3,
         request_type/0,
         validate_request/3]).

init(Config) ->
    oc_chef_wm_base:init(?MODULE, Config).

init_resource_state(_Config) ->
    {ok, #environment_state{}}.

request_type() ->
    "environments".

allowed_methods(Req, State) ->
    {['POST', 'GET'], Req, State}.

validate_request('GET', Req, #base_state{organization_guid = OrgId} = State) ->
    %% Put a stub chef_environment record into the resource_state. This allows us to use shared
    %% code for generating the map of name => URL returned for GET /environments.  OrgId is set via
    %% malformed_request.
    {Req, State#base_state{resource_state = #chef_environment{org_id = OrgId}}};
validate_request('POST', Req, State) ->
    Body = wrq:req_body(Req),
    {ok, Environment} = chef_environment:parse_binary_json(Body),
    % POST /environments/_default can come from the org creator, in which case superuser is
    % allowed. No other endpoint gives special treatment to superuser.
    Name = ej:get({<<"name">>}, Environment),
    SuperuserBypassesChecks = case Name of
        <<"_default">> -> true;
        _ -> false
    end,
    {Req, State#base_state{superuser_bypasses_checks = SuperuserBypassesChecks,
                           resource_state =
                               #environment_state{environment_data = Environment}}}.

auth_info(Req, State) ->
    auth_info(wrq:method(Req), Req, State).

auth_info('POST', Req, State) ->
    {{create_in_container, environment}, Req, State};
auth_info('GET', Req, State) ->
    {{container, environment}, Req, State}.

resource_exists(Req, State) ->
    {true, Req, State}.

create_path(Req, #base_state{resource_state = EnvironmentState}=State) ->
    #environment_state{environment_data = EnvironmentData} = EnvironmentState,
    Name = ej:get({<<"name">>}, EnvironmentData),
    {binary_to_list(Name), Req, State}.

from_json(Req, #base_state{resource_state = EnvironmentState}=State) ->
    #environment_state{environment_data = EnvironmentData,
                       environment_authz_id = AuthzId
                      } = EnvironmentState,
    oc_chef_wm_base:create_from_json(Req, State, chef_environment, {authz_id, AuthzId},
                                  EnvironmentData).

malformed_request_message(Any, _Req, _State) ->
    error({unexpected_malformed_request_message, Any}).

-spec conflict_message(binary()) -> ejson_term().
conflict_message(_Name) ->
    {[{<<"error">>, [<<"Environment already exists">>]}]}.
