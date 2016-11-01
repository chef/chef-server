%% -*- erlang-indent-level: 4;indent-tabs-mode: nil; fill-column: 92 -*-
%% ex: ts=4 sw=4 et
%% @author Mark Anderson <mark@chef.io>
%% Copyright 2016 Chef Software, Inc. All Rights Reserved.
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

-module(chef_wm_validate).

-include("oc_chef_wm.hrl").


%% Webmachine resource callbacks
-mixin([{oc_chef_wm_base, [content_types_provided/2,
                           content_types_accepted/2,
                           finish_request/2,
                           malformed_request/2,
                           ping/2,
                           forbidden/2,
                           {allow_all/2, auth_info},
                           service_available/2]}]).

-export([allowed_methods/2,
         to_json/2,
         from_json/2]).

%% chef_wm behavior callbacks
-behaviour(chef_wm).
-export([init/1,
         init_resource_state/1,
         malformed_request_message/3,
         is_authorized/2,
         request_type/0,
         validate_request/3,
         delete_resource/2,
         delete_completed/2,
         process_post/2
        ]).


init(Config) ->
    oc_chef_wm_base:init(?MODULE, Config).

init_resource_state(_Config) ->
    {ok, #base_state{}}.

request_type() ->
  "validate".

allowed_methods(Req, State) ->
    {['GET', 'PUT', 'POST', 'DELETE'], Req, State}.

-spec validate_request(chef_wm:http_verb(), wm_req(), chef_wm:base_state()) ->
                              {wm_req(), chef_wm:base_state()}.
validate_request(_, Req, #base_state{organization_guid = OrgId, server_api_version = ApiVersion} = State) ->
    {Req, State#base_state{resource_state = #oc_chef_organization{id = OrgId, server_api_version = ApiVersion}}}.

is_authorized(Req, State) ->
    oc_chef_wm_base:is_authorized(Req, State, fun validate_extractor/3).

process_post(Req, State) ->
    {Result, Req1, State1} = to_json(Req, State),
    {true, wrq:set_resp_body(Result, Req1), State1}.

delete_resource(Req, State) ->
    make_and_set_body(Req, State).

delete_completed(Req, State) ->
    {true, Req, State}.

make_body(Req, #base_state{requestor = #chef_requestor{name = Name, type = Type}} = State) ->
    %% We may want to return other things here, possibily including a user/client GUID
    EJson = {[
              {requestor_name, Name},
              {requestor_type, Type}
             ]},
    {chef_json:encode(EJson), Req, State}.

make_and_set_body(Req, State) ->
    {EJson, Req1, State1} = make_body(Req, State),
    Req2 = wrq:set_resp_body(EJson, Req1),
    {true, Req2, State1}.

to_json(Req, State) ->
    make_body(Req, State).

from_json(Req, State) ->
    make_and_set_body(Req, State).

validate_extractor(path, Req, _State) ->
    %% wrq:disp_path/1 returns the path portion matched by "*", ie.
    %% foo/bar from /organizations/ORG/validate/foo/bar
    ShortPath = iolist_to_binary(wrq:disp_path(Req)),
    iolist_to_binary([<<"/">>, ShortPath]);
validate_extractor(Other, Req, State) ->
    oc_chef_wm_base:authorization_data_extractor(Other, Req, State).

-spec malformed_request_message(any(), wm_req(), #base_state{}) -> no_return().
malformed_request_message(Any, Req, State) ->
    oc_chef_wm_base:default_malformed_request_message(Any, Req, State).
