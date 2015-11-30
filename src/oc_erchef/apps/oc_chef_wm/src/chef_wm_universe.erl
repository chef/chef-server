%% -*- erlang-indent-level: 4;indent-tabs-mode: nil; fill-column: 92 -*-
%% ex: ts=4 sw=4 et
%% @author Thom May <thom@chef.io>
%% Copyright 2015 Chef Software, Inc. All Rights Reserved.
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

-module(chef_wm_universe).

-include("oc_chef_wm.hrl").

%% Webmachine resource callbacks
-mixin([{oc_chef_wm_base, [content_types_provided/2,
                           finish_request/2,
                           malformed_request/2,
                           ping/2,
                           forbidden/2,
                           is_authorized/2,
                           service_available/2]}]).

-export([allowed_methods/2,
        to_json/2 ]).

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
  {ok, #base_state{}}.

request_type() ->
  "universe".

allowed_methods(Req, State) ->
    {['GET'], Req, State}.

-spec validate_request(chef_wm:http_verb(), wm_req(), chef_wm:base_state()) ->
                              {wm_req(), chef_wm:base_state()}.
validate_request('GET', Req, #base_state{organization_guid = OrgId, server_api_version = ApiVersion} = State) ->
    {Req, State#base_state{resource_state = #oc_chef_organization{id = OrgId, server_api_version = ApiVersion}}}.

auth_info(Req, State) ->
    {{container, cookbook}, Req, State}.

to_json(Req, #base_state{chef_db_context = DbContext,
                         organization_guid = OrgId} = State) ->
  DependencyList = chef_db:fetch_all_cookbook_version_dependencies(DbContext, OrgId),
  Universe = make_universe(Req, DependencyList),
  {chef_json:encode({Universe}), Req, State}.

-spec make_dependencies(Dependencies::[ {binary(), version(), binary()}]) -> list().
make_dependencies(Dependencies) ->
    [ begin
          Spec = iolist_to_binary(io_lib:format("~s ~s", [Match, Version])),
          {Name, Spec}
      end || {Name, Version, Match} <- Dependencies].

-spec make_version_list(CookbookUrlFun::fun(),
                       Versions::[ {version(), [ {binary(), version(), binary()} ] } ]) ->
    list().
make_version_list(CookbookUrlFun, Versions) ->
    [ begin
          Deps = lists:flatten(make_dependencies(Dependencies)),
          { Version, {[
                       { <<"location_path">>, CookbookUrlFun(Version)},
                       { <<"location_type">>, <<"chef_server">>},
                       { <<"dependencies">>, {Deps}}
                      ]}
          }
      end || { Version, Dependencies} <- Versions].

-spec make_universe(Req::#wm_reqdata{}, DependencyList::[depsolver:dependency_set()]) ->
    list().
make_universe(Req, DependencyList) ->
  [ begin
        CookbookUrlFun = oc_chef_wm_routes:bulk_route_fun(cookbook_version, Name, Req),
        VersionList = make_version_list(CookbookUrlFun, Versions),
        { Name,
          { VersionList }
        }
    end || {Name, Versions} <- DependencyList].

malformed_request_message(Any, _Req, _State) ->
    error({unexpected_malformed_request_message, Any}).
