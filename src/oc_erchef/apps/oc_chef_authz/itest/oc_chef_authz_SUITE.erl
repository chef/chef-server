%% -*- erlang-indent-level: 4;indent-tabs-mode: nil; fill-column: 92 -*-
%% ex: ts=4 sw=4 et
%% @author Tyler Cloke <tyler@chef.io>
%%
%% Copyright 2013-2015 Chef, Inc. All Rights Reserved.
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

-module(oc_chef_authz_SUITE).

-include_lib("eunit/include/eunit.hrl").
-include_lib("common_test/include/ct.hrl").
-include("oc_chef_authz.hrl").
-include("oc_chef_types.hrl").
-include("server_api_version.hrl").

-compile([export_all, nowarn_export_all]).

%% Note: this is also defined in the schema test data
%% TODO: this is NOT the id of the org we make in init_per_suite/1, which is
%% probably confusing... this seems unused, delete it?
-define(ORG_ID, <<"33330000000000000000000000000000">>).

all() -> [fetch_container].

init_per_suite(LastConfig) ->
    Config = chef_test_db_helper:start_db([{app, oc_chef_authz}|LastConfig], "oc_chef_authz_itests"),
    suite_helper:start_server(Config),
    OrgsConfig = chef_test_suite_helper:make_orgs(),
    OrgsConfig ++ Config.

end_per_suite(Config) ->
    chef_test_suite_helper:stop_server(Config, suite_helper:needed_apps()).

fetch_container(_Config) ->
    ContainerName = <<"nodes">>,
    ReqId = <<"test-1-req-id">>,
    Ctx = oc_chef_authz:make_context(?API_MIN_VER, ReqId, darklaunch_stub),
    case oc_chef_authz_db:fetch_container(Ctx, ?ORG_ID, ContainerName) of
        #chef_container{name = <<"nodes">>} = C ->
            ct:pal("Found container: ~p", [C]),
            ok;
        Bad ->
            erlang:error({unexpected_result, Bad})
    end.

