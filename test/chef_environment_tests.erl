%% -*- erlang-indent-level: 4;indent-tabs-mode: nil; fill-column: 92-*-
%% ex: ts=4 sw=4 et
%% @author Christopher Maier <cm@opscode.com>
%% Copyright 2012 Opscode, Inc. All Rights Reserved.
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

-module(chef_environment_tests).

-include_lib("eunit/include/eunit.hrl").
-include_lib("ej/include/ej.hrl").
-include("chef_types.hrl").

new_record_test() ->
    OrgId = <<"12345678123456781234567812345678">>,
    AuthzId = <<"00000000000000000000000011111111">>,
    EnvData = {[{<<"name">>, <<"my-env">>}, {<<"alpha">>, <<"bravo">>}]},
    Env = chef_environment:new_record(OrgId, AuthzId, EnvData),
    ?assertMatch(#chef_environment{}, Env),
    %% TODO: validate more fields?
    ?assertEqual(<<"my-env">>, chef_environment:name(Env)).

