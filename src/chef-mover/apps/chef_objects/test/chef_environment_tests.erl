%% -*- erlang-indent-level: 4;indent-tabs-mode: nil; fill-column: 92-*-
%% ex: ts=4 sw=4 et
%% @author Christopher Maier <cm@chef.io>
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

update_from_ejson_test_() ->
    Env = #chef_environment{name = <<"old_name">>},
    RawEnv = {[{<<"name">>, <<"new_name">>},
                {<<"description">>, <<"environment description">>},
                {<<"json_class">>, <<"Chef::Environment">>}
               %% FIXME: fill out more compelte environment json object
               ]},
    [{"chef_environment fields are set from json for all dbs",
      [
       {atom_to_list(DbType),
        fun() ->
                Env1 = chef_object:update_from_ejson(Env, RawEnv),
                GotData = Env1#chef_environment.serialized_object,
                GotEjson = jiffy:decode(chef_db_compression:decompress(GotData)),
                ?assertEqual(<<"new_name">>, Env1#chef_environment.name),
                ?assertEqual(RawEnv, GotEjson)
        end} || DbType <- [mysql, pgsql] ]}
    ].

id_test() ->
    ?assertEqual(<<"1">>, chef_object:id(#chef_environment{id = <<"1">>})).

name_test() ->
    ?assertEqual(<<"a_name">>, chef_object:name(#chef_environment{name =  <<"a_name">>})).

type_name_test() ->
    ?assertEqual(environment, chef_object:type_name(#chef_environment{})).
