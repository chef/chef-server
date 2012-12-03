%% -*- erlang-indent-level: 4;indent-tabs-mode: nil; fill-column: 92-*-
%% ex: ts=4 sw=4 et
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


-module(chef_user_tests).

-include_lib("chef_objects/include/chef_types.hrl").
-include_lib("chef_objects/include/chef_osc_defaults.hrl").
-include_lib("ej/include/ej.hrl").
-include_lib("eunit/include/eunit.hrl").


public_key_data() ->
    {ok, Bin} = file:read_file("../test/spki_public.pem"),
    Bin.

cert_data() ->
    {ok, Bin} = file:read_file("../test/cert.pem"),
    Bin.

assemble_user_ejson_test_() ->
    [{"obtain expected EJSON",
      fun() ->
              User = #chef_user{username = <<"alice">>,
                                admin = true,
                                public_key = public_key_data()},
              {GotList} = chef_user:assemble_user_ejson(User, ?OSC_ORG_NAME),
              ExpectedData = [{<<"public_key">>, public_key_data()},
                              {<<"admin">>, true},
                              {<<"name">>, <<"alice">>}],
              ?assertEqual(lists:sort(ExpectedData), lists:sort(GotList))
      end}].

parse_binary_json_test_() ->
    [{"Can create with only name and password",
      fun() ->
              UserEjson = chef_json:encode({[
                                             {<<"name">>, <<"bob">>},
                                             {<<"password">>, <<"top secret 123456">>}
                                            ]}),
              {ok, User} = chef_user:parse_binary_json(UserEjson, create),
              ExpectedData = [{<<"password">>, <<"top secret 123456">>},
                              {<<"admin">>, false},
                              {<<"name">>, <<"bob">>}],
              {GotData} = User,
              ?assertEqual(lists:sort(ExpectedData), lists:sort(GotData))
      end
     },

     {"Password must be provided",
      fun() ->
              UserEjson = chef_json:encode({[{<<"name">>, <<"bob">>}]}),
              ?assertThrow(#ej_invalid{}, chef_user:parse_binary_json(UserEjson, create))
      end},

     {"Password must be valid", generator,
      fun() ->
              BadPass = [<<"short">>, 123, [<<"longenoughbutinalist">>], {[]}, true, null],
              Bodies = [ chef_json:encode({[{<<"name">>, <<"bob">>},
                                            {<<"password">>, P}
                                           ]})
                         || P <- BadPass ],
              [
               ?_assertThrow(#ej_invalid{},
                             chef_user:parse_binary_json(UserEjson, create))
               || UserEjson <- Bodies ]
      end},

     {"Error thrown with bad name",
      fun() ->
              Body = <<"{\"name\":\"bad~name\"}">>,
              ?assertThrow(#ej_invalid{},
                           chef_user:parse_binary_json(Body, update))
      end
     },

     {"a null public_key is removed for create",
      fun() ->
              Body = chef_json:encode({[
                                        {<<"name">>, <<"user1">>},
                                        {<<"password">>, <<"a password that is secret">>},
                                        {<<"public_key">>, null}
                                       ]}),
              {ok, Got} = chef_user:parse_binary_json(Body, create),
              ?assertEqual(undefined, ej:get({"public_key"}, Got))
      end},

     {"a null public_key is removed for update",
      fun() ->
              Body = chef_json:encode({[
                                        {<<"name">>, <<"user1">>},
                                        {<<"public_key">>, null}
                                       ]}),
              {ok, Got} = chef_user:parse_binary_json(Body, update),
              ?assertEqual(undefined, ej:get({"public_key"}, Got))
      end},

     {"a valid public_key is preserved",
      fun() ->
              Body = chef_json:encode({[
                                        {<<"name">>, <<"user1">>},
                                        {<<"password">>, <<"a password that is secret">>},
                                        {<<"public_key">>, public_key_data()}
                                       ]}),
              {ok, Got1} = chef_user:parse_binary_json(Body, create),
              ?assertEqual(public_key_data(), ej:get({"public_key"}, Got1)),
              {ok, Got2} = chef_user:parse_binary_json(Body, update),
              ?assertEqual(public_key_data(), ej:get({"public_key"}, Got2))
      end},

     {"Errors thrown for invalid public_key data ", generator,
      fun() ->
              MungedKey = re:replace(public_key_data(), "A", "2",
                                     [{return, binary}, global]),
              BadKeys = [MungedKey,
                         <<"a very bad key">>,
                         true,
                         113,
                         [public_key_data()],
                         {[]}],
              Bodies = [ chef_json:encode({[
                                            {<<"name">>, <<"client1">>},
                                            {<<"public_key">>, Key}]})
                         || Key <- BadKeys ],
              [ ?_assertThrow(#ej_invalid{},
                              chef_user:parse_binary_json(Body, update))
                || Body <- Bodies ]
      end}
    ].
