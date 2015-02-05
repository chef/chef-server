%% -*- erlang-indent-level: 4;indent-tabs-mode: nil; fill-column: 92-*-
%% ex: ts=4 sw=4 et
%% @author Marc Paradise <marc@chef.io>
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
-module(chef_key_tests).


-include_lib("eunit/include/eunit.hrl").

ejson_from_list_test_() ->
    URIDecorator = fun(Name) ->
                      iolist_to_binary(["/test/", Name])
                   end,
    [{"A valid keys input list returns expected EJ values",
      fun() ->
        EJ = chef_key:ejson_from_list(valid_keys_input_list(), URIDecorator),
        ?assertEqual(EJ, valid_keys_list_expected_ejson())
      end},
     {"An empty keys input list returns empty EJ values",
     fun() ->
        EJ = chef_key:ejson_from_list([], URIDecorator),
        ?assertEqual(EJ, [])
     end},
     {"Resulting ejson converts to json correctly",
      fun() ->
        EJ = chef_key:ejson_from_list(valid_keys_input_list(), URIDecorator),
        ?assertEqual(EJ, chef_json:encode(EJ))
      end}
    ].





valid_keys_input_list() ->
    [ [ <<"default">>, true ],
      [ <<"key1">>, false ],
      [ <<"key2">>, false ],
      [ <<"key3">>, true ] ].

valid_keys_list_expected_ejson() ->
    [ {[ { <<"uri">>, <<"/test/default">>}, {<<"name">>, <<"default">>}, {<<"expired">>, true}]},
      {[ { <<"uri">>, <<"/test/key1">>}, {<<"name">>, <<"key1">>}, {<<"expired">>, false}]},
      {[ { <<"uri">>, <<"/test/key2">>}, {<<"name">>, <<"key2">>}, {<<"expired">>, false}]},
      {[ { <<"uri">>, <<"/test/key3">>}, {<<"name">>, <<"key3">>}, {<<"expired">>, true}]} ].
