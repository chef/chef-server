%% -*- erlang-indent-level: 4;indent-tabs-mode: nil; fill-column: 92 -*-
%% ex: ts=4 sw=4 et
%% Copyright 2013-2018 Chef Software, Inc.
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

-module(chef_json_tests).

-include_lib("eunit/include/eunit.hrl").

roundtrip_test_() ->
    [
     {"encode/decode",
      fun() ->
              EJSON = {[{<<"a">>, 1}, {<<"b">>, <<"bee">>}, {<<"c">>, [true, false]}]},
              ExpectedJson = <<"{\"a\":1,\"b\":\"bee\",\"c\":[true,false]}">>,
              GotJson = chef_json:encode(EJSON),
              ?assertEqual(ExpectedJson, GotJson),
              ?assertEqual(EJSON, chef_json:decode(GotJson))
      end},

     {"encoding/decoding bignums",
      fun() ->
              %% jiffy returns an iolist when encoding a bignum like this and this was a regression
              %% until we wrapped the call in an iolist_to_binary.
              EJSON = {[{<<"silly_size">>, 100000000000000000000}]},
              Ans = chef_json:encode(EJSON),
              Expect = <<"{\"silly_size\":100000000000000000000}">>,
              ?assertEqual(Expect, Ans),
              %% roundtrip
              ?assertEqual(EJSON, chef_json:decode(Ans))
      end}
    ].

decode_body_test() ->
    ObjectJson = <<"{\"a\":1}">>,
    ListJson = <<"[1, 2, 3]">>,
    ?assertEqual({[{<<"a">>, 1}]}, chef_json:decode_body(ObjectJson)),
    ?assertEqual([1, 2, 3], chef_json:decode(ListJson)),
    ?assertThrow(invalid_json_object, chef_json:decode_body(ListJson)).
