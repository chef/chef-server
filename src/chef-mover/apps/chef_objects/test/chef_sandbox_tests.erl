%% -*- erlang-indent-level: 4;indent-tabs-mode: nil; fill-column: 92-*-
%% ex: ts=4 sw=4 et
%% @author Seth Falcon <seth@chef.io>
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


-module(chef_sandbox_tests).

-include_lib("eunit/include/eunit.hrl").
-include_lib("ej/include/ej.hrl").
-include("chef_types.hrl").

validate_sandbox_test_() ->
    [
     {"Sandboxes must not be empty",
      fun() ->
              Box = {[{<<"checksums">>, {[]}}]},
              ?assertThrow(#ej_invalid{type=fun_match,
                                       key = <<"checksums">>,
                                       msg = <<"Bad checksums!">>},
                           chef_sandbox:validate(Box))
      end},

     {"Valid sandbox is ok",
      fun() ->
              Box = {[{<<"checksums">>, {make_checksums(5)}}]},
              ?assertEqual({ok, Box}, chef_sandbox:validate(Box))
      end},

     {"Bad checksums are not Valid", generator,
      fun() ->
              BadSums = [<<"Xaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa">>,
                         <<"aaaaaaaaaaaaaaXaaaaaaaaaaaaaaaaX">>,
                         <<"aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaX">>,
                         <<"aaaaaaaaaaaaaaaaaaaaaaaaa">>,
                         <<"aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaabbbbbbb">>],
              Locs = [first, mid, last],
              MakeSums = fun(first, Bad) ->
                                 {[Bad|make_checksums(4)]};
                            (mid, Bad) ->
                                 {make_checksums(2) ++ [Bad] ++ make_checksums(2)};
                            (last, Bad) ->
                                 {make_checksums(4) ++ [Bad]}
                         end,
              Examples = [ {[{<<"checksums">>, MakeSums(Pos, Bad)}]} ||
                             Pos <- Locs, Bad <- BadSums ],
              [ ?_assertThrow(#ej_invalid{type=fun_match,
                                          key = <<"checksums">>,
                                          msg = <<"Bad checksums!">>},
                              chef_sandbox:validate(Box))
                || Box <- Examples ]
      end},

     {"You must map checksums to null", generator,
      fun() ->
              [{C1, null}, {C2, null}, {C3, null}] = make_checksums(3),
              Sums = [
                      {[{C1, true}, {C2, null}, {C3, null}]},
                      {[{C2, null}, {C1, true}, {C3, null}]},
                      {[{C2, null}, {C3, null}, {C1, true}]}
                     ],
              Examples = [ {[{<<"checksums">>, E}]} || E <- Sums ],
              [ ?_assertThrow(#ej_invalid{type=fun_match,
                                          key = <<"checksums">>,
                                          msg = <<"Bad checksums!">>},
                              chef_sandbox:validate(Box))
                || Box <-  Examples ]
      end}
    ].


make_checksums(N) ->
    make_checksums(N, []).

make_checksums(0, Acc) ->
    Acc;
make_checksums(N, Acc) ->
    CSum = md5_to_hex(crypto:md5(crypto:rand_bytes(4))),
    make_checksums(N - 1, [{CSum, null} | Acc]).


md5_to_hex(<<X:128/big-unsigned-integer>>) ->
    iolist_to_binary(io_lib:format("~32.16.0b", [X])).

sandbox_join_rows_to_record_test_() ->
    [
     {"Condenses several sandboxed checksum rows into a single sandbox record",
      fun() ->
              ?assertEqual(chef_sandbox:sandbox_join_rows_to_record(sandbox_rows()),
                           #chef_sandbox{id = <<"deadbeefdeadbeefdeadbeefdeadbeef">>,
                                         org_id = <<"abad1deaabad1deaabad1deaabad1dea">>,
                                         created_at = {{2012,4,25},{3,7,43.0}},
                                         checksums = [
                                                      {<<"aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa">>, false},
                                                      {<<"bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb">>, true},
                                                      {<<"cccccccccccccccccccccccccccccccc">>, false},
                                                      {<<"dddddddddddddddddddddddddddddddd">>, true}
                                                     ]})
      end}
    ].
%% Sandbox Tests

sandbox_rows() ->
  [
   [{<<"sandbox_id">>, <<"deadbeefdeadbeefdeadbeefdeadbeef">>},
    {<<"org_id">>, <<"abad1deaabad1deaabad1deaabad1dea">>},
    {<<"created_at">>, {{2012,4,25},{3,7,43.0}}},
    {<<"checksum">>, <<"aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa">>},
    {<<"uploaded">>, false}],
   [{<<"sandbox_id">>, <<"deadbeefdeadbeefdeadbeefdeadbeef">>},
    {<<"org_id">>, <<"abad1deaabad1deaabad1deaabad1dea">>},
    {<<"created_at">>, {{2012,4,25},{3,7,43.0}}},
    {<<"checksum">>, <<"bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb">>},
    {<<"uploaded">>, true}],
   [{<<"sandbox_id">>, <<"deadbeefdeadbeefdeadbeefdeadbeef">>},
    {<<"org_id">>, <<"abad1deaabad1deaabad1deaabad1dea">>},
    {<<"created_at">>, {{2012,4,25},{3,7,43.0}}},
    {<<"checksum">>, <<"cccccccccccccccccccccccccccccccc">>},
    {<<"uploaded">>, false}],
   [{<<"sandbox_id">>, <<"deadbeefdeadbeefdeadbeefdeadbeef">>},
    {<<"org_id">>, <<"abad1deaabad1deaabad1deaabad1dea">>},
    {<<"created_at">>, {{2012,4,25},{3,7,43.0}}},
    {<<"checksum">>, <<"dddddddddddddddddddddddddddddddd">>},
    {<<"uploaded">>, true}]
  ].

