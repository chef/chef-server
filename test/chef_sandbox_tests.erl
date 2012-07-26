%% -*- erlang-indent-level: 4;indent-tabs-mode: nil; fill-column: 92-*-
%% ex: ts=4 sw=4 et
%% @author Seth Falcon <seth@opscode.com>
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

validate_sandbox_test_() ->
    [
     {"Sandboxes must not be empty",
      fun() ->
              Box = {[{<<"checksums">>, {[]}}]},
              ?assertThrow({empty_checksums, <<"A sandbox must contain at least one checksum">>},
                           chef_sandbox:validate_sandbox(Box, create))
      end},

     {"Valid sandbox is ok",
      fun() ->
              Box = {[{<<"checksums">>, {make_checksums(5)}}]},
              ?assertEqual({ok, Box}, chef_sandbox:validate_sandbox(Box, create))
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
              [ ?_assertThrow({bad_checksum, <<"Invalid checksum in sandbox.">>},
                              chef_sandbox:validate_sandbox(Box, create))
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
              [ ?_assertThrow({bad_checksum, <<"Invalid checksum in sandbox.">>},
                              chef_sandbox:validate_sandbox(Box, create))
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
