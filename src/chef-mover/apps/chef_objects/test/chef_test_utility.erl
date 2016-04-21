%% -*- erlang-indent-level: 4;indent-tabs-mode: nil; fill-column: 92-*-
%% ex: ts=4 sw=4 et
%% @author Christopher Maier <cm@chef.io>
%%
%% @doc Broadly useful EUnit testing utilities.
%%
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

-module(chef_test_utility).

-include_lib("eunit/include/eunit.hrl").

-export([
         ejson_match/2
        ]).

%% @doc Recursively match two EJson structures, taking into account
%% nested associative structures by matching them in an
%% order-independent manner.  Non-associative structures (e.g., lists,
%% strings, binaries, etc.) are matched exactly.
%%
%% Use like you would any of the `assert' macros from EUnit; if the
%% structures do not match, an error will be thrown and the test will
%% fail.
ejson_match({Expected}, {Actual}) when is_list(Expected),
                                       is_list(Actual) ->

    ExpectedKeys = lists:sort(proplists:get_keys(Expected)),
    ActualKeys = lists:sort(proplists:get_keys(Actual)),

    ?assertEqual(ExpectedKeys, ActualKeys),

    [ ejson_match(proplists:get_value(K, Expected),
                  proplists:get_value(K, Actual)) || K <- ExpectedKeys ];
ejson_match(Expected, Actual) ->
    ?assertEqual(Expected, Actual).
