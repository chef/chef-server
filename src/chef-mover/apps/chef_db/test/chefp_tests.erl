%% -*- erlang-indent-level: 4;indent-tabs-mode: nil; fill-column: 92-*-
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
%%

-module(chefp_tests).

-include_lib("eunit/include/eunit.hrl").

batch_fold_test_() ->
    E = fun(X, Acc) -> [X | Acc] end,
    [
     {"Acc returned for empty list",
      ?_assertEqual(acc, chefp:batch_fold(E, [], acc, 2))},

     {"batch size of N", generator,
      fun() ->
              List = [1, 2 , 3],
              %%       {Size, Expected}
              Tests = [{1,    [[3], [2], [1]]},
                       {2,    [[3], [1, 2]]},
                       {3,    [[1, 2, 3]]},
                       {4,    [[1, 2, 3]]}],
              [ ?_assertEqual(Expected, chefp:batch_fold(E, List, [], N))
                || {N, Expected} <- Tests ]
      end}
    ].
