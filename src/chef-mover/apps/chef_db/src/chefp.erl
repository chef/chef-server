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

%% @doc `chefp' is a functional programming helpers module
-module(chefp).

-export([
         batch_fold/4
        ]).

%% @doc Generic batch processing as a foldl. Breaks `List' into
%% batches of size `Size' and calls `Fun(Batch, Acc)' on each batch.
-spec batch_fold(fun((T, Acc) -> Acc), List, Acc, Size) -> Acc when
      T :: any(),
      List :: [T],
      Acc :: any(),
      Size :: non_neg_integer().
batch_fold(_Fun, [], Acc, _Size) ->
    Acc;
batch_fold(Fun, List, Acc, Size) when is_list(List) ->
    batch_fold0(Fun, safe_split(Size, List), Acc, Size).

-spec batch_fold0(fun((T, Acc) -> Acc), {Batch, Rest}, Acc, Size) -> Acc when
      T :: any(),
      Batch :: [T],
      Rest :: [T],
      Acc :: any(),
      Size :: non_neg_integer().
batch_fold0(Fun, {LastBatch, []}, Acc, _Size) ->
    Fun(LastBatch, Acc);
batch_fold0(Fun, {Batch, Rest}, Acc, Size) ->
    batch_fold0(Fun, safe_split(Size, Rest), Fun(Batch, Acc), Size).

safe_split(N, L) ->
    try
        lists:split(N, L)
    catch
        error:badarg ->
            {L, []}
    end.
