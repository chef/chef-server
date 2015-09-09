%% -*- erlang-indent-level: 4;indent-tabs-mode: nil; fill-column: 92 -*-
%% ex: ts=4 sw=4 et
%%
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

-module(chef_index_utils).
-export([one_of/1]).

%% @doc Helper function for envy validations.
%% This feauture has been requested upstream:
%%
%% https://github.com/manderson26/envy/pull/10
%%
%% Replace with upstream function if accepted
-spec one_of(list()) -> function().
one_of(Allowed) when is_list(Allowed) ->
    fun(Item) ->
            lists:member(Item, Allowed)
    end.
