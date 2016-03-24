%% -*- erlang-indent-level: 4;indent-tabs-mode: nil; fill-column: 92 -*-
%% ex: ts=4 sw=4 et
%%% @author Kevin Smith
%%% @author Seth Falcon <seth@chef.io>
%%% @doc
%%% REST resource for monitoring status of erchef
%%% @end
%% Copyright 2011-2014 Chef Software, Inc. All Rights Reserved.
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

-module(chef_wm_metrics).

-ifdef(TEST).
-compile(export_all).
-endif.

-export([init/1,
         allowed_methods/2,
         content_types_provided/2,
         to_json/2]).

-include_lib("webmachine/include/webmachine.hrl").
-define(A2B(X), erlang:atom_to_binary(X, utf8)).

init(_Any) ->
    {ok, <<"{}">>}.

allowed_methods(Req, State) ->
    {['GET'], Req, State}.

content_types_provided(Req, State) ->
    {[{"application/json", to_json}], Req, State}.

to_json(Req, State) ->
    Body = check_metrics(),
    {Body, Req, State}.

%% private functions

-spec check_metrics() -> {binary()}.
check_metrics() ->
    %% Add: chef_index_http, chef_depsolver
    Pools = [sqerl, oc_chef_authz_http],
    Metrics = {get_metrics(Pools)},
    chef_json:encode(Metrics).

% get_free_members([{_,{_,free,{_,_,_}}} | T]) ->
%     get_free_members(T, 1, 1);
% get_free_members([{_,{_,_,{_,_,_}}} | T]) ->
%     get_free_members(T, 0, 1).
get_free_members([{_,{_,free,{_,_,_}}} | T], Free, Total) ->
    get_free_members(T, Free+1, Total+1);
get_free_members([{_,{_,_,{_,_,_}}} | T], Free, Total) ->
    get_free_members(T, Free, Total+1);
get_free_members([], Free, Total) -> {[{<<"free">>, list_to_binary(integer_to_list(Free))}, {<<"total">>, list_to_binary(integer_to_list(Total))}]}.

get_metrics([H | T]) ->
    PoolMembers = pooler:pool_stats(H),
    FreeMembers = [{?A2B(H), get_free_members(PoolMembers, 0, 0)}],
    get_metrics(T, FreeMembers);
get_metrics([]) -> ok.
get_metrics([H | T], Metrics) ->
    PoolMembers = pooler:pool_stats(H),
    FreeMembers = lists:append(Metrics,[{?A2B(H), get_free_members(PoolMembers, 0, 0)}]),
    get_metrics(T, FreeMembers);
get_metrics([], Metrics) -> Metrics.
