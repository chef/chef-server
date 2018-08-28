%% -*- erlang-indent-level: 4;indent-tabs-mode: nil; fill-column: 92 -*-
%% ex: ts=4 sw=4 et
%% @author Seth Falcon <seth@chef.io>
%% Copyright 2012-2018 Chef Software, Inc.
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

-module(chef_wm_darklaunch).

-export([get_proplist/1,
         is_enabled/2]).

-ifndef(CHEF_WM_DARKLAUNCH).
is_enabled(<<"couchdb_", _Rest/binary>>, _) ->
    false;
is_enabled(<<"rabbit_aux_", _Rest/binary>>, _) ->
    false;
is_enabled(<<"query_aux_", _Rest/binary>>, _) ->
    false;
is_enabled(<<"solr4">>, _) ->
    false;
is_enabled(_, _) ->
    true.

get_proplist(_) ->
    [].

-else.
is_enabled(Feature, Darklaunch) ->
    ?CHEF_WM_DARKLAUNCH:is_enabled(Feature, Darklaunch).

get_proplist(Darklaunch) ->
    ?CHEF_WM_DARKLAUNCH:get_proplist(Darklaunch).
-endif.
