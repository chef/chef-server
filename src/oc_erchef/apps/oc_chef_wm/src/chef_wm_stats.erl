%% -*- erlang-indent-level: 4;indent-tabs-mode: nil; fill-column: 92 -*-
%% ex: ts=4 sw=4 et
%%% @doc
%%% REST resource for reporting chef server stats
%%% @end
%% Copyright 2017 Chef Software, Inc. All Rights Reserved.
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

-module(chef_wm_stats).

-ifdef(TEST).
-compile(export_all).
-compile(nowarn_export_all).
-endif.

-export([init/1,
         allowed_methods/2,
         content_types_provided/2,
         to_json/2,
         to_text/2]).

-include_lib("webmachine/include/webmachine.hrl").

init(_Any) ->
    {ok, []}.

allowed_methods(Req, State) ->
    {['GET'], Req, State}.

content_types_provided(Req, State) ->
    JsonProvider = {"application/json", to_json},
    TextProviders = [
      % I think there is a bug in webmachine where it wont allow us to use
      % 'text/plain; version=0.0.4'.
      % TODO: Understand https://github.com/basho/webmachine/blob/develop/src/webmachine_util.erl#L140-L158
      {{"text/plain",[{"version","0.0.4"}]}, to_text},
      {"text/plain", to_text}],
    case wrq:get_qs_value("format", Req) of
        undefined ->
            {[JsonProvider | TextProviders], Req, State};
        "json" ->
            {[JsonProvider], Req, State};
        "text" ->
            {TextProviders, Req, State};
        _Format ->
            %% Unknown content type requested in the query string.
            {[], Req, State}
    end.

to_json(Req, State) ->
    {chef_wm_prometheus_json_format:format(), Req, State}.

to_text(Req, State) ->
    {prometheus_text_format:format(), Req, State}.
