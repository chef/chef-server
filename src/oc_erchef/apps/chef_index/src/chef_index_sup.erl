%% -*- erlang-indent-level: 4;indent-tabs-mode: nil; fill-column: 92 -*-
%% ex: ts=4 sw=4 et
%% @author Seth Falcon <seth@chef.io>
%% @author Kevin Smith <kevin@chef.io>
%% Copyright 2011-2012 Opscode, Inc. All Rights Reserved.
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

-module(chef_index_sup).

-behaviour(supervisor).

%% API
-export([
         start_link/0
        ]).

%% Supervisor callbacks
-export([init/1]).

-ifdef(TEST).
-compile([export_all, nowarn_export_all]).
-endif.

-define(SERVER, ?MODULE).

%% @doc Start the chef_index_sup.
start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

init([]) ->
    error_logger:info_msg("Starting chef_index_sup.~n", []),
    error_logger:info_msg("Creating HTTP pool for Search Index.~n"),
    Provider = envy:get(chef_index, search_provider, solr, envy:one_of([solr, elasticsearch])),
    case Provider of
        elasticsearch ->
            chef_elasticsearch:declare_metrics();
        _ -> ok
    end,
    chef_index_expand:declare_metrics(),
    chef_index_http:create_pool(),
    %% TODO should we not start up batch if in inline mode?
    Children = [{chef_index_batch, {chef_index_batch, start_link, []},
                 permanent, 5000, worker, [chef_index_batch]}],
    {ok, {{one_for_one, 60, 10}, Children}}.
