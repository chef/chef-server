%% -*- erlang-indent-level: 4; indent-tabs-mode: nil; fill-column: 80 -*-
%% ex: ts=4 sx=4 et
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
%%%-------------------------------------------------------------------
%%% @author Oliver Ferrigni <oliver@chef.io>
%%% @doc
%%%
%%% @end
%%% Created :  5 Jun 2013 by Oliver Ferrigni <oliver@chef.io>
%%%-------------------------------------------------------------------
-module(depsolver_app).

-behaviour(application).
%% API
-export([start/0, stop/0]).
%% Application callbacks
-export([start/2, stop/1]).

start()->
  start(type,args).

stop()->
  stop(state).

start(_StartType, _StartArgs) ->
    depsolver_sup:start_link().

stop(_State) ->
    ok.
