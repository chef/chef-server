%% -*- erlang-indent-level: 4;indent-tabs-mode: nil; fill-column: 92 -*-
%% ex: ts=4 sw=4 et
%% @author Mark Anderson <mark@chef.io>
%% @version 0.0.1
%% @end
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

-module(oc_chef_authz_test_utils).

-export([
         test_setup/0,
         automeck_file/2,
         read_file/1
        ]).

-include_lib("eunit/include/eunit.hrl").

%% a fake URL for setting up the connection pool. We rely on an implementation detail that
%% no connection is attempted until a request is made and we mock out that part of things in
%% the tests.
-define(pool_opts, [{root_url, "http://oc_chef_authz.localhost:5121"},
                    {max_count, 1},
                    {init_count, 1}]).

test_setup() ->
    application:set_env(oc_chef_authz, http_pool, [{oc_chef_authz_test_pool, ?pool_opts}]),
    Server = {context,<<"test-req-id">>,{server,"localhost",5984,[],[]}},
    Superuser = <<"cb4dcaabd91a87675a14ec4f4a00050d">>,
    {Server, Superuser}.

automeck_file(Module, TestName) ->
    FileName = filename(Module, TestName),
    AppFile = automeck_file_(app, FileName),
    case filelib:is_file(AppFile) of
        true ->
            AppFile;
        false ->
            automeck_file_(test, FileName)
    end.

automeck_file_(app, FileName) -> %% Rebar3
    filename:join([".", "apps", "oc_chef_authz", "test", "automeck_config", FileName]);
automeck_file_(test, FileName) -> %% Rebar2
    filename:join(["..", "test", "automeck_config", FileName]).

filename(Module, TestName) ->
    atom_to_list(Module) ++ "_" ++ atom_to_list(TestName) ++ ".config".

read_file(File) ->
    case read_file(app, File) of
        {error, enoent} ->
            read_file(test, File);
        {ok, Bin} ->
            {ok, Bin}
    end.

read_file(app, File) -> %% Rebar3
    file:read_file(filename:join([".", "apps", "oc_chef_authz", "test", File]));
read_file(test, File) -> %% Rebar2
    file:read_file(filename:join(["..", "test", File])).
