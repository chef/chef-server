%% -*- erlang-indent-level: 4; indent-tabs-mode: nil; fill-column: 80 -*-
%% ex: ts=4 sw=4 et
%%
%%-------------------------------------------------------------------
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
%% @author Eric Merritt <ericbmerritt@gmail.com>
%%-------------------------------------------------------------------
-module(depsolver_tests).

-include_lib("eunit/include/eunit.hrl").
%%===========================================================================
%% Tests
%%============================================================================
all_test_() ->
  {foreach,
    fun() -> ok end,
    fun(_) -> ok end,
    [
      {?MODULE, filter_versions}
    ]
}.

filter_versions() ->

    Cons = [{app2, "2.1", '~>'},
            {app3, "0.1.1", "0.1.5", between},
            {app4, "5.0.0", gte},
            {app5, "2.0.0", '>='},
            app5],

    Packages = [{app1, "0.1.0"},
                {app1, "0.2"},
                {app1, "0.2"},
                {app1, "3.0"},
                {app2, "0.0.1"},
                {app2, "0.1"},
                {app2, "1.0"},
                {app2, "2.1.5"},
                {app2, "2.2"},
                {app2, "3.0"},
                {app3, "0.1.0"},
                {app3, "0.1.3"},
                {app3, "2.0.0"},
                {app3, "3.0.0"},
                {app3, "4.0.0"},
                {app4, "0.1.0"},
                {app4, "0.3.0"},
                {app4, "5.0.0"},
                {app4, "6.0.0"},
                {app5, "0.1.0"},
                {app5, "0.3.0"},
                {app5, "2.0.0"},
                {app5, "6.0.0"}],

    ?assertMatch({ok, [{app1,"0.1.0"},
                       {app1,"0.2"},
                       {app1,"0.2"},
                       {app1,"3.0"},
                       {app2,"2.1.5"},
                       {app2,"2.2"},
                       {app3,"0.1.3"},
                       {app4,"5.0.0"},
                       {app4,"6.0.0"},
                       {app5,"2.0.0"},
                       {app5,"6.0.0"}]},
                 depsolver:filter_packages(Packages, Cons)),

    Ret = depsolver:filter_packages(Packages,
                                    [{"foo", "1.0.0", '~~~~'} | Cons]),
    ?assertMatch({error, {invalid_constraints, [{<<"foo">>,{{1,0,0},{[],[]}},'~~~~'}]}}, Ret).
