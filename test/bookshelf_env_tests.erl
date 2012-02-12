%% @copyright 2012 Opscode, Inc. All Rights Reserved
%% @author Tim Dysinger <timd@opscode.com>
%%
%% Licensed to the Apache Software Foundation (ASF) under one or more
%% contributor license agreements.  See the NOTICE file distributed
%% with this work for additional information regarding copyright
%% ownership.  The ASF licenses this file to you under the Apache
%% License, Version 2.0 (the "License"); you may not use this file
%% except in compliance with the License.  You may obtain a copy of
%% the License at http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or
%% implied.  See the License for the specific language governing
%% permissions and limitations under the License.

-module(bookshelf_env_tests).
-include("bookshelf.hrl").

with_ip_test_() ->
    [{"should configure the listen ip address if the env has an 'interface'",
      fun() ->
              Env = ?env(with_ip, [{interface, "lo"}]),
              {ip, {127,0,0,1}} = lists:keyfind(ip, 1, Env)
      end
     }].

with_dispatch_test_() ->
    [{"should build proper 'cowboy' dispatch rules using env 'domains'",
      fun() ->
              EnvV1 = [{domains, ["clown.com", "school.com"]}],
              EnvV2 = ?env(with_dispatch, EnvV1),
              {dispatch,
               [{[bucket, <<"clown">>, <<"com">>],
                 [{[], bookshelf_bkt, EnvV1}]},
                {[bucket, <<"school">>, <<"com">>],
                 [{[], bookshelf_bkt, EnvV1}]}]} =
                  lists:keyfind(dispatch, 1, EnvV2)
      end
     }].

with_dir_test_() ->
    [{"should use any env 'dir' if provided",
      fun() ->
              [{dir, "/tmp"}] = ?env(with_dir, [{dir, "/tmp"}])
      end
     },
     {"should use ${priv_dir}/data/ if env 'dir' is the atom 'priv_dir'",
      fun() ->
              Expect = [{dir, ?file("data")}],
              Expect = ?env(with_dir, [{dir, priv_dir}])
      end
     },
     {"should use ${priv_dir}/data/ if env 'dir' is absent",
      fun() ->
              Expect = [{dir, ?file("data")}],
              Expect = ?env(with_dir, [])
      end
     }
    ].
