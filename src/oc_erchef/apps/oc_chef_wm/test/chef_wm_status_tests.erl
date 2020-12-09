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

-module(chef_wm_status_tests).

-include_lib("eunit/include/eunit.hrl").
-include("oc_chef_wm.hrl").

-define(CHECK_MODS, [chef_sql, chef_solr]).

%% totally made up data, used for mocks
-define(KEYGEN_STATUS, [{keys, 10}, {max, 10},
                        {max_workers, 10}, {cur_max_workers, 10},
                        {inflight, 0}, {avail_workers, 10},
                        {start_size, 10}]).

-define(PING_TIMEOUT, 50).
-define(SLOW_CHECK_SLEEP, 100).

setup_env() ->
    application:set_env(oc_chef_wm, health_ping_timeout, ?PING_TIMEOUT),
    application:set_env(oc_chef_wm, health_ping_modules, ?CHECK_MODS),
    ok.

cleanup_env() ->
    application:unset_env(oc_chef_wm, health_ping_timeout),
    application:unset_env(oc_chef_wm, health_ping_modules),
    ok.

check_health_all_ok_test_() ->
    {setup,
     fun() ->
             setup_env(),
             [ begin
                   meck:new(Mod),
                   meck:expect(Mod, ping, fun() -> pong end)
               end || Mod <- ?CHECK_MODS ],
             meck:new(chef_keygen_cache),
             meck:expect(chef_keygen_cache, status_for_json, fun() -> ?KEYGEN_STATUS end)
     end,
     fun(_) ->
             [ meck:unload(Mod) || Mod <- ?CHECK_MODS ],
             meck:unload(chef_keygen_cache),
             cleanup_env()
     end,
     [
      fun() ->
              {Status, Json} = chef_wm_status:check_health(<<"test_version">>),
              ?assertEqual(pong, Status),
              Ejson = chef_json:decode(Json),
              ?assertEqual(<<"pong">>, ej:get({<<"status">>}, Ejson)),
              [ ?assertEqual(<<"pong">>, ej:get({"upstreams", a2b(Mod)}, Ejson))
                || Mod <- ?CHECK_MODS ]
      end]}.

check_health_one_crash_no_module_test_() ->
    {setup,
     fun() ->
             setup_env(),
             application:set_env(chef_wm, health_ping_modules, [bad_module_no_exist]),
             meck:new(chef_keygen_cache),
             meck:expect(chef_keygen_cache, status_for_json, fun() -> ?KEYGEN_STATUS end)

     end,
     fun(_) ->
             cleanup_env(),
             meck:unload(chef_keygen_cache)
     end,
     [
      fun() ->
              {Status, _Json} = chef_wm_status:check_health(<<"test_version">>),
              ?assertEqual(fail, Status)
      end]}.

check_health_one_pang_test_() ->
    [ check_health_mod_fails(Mod, pang) || Mod <- ?CHECK_MODS ].

check_health_one_timeout_test_() ->
    [ check_health_mod_fails(Mod, timeout) || Mod <- ?CHECK_MODS ].

check_health_mod_fails(BadMod, How) ->
    OkMods = ?CHECK_MODS -- [BadMod],
    {setup,
     fun() ->
             setup_env(),
             [ begin
                   meck:new(Mod),
                   meck:expect(Mod, ping, fun() -> pong end)
               end || Mod <- OkMods ],
             meck:new(BadMod),
             case How of
                 pang ->
                     meck:expect(BadMod, ping, fun() -> pang end);
                 timeout ->
                     meck:expect(BadMod, ping, fun() -> timer:sleep(?SLOW_CHECK_SLEEP) end)
             end,
             meck:new(chef_keygen_cache),
             meck:expect(chef_keygen_cache, status_for_json, fun() -> ?KEYGEN_STATUS end)
     end,
     fun(_) ->
             [ meck:unload(Mod) || Mod <- ?CHECK_MODS ],
             meck:unload(chef_keygen_cache),
             cleanup_env()
     end,
     [{"handles failure of " ++ a2s(BadMod),
      fun() ->
              {Status, _Json} = chef_wm_status:check_health(<<"test_version">>),
              ?assertEqual(fail, Status)
      end}]}.

a2b(A) ->
    erlang:atom_to_binary(A, utf8).

a2s(A) ->
    atom_to_list(A).
