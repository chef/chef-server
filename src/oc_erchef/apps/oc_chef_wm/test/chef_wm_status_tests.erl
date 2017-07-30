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

-define(ANALYTICS_STATUS,{[
  {<<"queue_at_capacity">>, false},
  {<<"dropped_since_last_check">>, 0},
  {<<"max_length">>, 0},
  {<<"last_recorded_length">>, 0},
  {<<"total_dropped">>, 0},
  {<<"check_count">>, 0},
  {<<"mailbox_length">>, 0}]}).

-define(PING_TIMEOUT, 50).
-define(SLOW_CHECK_SLEEP, 100).

setup_env() ->
    application:set_env(oc_chef_wm, health_ping_timeout, ?PING_TIMEOUT),
    application:set_env(oc_chef_wm, health_ping_modules, ?CHECK_MODS),
    application:set_env(oc_chef_wm, rabbitmq, default_config()),
    oc_chef_action_queue_config:set_rabbit_queue_monitor_setting(queue_length_monitor_enabled, false),
    ok.

cleanup_env() ->
    application:unset_env(oc_chef_wm, health_ping_timeout),
    application:unset_env(oc_chef_wm, health_ping_modules),
    application:unset_env(oc_chef_wm, rabbitmq),
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
              {Status, Json} = chef_wm_status:check_health(),
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
              {Status, _Json} = chef_wm_status:check_health(),
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
              {Status, _Json} = chef_wm_status:check_health(),
              ?assertEqual(fail, Status)
      end}]}.


default_config() ->
    [
        {management,
        [{user, <<"rabbitmgmt">>},
            {port, 15672},
            {password, <<"chef123">>},
            % rabbitmq management http connection pool
            {rabbitmq_management_service,
            [{root_url, "http://127.0.0.1:15672/api"},
            {timeout, 30000},
            {init_count, 25},
            {max_count, 100},
            {cull_interval, {60, sec}},
            {max_age, {70, sec}},
            {max_connection_duration, {70, sec}},
            {ibrowse_options,
            [{connect_timeout, 10000},
                {basic_auth,
                {"rabbitmgmt",
                "chef123"}}]}
            ]}]},
        {monitoring,
        [{queue_length_monitor_enabled, true},
            {queue_length_monitor_vhost, "/analytics"},
            {queue_length_monitor_queue, alaska },
            {queue_length_monitor_millis, 30000 },
            {queue_length_monitor_timeout_millis, 5000 },
            {drop_on_full_capacity, true },
            {queue_at_capacity_affects_overall_status, false}
        ]}
    ].


check_health_with_rabbit_monitoring_test_() ->
  {setup,
    fun() ->
      setup_env(),
      application:set_env(oc_chef_wm, rabbitmq, default_config()),
      {ok, _QMPid} = chef_wm_actions_queue_monitoring:start_link("/analytics", "alaska", 0, 0),
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
          catch(chef_wm_actions_queue_monitoring:stop()),
          cleanup_env()
    end,
    [
    fun() ->
          {Status, Json} = chef_wm_status:check_health(),
          ?assertEqual(pong, Status),
          Ejson = chef_json:decode(Json),
          ?assertEqual(<<"pong">>, ej:get({<<"status">>}, Ejson)),
          [ ?assertEqual(<<"pong">>, ej:get({"upstreams", a2b(Mod)}, Ejson))
              || Mod <- ?CHECK_MODS ],
          ?assertEqual(?ANALYTICS_STATUS, ej:get({<<"analytics_queue">>}, Ejson))

    end]}.

check_queue_mon_affects_overall_status_test_() ->
    {setup,
     fun() ->
             setup_env(),
             application:set_env(oc_chef_wm, rabbitmq, default_config()),
             {ok, _QMPid} = chef_wm_actions_queue_monitoring:start_link("/analytics", "alaska", 0, 0),
             chef_wm_actions_queue_monitoring:override_queue_at_capacity(true),
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
              % overall_status will be fail as the queue is at capacity as
              % queue_at_capacity_affects_overall_status is true
              oc_chef_action_queue_config:set_rabbit_queue_monitor_setting(queue_at_capacity_affects_overall_status, true),
              {Status, Json} = chef_wm_status:check_health(),
              ?assertEqual(fail, Status),
              Ejson = chef_json:decode(Json),
              ?assertEqual(<<"fail">>, ej:get({<<"status">>}, Ejson)),
              ?assertEqual(true, ej:get({<<"analytics_queue">>, <<"queue_at_capacity">>}, Ejson)),
              [ ?assertEqual(<<"pong">>, ej:get({"upstreams", a2b(Mod)}, Ejson))
                || Mod <- ?CHECK_MODS ]
      end,
      fun() ->
              % overall_status will be pong as the queue is at capacity, but
              % queue_at_capacity_affects_overall_status is false
              oc_chef_action_queue_config:set_rabbit_queue_monitor_setting(queue_at_capacity_affects_overall_status, false),
              {Status, Json} = chef_wm_status:check_health(),
              ?assertEqual(pong, Status),
              Ejson = chef_json:decode(Json),
              ?assertEqual(<<"pong">>, ej:get({<<"status">>}, Ejson)),
              ?assertEqual(true, ej:get({<<"analytics_queue">>, <<"queue_at_capacity">>}, Ejson)),
              [ ?assertEqual(<<"pong">>, ej:get({"upstreams", a2b(Mod)}, Ejson))
                || Mod <- ?CHECK_MODS ]
      end,
      fun() ->
              % overall_status will be pong as the queue is at capacity, but the queue monitor is
              % disabled
              oc_chef_action_queue_config:set_rabbit_queue_monitor_setting(queue_at_capacity_affects_overall_status, false),
              oc_chef_action_queue_config:set_rabbit_queue_monitor_setting(queue_length_monitor_enabled, false),
              {Status, Json} = chef_wm_status:check_health(),
              ?assertEqual(pong, Status),
              Ejson = chef_json:decode(Json),
              ?assertEqual(<<"pong">>, ej:get({<<"status">>}, Ejson)),
              ?assertEqual(undefined, ej:get({<<"analytics_queue">>, <<"queue_at_capacity">>}, Ejson)),
              [ ?assertEqual(<<"pong">>, ej:get({"upstreams", a2b(Mod)}, Ejson))
                || Mod <- ?CHECK_MODS ]
      end,
      fun() ->
              % overall_status not affected as the queue monitor is disabled
              oc_chef_action_queue_config:set_rabbit_queue_monitor_setting(queue_at_capacity_affects_overall_status, true),
              oc_chef_action_queue_config:set_rabbit_queue_monitor_setting(queue_length_monitor_enabled, false),
              {Status, Json} = chef_wm_status:check_health(),
              ?assertEqual(pong, Status),
              Ejson = chef_json:decode(Json),
              ?assertEqual(<<"pong">>, ej:get({<<"status">>}, Ejson)),
              ?assertEqual(undefined, ej:get({<<"analytics_queue">>, <<"queue_at_capacity">>}, Ejson)),
              [ ?assertEqual(<<"pong">>, ej:get({"upstreams", a2b(Mod)}, Ejson))
                || Mod <- ?CHECK_MODS ]
      end


     ]}.


a2b(A) ->
    erlang:atom_to_binary(A, utf8).

a2s(A) ->
    atom_to_list(A).


