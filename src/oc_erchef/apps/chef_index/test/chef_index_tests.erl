%% Copyright 2015 Chef Server, Inc. All Rights Reserved.
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

-module(chef_index_tests).
-include_lib("eunit/include/eunit.hrl").
-define(EXPECTED_DOC, [<<"<doc>">>,
                       [[<<"<field name=\"">>,<<"X_CHEF_id_CHEF_X">>,<<"\">">>,<<"a1">>,
                         <<"</field>">>],
                        [<<"<field name=\"">>,<<"X_CHEF_database_CHEF_X">>,<<"\">">>,<<"chef_db1">>,
                         <<"</field>">>],
                        [<<"<field name=\"">>,<<"X_CHEF_type_CHEF_X">>,<<"\">">>,<<"role">>,
                         <<"</field>">>]],
                       [],
                       [<<"<field name=\"">>,<<"content">>,<<"\">">>,
                        [[<<"X_CHEF_database_CHEF_X">>,<<"__=__">>,<<"chef_db1">>,<<" ">>],
                         [<<"X_CHEF_id_CHEF_X">>,<<"__=__">>,<<"a1">>,<<" ">>],
                         [<<"X_CHEF_type_CHEF_X">>,<<"__=__">>,<<"role">>,<<" ">>],
                         [<<"key1">>,<<"__=__">>,<<"value1">>,<<" ">>],
                         [<<"key2">>,<<"__=__">>,<<"value2">>,<<" ">>]],
                        <<"</field>">>],
                       <<"</doc>">>]).

-define(EXPECTED_DELETE_DOC, [<<"<delete><id>">>,<<"a1">>,<<"</id></delete>">>]).

chef_index_test_() ->
    Item = {[{<<"key1">>, <<"value1">>},
             {<<"key2">>, <<"value2">>}]},
    {foreach,
     fun() ->
             chef_index_test_utils:start_stats_hero(),
             application:set_env(chef_index, rabbitmq_vhost, <<"testvhost">>),
             application:ensure_all_started(prometheus),
             chef_index_expand:declare_metrics(),
             meck:new([chef_index_queue, chef_index_expand, chef_index_batch], [passthrough])
     end,
     fun(_) ->
             meck:unload([chef_index_queue, chef_index_expand, chef_index_batch])
     end,
     [{"add calls chef_index_queue:set when search_queue_mode is rabbitmq",
       fun() ->
               application:set_env(chef_index, search_queue_mode, rabbitmq),
               meck:expect(chef_index_queue, set, fun(<<"testvhost">>, role, <<"a1">>, <<"db1">>, [{}]) -> ok end),
               chef_index:add(role, <<"a1">>, <<"db1">>, [{}], <<"abcde">>),
               ?assert(meck:validate(chef_index_queue))
       end
      },
      {"add calls chef_index_batch:add_item when search_queue_mode is batch",
       fun() ->
               application:set_env(chef_index, search_queue_mode, batch),
               meck:expect(chef_index_batch, add_item, fun(?EXPECTED_DOC) -> ok end),
               chef_index:add(role, <<"a1">>, <<"db1">>, Item, <<"undefined">>),
               ?assert(meck:validate(chef_index_batch))
       end
      },
      {"add calls chef_index_expand:send_item when search_queue_mode is inline",
       fun() ->
               application:set_env(chef_index, search_queue_mode, inline),
               meck:expect(chef_index_expand, send_item, fun(?EXPECTED_DOC) -> ok end),
               chef_index:add(role, <<"a1">>, <<"db1">>, Item, <<"undefined">>),
               ?assert(meck:validate(chef_index_expand))
       end
      },
      {"add_batch adds each item passed to it",
       fun() ->
               application:set_env(chef_index, search_queue_mode, inline),
               meck:expect(chef_index_expand, send_item, fun(?EXPECTED_DOC) -> ok end),
               chef_index:add_batch([{role, <<"a1">>, <<"db1">>, Item},
                                     {role, <<"a1">>, <<"db1">>, Item},
                                     {role, <<"a1">>, <<"db1">>, Item}]),
               ?assertEqual(3, meck:num_calls(chef_index_expand, send_item, '_'))
       end
      },
      {"delete calls chef_index_queue:delete when search_queue_mode is rabbitmq",
       fun() ->
               application:set_env(chef_index, search_queue_mode, rabbitmq),
               meck:expect(chef_index_queue, delete, fun(<<"testvhost">>, role, <<"a1">>, <<"db1">>) -> ok end),
               chef_index:delete(role, <<"a1">>, <<"db1">>, <<"undefined">>),
               ?assert(meck:validate(chef_index_queue))
       end
      },
      {"delete calls chef_index_expand:send_delete when search_queue_mode is inline",
      fun() ->
              application:set_env(chef_index, search_queue_mode, inline),
              delete_assertion()
      end
      },
      {"delete calls chef_index_expand:send_delete when search_queue_mode is batch",
       fun() ->
               application:set_env(chef_index, search_queue_mode, batch),
               delete_assertion()
       end}
     ]
    }.

delete_assertion() ->
    meck:expect(chef_index_expand, send_delete, fun(?EXPECTED_DELETE_DOC) -> ok end),
    chef_index:delete(role, <<"a1">>, <<"db1">>, <<"abcd">>),
    ?assert(meck:validate(chef_index_expand)).

ping_test_() ->
    {foreach,
     fun() ->
             application:set_env(chef_index, rabbitmq_vhost, <<"/testvhost">>),
             application:set_env(chef_index, search_queue_mode, rabbitmq),
             application:set_env(chef_index, rabbitmq_index_management_service, [{enabled, true}]),
             meck:new(chef_wm_rabbitmq_management)
     end,
     fun(_) ->
             meck:unload(chef_wm_rabbitmq_management)
     end,
     [{"When rabbitmq is not used, the check returns pong",
       % This is not a requirement, there's just nothing else that
       % is currently checked.
       fun() ->
               application:set_env(chef_index, search_queue_mode, batch),
               meck:expect(chef_wm_rabbitmq_management, check_aliveness,
                           fun(_, "/testvhost") ->
                                   throw(shouldnt_be_called)
                           end),
               Status = chef_index:ping(),
               ?assertEqual(pong, Status)
       end
      },
      {"When rabbitmq is enabled but management is not, the check returns pong",
       % This is not a requirement, there's just nothing else that
       % is currently checked.
       fun() ->
               application:set_env(chef_index, rabbitmq_index_management_service, [{enabled, false}]),
               meck:expect(chef_wm_rabbitmq_management, check_aliveness,
                           fun(_, "/testvhost") ->
                                   throw(shouldnt_be_called)
                           end),
               Status = chef_index:ping(),
               ?assertEqual(pong, Status)
       end
      },
      {"When rabbitmq and management are enabled, and rabbit is alive, returns pong",
       fun() ->
               meck:expect(chef_wm_rabbitmq_management, check_aliveness,
                           fun(_, "/testvhost") ->
                                   true
                           end),
               Status = chef_index:ping(),
               ?assertEqual(pong, Status)
       end
      },
      {"When rabbitmq and management are enabled, and rabbit is not alive, returns pang",
       fun() ->
               meck:expect(chef_wm_rabbitmq_management, check_aliveness,
                           fun(_, "/testvhost") ->
                                   false
                           end),
               Status = chef_index:ping(),
               ?assertEqual(pang, Status)
       end
      }
     ]}.
