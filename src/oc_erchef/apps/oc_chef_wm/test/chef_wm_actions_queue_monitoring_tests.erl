% -*- erlang-indent-level: 4;indent-tabs-mode: nil; fill-column: 92 -*-
%% ex: ts=4 sw=4 et
%%% @author Dave Parfitt <dparfitt@chef.io>
%% Copyright 2015 Chef Software, Inc.
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

-module(chef_wm_actions_queue_monitoring_tests).

-include_lib("proper/include/proper.hrl").
-include_lib("eunit/include/eunit.hrl").


-define(QUEUE_LENGTH_REQ, "/queues/%2Fanalytics").
-define(MAX_LENGTH_REQ, "/policies/%2Fanalytics/max_length").

-define(EMPTY_STATUS,  [{queue_at_capacity,false},
                        {dropped_since_last_check,0},
                        {max_length,0},
                        {last_recorded_length,0},
                        {total_dropped,0},
                        {check_count, 1},
                        {mailbox_length, 0}]).

%% same as EMPTY_STATE with max_length set
-define(MAX_SET_STATUS,  [{queue_at_capacity,false},
                          {dropped_since_last_check,0},
                          {max_length,99},
                          {last_recorded_length,0},
                          {total_dropped,0},
                          {check_count, 1},
                          {mailbox_length, 0}]).



-define(ROUTING_KEY, <<"MyRoutingKey">>).
-define(MESSAGE, <<"MyMessage">>).

-define(VHOST, "/analytics").
-define(QUEUE_NAME, "alaska").

% allow quickcheck output in eunit tests via to_file etc
% https://github.com/manopapad/proper#using-proper-in-conjunction-with-eunit
-define(QC(Prop),?assertEqual(true, proper:quickcheck(proper:numtests(100, Prop), [{to_file, user}]))).



parse_max_length_test() ->
    MaxLengthJson = "{\"vhost\":\"/analytics\",\"name\":\"max_length\",\"pattern\":\"(erchef|alaska|notifier.notifications|notifier_config)\",\"apply-to\":\"queues\",\"definition\":{\"max-length\":10},\"priority\":0}",
    Result = chef_wm_rabbitmq_management:parse_max_length_response(MaxLengthJson),
    ?assertEqual(10, Result).

parse_max_length_invalid_json_test() ->
    MaxLengthJson = "{abc",
    Result = chef_wm_rabbitmq_management:parse_max_length_response(MaxLengthJson),
    ?assertEqual(undefined, Result).


parse_current_length_no_queue_test() ->
    NoQueueJson = "[]",
    Result =
    chef_wm_rabbitmq_management:parse_current_length_response(NoQueueJson, ?QUEUE_NAME),
    ?assertEqual(undefined, Result).


parse_current_length_no_messages_test() ->
    Result =
    chef_wm_rabbitmq_management:parse_current_length_response(no_messages_json(), ?QUEUE_NAME),
    ?assertEqual(0, Result).


parse_current_length_invalid_json_test() ->
    Json = "{abc",
    Result = chef_wm_rabbitmq_management:parse_current_length_response(Json, ?QUEUE_NAME),
    ?assertEqual(undefined, Result).


parse_current_length_some_messages_test() ->
    Result =
    chef_wm_rabbitmq_management:parse_current_length_response(some_message_json(), ?QUEUE_NAME),
    ?assertEqual(7, Result).



percentage_test() ->
    ?QC(prop_calc_ratio_and_percent()).

prop_calc_ratio_and_percent() ->
    ?FORALL({X, Y}, {pos_integer(), pos_integer()},
                begin
                    {Ratio, Percent} = chef_wm_rabbitmq_management:calc_ratio_and_percent(X, Y),
                    Ratio >= 0.0 andalso
                    Ratio =< 1.0 andalso
                    Percent >= 0.0 andalso
                    Percent =< 100.0
                end).


parse_integer_test() ->
    ?QC(prop_parse_integer()).

prop_parse_integer() ->
    ?FORALL(X, any(),
            case chef_wm_rabbitmq_management:parse_integer(X) of
                Y when is_integer(Y) -> true;
                Z -> Z == undefined
            end).

queue_monitor_test() ->
    ?QC(chef_wm_actions_queue_monitoring_statem:queue_monitor_prop()).

read_file(File) -> %% Rebar3
    {ok, Bin} = file:read_file(filename:join([".", "apps", "oc_chef_wm",
                                              "test", "queue_data", File])),
    binary_to_list(Bin).

max_length_json() ->
    read_file("valid_max_length.json").

no_messages_json() ->
    read_file("no_messages.json").

some_message_json() ->
    read_file("some_messages.json").

at_capacity_json() ->
    read_file("at_capacity.json").


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




queue_length_test_() ->
    {foreach,
     fun() ->
              % helpful for debugging, but makes the tests noisy
              %lager:start(),
              application:set_env(oc_chef_wm, rabbitmq, default_config()),
              {ok, _QMPid} =
              chef_wm_actions_queue_monitoring:start_link(?VHOST, ?QUEUE_NAME, 0, 0)
     end,
     fun(_) ->
             catch(chef_wm_actions_queue_monitoring:stop()),
             catch(meck:unload(oc_httpc))
     end,
     [
      {"max length not set",
       fun() ->
            % maximum length has been set, no state should be changed
            meck_response("404", "", "404", ""),
            chef_wm_actions_queue_monitoring:sync_status_ping(),

            %% checking max_length returns [], so state should be "empty"
            ?assertMatch(?EMPTY_STATUS, chef_wm_actions_queue_monitoring:status())
       end},
      {"max length set, no queue bound",
       fun() ->
            % maximum length has been configured, however a queue isn't bound
            % to the /analytics exchange
            meck_response("200", max_length_json(), "404", ""),
            chef_wm_actions_queue_monitoring:sync_status_ping(),
            ?assertMatch(?MAX_SET_STATUS, chef_wm_actions_queue_monitoring:status())
       end},
      {"max_length_set_queue_bound_no_messages",
       fun() ->
          % maximum length has been configured, a queue is bound to the exchange.
          % no messages are available to read

          meck_response("200",  max_length_json(), "200", no_messages_json()),
          chef_wm_actions_queue_monitoring:sync_status_ping(),

          ?assertMatch([{queue_at_capacity,false},
                                {dropped_since_last_check,0},
                                {max_length,99},
                                {last_recorded_length,0},
                                {total_dropped,0},
                                {check_count, 1},
                                {mailbox_length, 0}]
                        ,chef_wm_actions_queue_monitoring:status())
          end},
      {"max_length_set_queue_bound_some_messages",
       fun() ->
           % maximum length has been configured, a queue is bound to the exchange.
            % 7 messages are available to read
            meck_response("200", max_length_json(), "200", some_message_json()),
            chef_wm_actions_queue_monitoring:sync_status_ping(),

            ?assertMatch([{queue_at_capacity,false},
                           {dropped_since_last_check,0},
                           {max_length,99},
                           {last_recorded_length,7},
                           {total_dropped,0},
                           {check_count, 1},
                           {mailbox_length, 0}]
                     ,chef_wm_actions_queue_monitoring:status())

       end},
      {"drop_messages_no_overload",
       fun() ->
         % maximum length has been configured, a queue is bound to the exchange.
         % 7 messages are available to read
         % 2 messages will be dropped
        meck_response("200", max_length_json(), "200", some_message_json()),
            chef_wm_actions_queue_monitoring:sync_status_ping(),

            chef_wm_actions_queue_monitoring:message_dropped(),
            chef_wm_actions_queue_monitoring:message_dropped(),

            ?assertMatch([{queue_at_capacity,false},
                                {dropped_since_last_check,2},
                                {max_length,99},
                                {last_recorded_length,7},
                                {total_dropped,2},
                                {check_count, 1},
                                {mailbox_length, 0}]
                        ,chef_wm_actions_queue_monitoring:status())

            end},
      {"drop_messages_and_reset_no_overload",
       fun() ->
            % maximum length has been configured, a queue is bound to the exchange.
            % 7 messages are available to read
            % 2 messages will be dropped
            % recheck queue status which clears dropped_since_last_check
            meck_response("200", max_length_json(), "200", some_message_json()),
                chef_wm_actions_queue_monitoring:sync_status_ping(),

                chef_wm_actions_queue_monitoring:message_dropped(),
                chef_wm_actions_queue_monitoring:message_dropped(),
                %% calling sync_status_ping() will set the dropped_since_last_check to 0
                chef_wm_actions_queue_monitoring:sync_status_ping(),
                ?assertMatch([{queue_at_capacity,false},
                                    {dropped_since_last_check,0},
                                    {max_length,99},
                                    {last_recorded_length,7},
                                    {total_dropped,2},
                                    {check_count, 2},
                                    {mailbox_length, 0}]
                            ,chef_wm_actions_queue_monitoring:status())

         end},
      {"drop_messages_overload",
       fun() ->
            % maximum length has been configured, a queue is bound to the exchange.
            % 99 messages are available to read
            % 2 messages will be dropped
            meck_response("200", max_length_json(), "200", at_capacity_json()),
                chef_wm_actions_queue_monitoring:sync_status_ping(),

                chef_wm_actions_queue_monitoring:message_dropped(),
                chef_wm_actions_queue_monitoring:message_dropped(),

                ?assertMatch([{queue_at_capacity,true},
                                    {dropped_since_last_check,2},
                                    {max_length,99},
                                    {last_recorded_length,99},
                                    {total_dropped,2},
                                    {check_count, 1},
                                    {mailbox_length, 0}]
                            ,chef_wm_actions_queue_monitoring:status())
        end},

      {"drop_messages_overload_and_reset",
       fun() ->
            % maximum length has been configured, a queue is bound to the exchange.
            % 99 messages are available to read
            % 2 messages will be dropped
            % recheck queue status which clears dropped_since_last_check
            % check_count should be 2
            meck_response("200", max_length_json(), "200", at_capacity_json()),
            chef_wm_actions_queue_monitoring:sync_status_ping(),

            chef_wm_actions_queue_monitoring:message_dropped(),
            chef_wm_actions_queue_monitoring:message_dropped(),

            chef_wm_actions_queue_monitoring:sync_status_ping(),

            ?assertMatch([{queue_at_capacity,true},
                                    {dropped_since_last_check,0},
                                    {max_length,99},
                                    {last_recorded_length,99},
                                    {total_dropped,2},
                                    {check_count, 2},
                                    {mailbox_length, 0}]
                            ,chef_wm_actions_queue_monitoring:status())
       end},
      {"check_publish_not_at_capacity",
       fun() ->
            % ensure the publish function is called and no messages are dropped
            oc_chef_action_queue_config:set_rabbit_queue_monitor_setting(queue_length_monitor_enabled, true),
            oc_chef_action_queue_config:set_rabbit_queue_monitor_setting(drop_on_full_capacity, true),

            meck:new(oc_chef_action_queue),
            meck:expect(oc_chef_action_queue, publish, fun (_, _) -> ok end),

            ?assertEqual(0, meck:num_calls(oc_chef_action_queue, publish, [?ROUTING_KEY,?MESSAGE])),

            oc_chef_action:publish(?ROUTING_KEY, ?MESSAGE),

            ?assertMatch(1, meck:num_calls(oc_chef_action_queue, publish, [?ROUTING_KEY,?MESSAGE])),

            meck:unload(oc_chef_action_queue)
       end},
      {"check_publish_at_capacity",
       fun() ->
            %% ensure the publish function is called and 1 message is dropped
            %% due to queue being at capacity
            oc_chef_action_queue_config:set_rabbit_queue_monitor_setting(queue_length_monitor_enabled, true),
            oc_chef_action_queue_config:set_rabbit_queue_monitor_setting(drop_on_full_capacity, true),

            meck_response("200", max_length_json(), "200", at_capacity_json()),
            %% ensure that the queue is at capacity before calling
            %% oc_chef_action:publish
            chef_wm_actions_queue_monitoring:sync_status_ping(),

            meck:new(oc_chef_action_queue),
            meck:expect(oc_chef_action_queue, publish, fun (_, _) -> ok end),

            ?assertEqual(0, meck:num_calls(oc_chef_action_queue, publish, [?ROUTING_KEY,?MESSAGE])),

            oc_chef_action:publish(?ROUTING_KEY, ?MESSAGE),

            %% NO messages have been published, 1 message should be logged as dropped
            ?assertEqual(0, meck:num_calls(oc_chef_action_queue, publish, [?ROUTING_KEY,?MESSAGE])),
            Status = chef_wm_actions_queue_monitoring:status(),

            ?assertEqual(1, proplists:get_value(total_dropped, Status)),
            ?assertEqual(true, proplists:get_value(queue_at_capacity, Status)),
            ?assertEqual(99, proplists:get_value(last_recorded_length, Status)),

            meck:unload(oc_chef_action_queue)

       end},
      {"check_publish_at_capacity_and_reset",
        fun() ->
            %% ensure the publish function is called and 1 message is dropped
            %% due to queue being at capacity, reset queue length to 0
            %% and 1 message should be published

            oc_chef_action_queue_config:set_rabbit_queue_monitor_setting(queue_length_monitor_enabled, true),
            oc_chef_action_queue_config:set_rabbit_queue_monitor_setting(drop_on_full_capacity, true),

            meck_response("200", max_length_json(), "200", at_capacity_json()),
            %% ensure that the queue is at capacity before calling
            %% oc_chef_action:publish
            chef_wm_actions_queue_monitoring:sync_status_ping(),

            meck:new(oc_chef_action_queue),
            meck:expect(oc_chef_action_queue, publish, fun (_, _) -> ok end),

            ?assertEqual(0, meck:num_calls(oc_chef_action_queue, publish, [?ROUTING_KEY,?MESSAGE])),

            oc_chef_action:publish(?ROUTING_KEY, ?MESSAGE),

            %% NO messages have been published
            ?assertEqual(0, meck:num_calls(oc_chef_action_queue, publish, [?ROUTING_KEY,?MESSAGE])),
            Status = chef_wm_actions_queue_monitoring:status(),

            ?assertEqual(1 ,proplists:get_value(total_dropped, Status)),
            ?assertEqual(true ,proplists:get_value(queue_at_capacity, Status)),
            ?assertEqual(99 ,proplists:get_value(last_recorded_length, Status)),

            %% simulate the queue length going back down to 0
            meck:unload(oc_httpc),
            meck_response("200",  max_length_json(), "200", no_messages_json()),
            chef_wm_actions_queue_monitoring:sync_status_ping(),

            oc_chef_action:publish(?ROUTING_KEY, ?MESSAGE),

            ?assertEqual(1, meck:num_calls(oc_chef_action_queue, publish, [?ROUTING_KEY,?MESSAGE])),

            UpdatedStatus = chef_wm_actions_queue_monitoring:status(),
            ?assertEqual(1 ,proplists:get_value(total_dropped, UpdatedStatus)),
            ?assertEqual(false ,proplists:get_value(queue_at_capacity, UpdatedStatus)),
            ?assertEqual(0 ,proplists:get_value(last_recorded_length, UpdatedStatus)),

            meck:unload(oc_chef_action_queue)

        end},
      {"check_publish_at_capacity_no_drop",
       fun() ->
          % queue is at capacity, but don't drop messages due to configuration
          oc_chef_action_queue_config:set_rabbit_queue_monitor_setting(queue_length_monitor_enabled, true),
          oc_chef_action_queue_config:set_rabbit_queue_monitor_setting(drop_on_full_capacity, false),

          meck_response("200", max_length_json(), "200", at_capacity_json()),
          %% ensure that the queue is at capacity before calling
          %% oc_chef_action:publish
          chef_wm_actions_queue_monitoring:sync_status_ping(),

          meck:new(oc_chef_action_queue),
          meck:expect(oc_chef_action_queue, publish, fun (_, _) -> ok end),

          ?assertEqual(0 ,meck:num_calls(oc_chef_action_queue, publish, [?ROUTING_KEY,?MESSAGE])),

          oc_chef_action:publish(?ROUTING_KEY, ?MESSAGE),

          %% rabbitmq_drop_on_full_capacity is false, so don't drop the message
          ?assertEqual(1 ,meck:num_calls(oc_chef_action_queue, publish, [?ROUTING_KEY,?MESSAGE])),

          meck:unload(oc_chef_action_queue)

       end},
      {"no_queue_length_monitor",
       fun() ->
          % queue length monitor is disabled
          catch(chef_wm_actions_queue_monitoring:stop()),

          oc_chef_action_queue_config:set_rabbit_queue_monitor_setting(queue_length_monitor_enabled, false),
          oc_chef_action_queue_config:set_rabbit_queue_monitor_setting(drop_on_full_capacity, false),

          undefined = whereis(chef_wm_actions_queue_monitoring),
          meck:new(oc_chef_action_queue),
          meck:expect(oc_chef_action_queue, publish, fun (_, _) -> ok end),

          ?assertEqual(0 ,meck:num_calls(oc_chef_action_queue, publish, [?ROUTING_KEY,?MESSAGE])),

          oc_chef_action:publish(?ROUTING_KEY, ?MESSAGE),

          ?assertEqual(1 ,meck:num_calls(oc_chef_action_queue, publish, [?ROUTING_KEY,?MESSAGE])),

          meck:unload(oc_chef_action_queue)
       end},
      {"queue_monitor_timeout",
       fun() ->
            % set oc_httpc to sleep for 100 millis, but have is_queue_at_capacity timeout
            % after 10 millis.
            oc_chef_action_queue_config:set_rabbit_queue_monitor_setting(queue_length_monitor_timeout_millis, 0),

            ?assertEqual(true, chef_wm_actions_queue_monitoring:is_queue_at_capacity())
       end
      },
      {"override_queue_at_capacity",
       fun() ->
            % manually set the queue capacity
            meck_response("200", max_length_json(), "200", no_messages_json()),
            chef_wm_actions_queue_monitoring:sync_status_ping(),
            FirstStatus = chef_wm_actions_queue_monitoring:status(),
            chef_wm_actions_queue_monitoring:override_queue_at_capacity(true),
            SecondStatus = chef_wm_actions_queue_monitoring:status(),
            chef_wm_actions_queue_monitoring:override_queue_at_capacity(false),
            ThirdStatus = chef_wm_actions_queue_monitoring:status(),
            ?assertEqual(false, proplists:get_value(queue_at_capacity, FirstStatus)),
            ?assertEqual(true, proplists:get_value(queue_at_capacity, SecondStatus)),
            ?assertEqual(false, proplists:get_value(queue_at_capacity, ThirdStatus))
       end},
      {"start_stop_timer",
       fun() ->
            % match against the timer tuple in case the fields of #state{} change
            ?assertMatch({interval, _}, erlang:element(5, sys:get_state(chef_wm_actions_queue_monitoring))),

            % calling stop_timer() more than once shouldn't break anything
            chef_wm_actions_queue_monitoring:stop_timer(),
            ?assertEqual(undefined, erlang:element(5, sys:get_state(chef_wm_actions_queue_monitoring))),
            chef_wm_actions_queue_monitoring:stop_timer(),

            % calling start_timer() more than once shouldn't break anything
            chef_wm_actions_queue_monitoring:start_timer(),
            ?assertMatch({interval, _},erlang:element(5, sys:get_state(chef_wm_actions_queue_monitoring))),
            chef_wm_actions_queue_monitoring:start_timer(),

            % calling stop_timer() again calls terminate/2 without a timer (coverage)
            chef_wm_actions_queue_monitoring:stop_timer()
       end},
      {"unknown calls don't crash the gen_server",
       fun() ->
          ?assertEqual(ignored, gen_server:call(chef_wm_actions_queue_monitoring, foo))
       end},
           {"max_length_connection_fail",
       fun() ->
          % simulate a connection failure while checking max_length
          meck_conn_failure(),
          chef_wm_actions_queue_monitoring:sync_status_ping(),
          ?assertMatch(?EMPTY_STATUS, chef_wm_actions_queue_monitoring:status())
       end},
      {"current_length_connection_fail",
       fun() ->
          % max_length check succeeds
          % simulate a connection failure while checking current length
          meck:new(oc_httpc),
          meck:expect(oc_httpc, request,
              fun(_, Path, _, _, _) ->
                 case Path of
                        ?MAX_LENGTH_REQ -> dummy_response("200", max_length_json());
                        ?QUEUE_LENGTH_REQ -> {error,{conn_failed,undefined}}
                 end
              end),

          chef_wm_actions_queue_monitoring:sync_status_ping(),
          ?assertMatch(?MAX_SET_STATUS, chef_wm_actions_queue_monitoring:status())
       end},
      {"max_length_unknown_response",
       fun() ->
          %% oc_httpc/ibrowse return an unknown response
          meck:new(oc_httpc),
          meck:expect(oc_httpc, request, fun(_, _Path, _, _, _) -> foo end),
          chef_wm_actions_queue_monitoring:sync_status_ping(),
          ?assertMatch(?EMPTY_STATUS, chef_wm_actions_queue_monitoring:status())
       end},
      {"current_length_unknown_response",
       fun() ->
          % max_length check succeeds
          % simulate an unknown response while checking current length
          meck:new(oc_httpc),
          meck:expect(oc_httpc, request,
              fun(_, Path, _, _, _) ->
                 case Path of
                        ?MAX_LENGTH_REQ -> dummy_response("200", max_length_json());
                        ?QUEUE_LENGTH_REQ -> foo
                 end
              end),

          chef_wm_actions_queue_monitoring:sync_status_ping(),
          ?assertMatch(?MAX_SET_STATUS, chef_wm_actions_queue_monitoring:status())
       end}
     ]}.


process_test_() ->
 {foreach,
     fun() ->
              %% DON'T start the queue manager
              application:set_env(oc_chef_wm, rabbitmq, default_config())
     end,
     fun(_) ->
             % try and stop the queue monitor if it was started in a test
             catch(chef_wm_actions_queue_monitoring:stop()),
             catch(meck:unload(oc_httpc))
     end,
     [
     {"unknown casts don't crash the gen_server",
       fun() ->
          {ok, QMPid} = chef_wm_actions_queue_monitoring:start_link(?VHOST, ?QUEUE_NAME, 0, 0),
          gen_server:cast(chef_wm_actions_queue_monitoring, foo),
          ?assertEqual(QMPid, whereis(chef_wm_actions_queue_monitoring))
       end},
      {"unknown info doesn't crash the gen_server",
       fun() ->
          {ok, QMPid} = chef_wm_actions_queue_monitoring:start_link(?VHOST, ?QUEUE_NAME, 0, 0),
          QMPid ! foo,
          ?assertEqual(QMPid, whereis(chef_wm_actions_queue_monitoring))
       end},

      {"unknown_process_exit",
       fun() ->
          {ok, QMPid} = chef_wm_actions_queue_monitoring:start_link(?VHOST, ?QUEUE_NAME, 0, 0),
          % send a random EXIT signal to the queue monitor, it shouldn't crash
          chef_wm_actions_queue_monitoring:sync_status_ping(),
          {interval, _} = TimerBefore = erlang:element(5, sys:get_state(chef_wm_actions_queue_monitoring)),
          QMPid ! {'EXIT', foo, bar},
          ?assertEqual(QMPid, whereis(chef_wm_actions_queue_monitoring)),
          TimerAfter = erlang:element(5, sys:get_state(chef_wm_actions_queue_monitoring)),
          ?assertMatch(TimerBefore, TimerAfter)
        end}
     ]}.

% generate a dummy ibrowse response using the given StatusCode and Content
dummy_response(StatusCode, Content) ->
                            {ok,StatusCode,
                            [{"Content-Type","application/json"}],
                            Content}.


% meck the RabbitMQ management api endpoints for max_length and current queue
% length
meck_response(MaxLengthStatus, MaxLengthContent,
              QueueLengthStatus, QueueLengthContent) ->
    meck:new(oc_httpc),
    meck:expect(oc_httpc, request,
        fun(_, Path, _, _, _) ->
            case Path of
                ?QUEUE_LENGTH_REQ -> dummy_response(QueueLengthStatus, QueueLengthContent);
                ?MAX_LENGTH_REQ -> dummy_response(MaxLengthStatus, MaxLengthContent);
                Unknown -> io:format(user, "UNKNOWN REQUEST ~p~n", [Unknown])
            end
        end).


meck_conn_failure() ->
    meck:new(oc_httpc),
    meck:expect(oc_httpc, request,
        fun(_, _Path, _, _, _) ->
            {error,{conn_failed,undefined}}
        end).

