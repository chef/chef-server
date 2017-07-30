% -*- erlang-indent-level: 4;indent-tabs-mode: nil; fill-column: 92 -*-
%% ex: ts=4 sw=4 et
%%% @author Dave Parfitt <dparfitt@chef.io>
%%% @doc
%%% monitor RabbitMQ length of analytics queues
%%% @end
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


%% This gen_server periodically checks the RabbitMQ management console for
%% max_length and current queue length values.
%%
%% 0) If a max_length policy is not set on the exchange, do nothing and recheck
%%    at the next interval.
%% 1) If a max_length policy IS set on the exchange but a queue is not
%%    bound to the /analytics exchange, do nothing and recheck at the
%%    next interval.
%% 2) If a max_length policy IS set on the exchange, store the value and
%%    proceed to check the current queue length. Store max_length and
%%    current length values.
%% 2a) If current length < max_length, and the queue is < 80% full,
%%     set queue_at_capacity = false, continue
%% 2b) If current length < max_length, and the queue is >= 80% && < 100% full
%%     log a warning message stating the current percentage of capacity,
%%     set queue_at_capacity, continue
%% 2b) If current length = max_length, set queue_at_capacity = true,
%%     log a warning message stating the current percentage of capacity


%% clients of chef_wm_actions_queue_monitoring use 2 API calls:
%% is_queue_at_capacity/0 and message_dropped/0. When attempting to send
%% message to RabbitMQ, a client can check is_queue_at_capacity/0 and
%% optionally not send a message if true ("dropping a message"). Clients
%% that drop messages should report that a message has been dropped
%% to this gen_server via message_dropped/0. Note, message_dropped/0
%% is a cast, while is_queue_at_capacity/0 is a call. If calls to
%% is_queue_at_capacity/0 take "too long", a timeout can be set via
%% the `rabbitmq_queue_length_timeout_millis` config setting. This
%% setting will force is_queue_at_capacity/0 to return `true` upon
%% timeout, even if it's not necessarily the queue that is timing
%% out.


-module(chef_wm_actions_queue_monitoring).

-ifdef(TEST).
-compile(export_all).
-endif.

-export([start_link/4,
         init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3,
         stop/0,
         status/0,
         is_queue_at_capacity/0,
         message_dropped/0,
         override_queue_at_capacity/1,
         sync_status_ping/0,
         start_timer/0,
         stop_timer/0

        ]).

-behaviour(gen_server).
-define(SERVER, ?MODULE).


-define(VHOST, "%2Fanalytics").
-define(QUEUE, <<"alaska">>).
-define(GEN_SERVER_TIMEOUT_MILLIS, 5000).
-define(QUEUE_MONITOR_RUN_EVERY_MILLIS, 60000).
-define(QUEUE_MONITOR_SETTING(Key, Default), oc_chef_action_queue_config:get_rabbit_queue_monitor_setting(Key, Default)).

-record(queue_monitor_state, {
                % name of the rabbitmq vhost to monitor, as a string
                vhost_to_monitor = undefined,

                % name of the rabbitmq queue to monitor, as a binary
                queue_to_monitor = undefined,

                % has the max_length of the queue been reached?
                queue_at_capacity = false,

                % timer to check max and current queue length
                timer = undefined,

                % maximum queue length set by rabbitmq policy
                % mostly static, but a user *can* change the value
                % via rabbitmqctl
                max_length = 0,

                % last recorded length of the analytics queue
                last_recorded_length = 0,

                % number of messages that have NOT been sent to the analytics
                % queue due to queue_at_capacity = true.
                % This metric is manually reported by users of the queue
                % monitor
                dropped_since_last_check = 0,

                % monotonically increasing number of messages that have NOT
                % been sent to the analytics queue due to queue_at_capacity =
                % true. This number is never reset.
                total_dropped = 0,

                % The async worker pid responsible for checking max length and
                % current length of the queue. There can ONLY be ONE worker
                % process.
                worker_process = undefined,

                % if sync_status_ping/0 is called, this is the Pid of the
                % calling process. This is recorded because checking the queue
                % length and max is async via the worker_process, and the
                % reply is sent back to this Pid via gen_server:reply()
                sync_response_process = undefined,

                % total # of checks, even if unsuccessful
                check_count = 0
               }).


start_link(Vhost, Queue, MaxLength, CurrentLength) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [Vhost,
                                                      Queue,
                                                      MaxLength,
                                                      CurrentLength], []).

-spec status() -> [{atom(), _}].
status() ->
    gen_server:call(?SERVER, status).

% is current queue length >= max queue length?
% Note, this simply checks the gen_server state, and does NOT
% ping the RabbitMQ management console.
% Pinging the management console is handled by an internal timer
% so we never block waiting for an HTTP response.
is_queue_at_capacity() ->
    Timeout =
      ?QUEUE_MONITOR_SETTING(queue_length_monitor_timeout_millis, ?GEN_SERVER_TIMEOUT_MILLIS),

    try
        gen_server:call(?SERVER, is_queue_at_capacity, Timeout)
    catch Error:Reason ->
        lager:warning("Queue monitor timeout ~p ~p ", [Error, Reason]),
        true
    end.

% log that a message has been dropped (hasn't been sent to rabbitmq)
% This is a cast so we don't wait for an `ok` response
% back to the callee, which a call would have returned as well.
message_dropped() ->
    gen_server:cast(?SERVER, message_dropped).


-spec stop() -> ok.
stop() ->
    gen_server:call(?SERVER, stop).


%% Support functions --------------------------------------------
% Synchronously update state by pinging the RabbitMQ Management Console
sync_status_ping() ->
    gen_server:call(?SERVER, sync_status_ping).

% manually toggle the at capacity flag
override_queue_at_capacity(AtCapacity) ->
    gen_server:call(?SERVER, {override_queue_at_capacity, AtCapacity}).

start_timer() ->
    gen_server:call(?SERVER, start_timer).

stop_timer() ->
    gen_server:call(?SERVER, stop_timer).



%%-------------------------------------------------------------
init([Vhost, Queue, MaxLength, CurrentLength]) ->
    % used to catch worker msgs
    process_flag(trap_exit, true),
    TRef = start_update_timer(),
    {ok, #queue_monitor_state{timer=TRef,
                              queue_at_capacity = MaxLength == CurrentLength andalso MaxLength > 0,
                              worker_process = undefined,
                              vhost_to_monitor = Vhost,
                              queue_to_monitor = Queue,
                              max_length = MaxLength,
                              last_recorded_length = CurrentLength}}.

%%-------------------------------------------------------------
%% CALLS
handle_call(is_queue_at_capacity, _From, #queue_monitor_state{queue_at_capacity =
                                                QueueAtCapacity} = State) ->
    {reply, QueueAtCapacity, State};

handle_call({override_queue_at_capacity, AtCapacity}, _From, State) ->
    lager:info("Manually setting Queue Monitor queue_at_capacity ~p", [AtCapacity]),
    {reply, ok, State#queue_monitor_state{queue_at_capacity = AtCapacity}};
handle_call(status, _From, State) ->
    {_, MBoxLength} = erlang:process_info(self(), message_queue_len),
    % return a term to be converted to JSON
    Stats = [{queue_at_capacity,State#queue_monitor_state.queue_at_capacity},
             {dropped_since_last_check, State#queue_monitor_state.dropped_since_last_check},
             {max_length, State#queue_monitor_state.max_length},
             {last_recorded_length, State#queue_monitor_state.last_recorded_length},
             {total_dropped, State#queue_monitor_state.total_dropped},
             {check_count, State#queue_monitor_state.check_count},
             {mailbox_length, MBoxLength}
            ],
    {reply, Stats, State};
handle_call(sync_status_ping, From, #queue_monitor_state{sync_response_process = undefined} = State) ->
     self() ! status_ping,
     {noreply, State#queue_monitor_state{sync_response_process = From}};
handle_call(stop, _From, State) ->
    lager:info("Stopping Queue Monitor"),
    {stop,normal,ok,State};
handle_call(start_timer, _From, #queue_monitor_state{timer = undefined} = State) ->
    TRef = start_update_timer(),
    {reply, ok, State#queue_monitor_state{timer = TRef}};
handle_call(start_timer, _From, State) ->
    lager:info("Queue Monitoring timer already started"),
    {reply, ok, State};
handle_call(stop_timer, _From, #queue_monitor_state{timer = undefined} = State) ->
    lager:info("Queue Monitoring timer already stopped"),
    {reply, ok, State};
handle_call(stop_timer, _From, #queue_monitor_state{timer = Timer} = State) ->
    {ok, cancel} = timer:cancel(Timer),
    {reply, ok, State#queue_monitor_state{timer = undefined}};
handle_call(Request, _From, State) ->
    lager:debug("Unknown request: ~p", [Request]),
    {reply, ignored, State}.


%%-------------------------------------------------------------
%% CASTS
handle_cast(message_dropped, #queue_monitor_state{total_dropped = TotalDropped,
                                    dropped_since_last_check = Dropped} = State) ->
    {noreply, State#queue_monitor_state{total_dropped = TotalDropped + 1,
                          dropped_since_last_check = Dropped + 1}};
handle_cast(Msg, State) ->
    lager:debug("Unknown cast: ~p", [Msg]),
    {noreply, State}.


%%-------------------------------------------------------------
%% INFO
handle_info({MaxLength, reset_dropped_since_last_check}, State) ->
    {noreply,State#queue_monitor_state{max_length = MaxLength,
                                       dropped_since_last_check = 0}};
handle_info({'EXIT', From, Reason},
                            #queue_monitor_state{worker_process = WorkerPid,
                                                sync_response_process = SyncPid,
                                                check_count = CheckCount} = State) ->
    % Check to see if the EXIT came from our worker
    case From == WorkerPid of
        true ->
              % check if we need to reply to a sync_status_ping() call
              case SyncPid of
                  undefined -> ok;
                  Pid -> gen_server:reply(Pid, ok)
              end,
              % clear the worker_process, allowing for future workers to start
              {noreply, State#queue_monitor_state{worker_process = undefined,
                                                  sync_response_process = undefined,
                                                  check_count = CheckCount + 1}};
        false ->
            lager:warning("Unknown process exit detected in Queue Monitor ~p ~p", [From, Reason]),
          {noreply, State}
    end;
handle_info({MaxLength, N, AtCap}, State) ->
    % a successful check of the max queue length and current queue length
    % just update the state and carry on
    {noreply,State#queue_monitor_state{
               max_length = MaxLength,
               last_recorded_length = N,
               queue_at_capacity = AtCap,
               dropped_since_last_check = 0
              }};
% guard against starting more than one worker process via the
% match to worker_process = undefined.
handle_info(status_ping, #queue_monitor_state{
                                worker_process = undefined,
                                dropped_since_last_check = Dropped,
                                vhost_to_monitor = Vhost,
                                queue_to_monitor = Queue} = State) ->
    ParentPid = self(),
    Pid = spawn_link(
            fun () ->
                    Result =
                      chef_wm_rabbitmq_management:check_current_queue_state(
                        oc_chef_action_queue_config:get_rabbit_management_pool_name(),
                        Vhost,
                        Queue,
                        Dropped),
                    ParentPid ! Result
            end),
    {noreply, State#queue_monitor_state{worker_process=Pid}};
handle_info(status_ping, State) ->
    lager:info("Queue monitor check still running, skipping next check"),
    {noreply, State};
handle_info(Info, State) ->
    lager:debug("Unknown info: ~p", [Info]),
    {noreply, State}.

terminate(_Reason, #queue_monitor_state{timer=undefined}) ->
    ok;
terminate(_Reason, #queue_monitor_state{timer=Timer}) ->
    timer:cancel(Timer),
    ok.


code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


%%-------------------------------------------------------------
%% private

% start the timer that updates stats
-spec start_update_timer() -> timer:tref().
start_update_timer() ->
    Interval =
      ?QUEUE_MONITOR_SETTING(queue_length_monitor_millis, ?QUEUE_MONITOR_RUN_EVERY_MILLIS),
    {ok, TRef} = timer:send_interval(Interval, status_ping),
    TRef.





