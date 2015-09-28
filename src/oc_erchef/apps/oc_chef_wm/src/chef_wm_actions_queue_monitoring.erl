% -*- erlang-indent-level: 4;indent-tabs-mode: nil; fill-column: 92 -*-
%% ex: ts=4 sw=4 et
%%% @author Dave Parfitt <dparfitt@chef.io>
%%% @doc
%%% monitor RabbitMQ length of analytics queues
%%% @end
%% Copyright 2011-2015 Chef Software, Inc. All Rights Reserved.
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

-export([start_link/0]).
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3,
         stop/0,
         status/0,
         is_queue_at_capacity/0,
         message_dropped/0,
         create_pool/0,
         delete_pool/0,
         get_pool_configs/0,
         override_queue_at_capacity/1,
         sync_check_current_state/0,
         start_timer/0,
         stop_timer/0

        ]).

-behaviour(gen_server).
-define(SERVER, ?MODULE).


-record(state, {
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
                % if sync_check_current_state/0 is called, this is the Pid of the
                % calling process. This is recorded because checking the queue
                % length and max is async via the worker_process, and the
                % reply is sent back to this Pid via gen_server:reply()
                sync_response_process = undefined
               }).

% exchange is /analytics, so encode the / as %2F
% NOTE: oc_httpc client is configured to prepend /api
-define(MAX_LENGTH_PATH, "/policies/%2Fanalytics/max_length").
-define(QUEUE_LENGTH_PATH, "/queues/%2Fanalytics").
-define(LOG_THRESHOLD, 0.8).
-define(POOLNAME, rabbitmq_management_service).


start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

-spec status() -> [{atom(), _}].
status() ->
    gen_server:call(?SERVER, status).

% is current queue length >= max queue length?
% Note, this simply checks the gen_server state, and does NOT
% ping the RabbitMQ management console.
% Pinging the management console is handled by an internal timer
% so we never block waiting for an HTTP response.
is_queue_at_capacity() ->
    Timeout = envy:get(oc_chef_wm, rabbitmq_queue_length_timeout_millis, 5000, non_neg_integer),
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
sync_check_current_state() ->
    gen_server:call(?SERVER, sync_check_current_state).

% manually toggle the at capacity flag
override_queue_at_capacity(AtCapacity) ->
    gen_server:call(?SERVER, {override_queue_at_capacity, AtCapacity}).

start_timer() ->
    gen_server:call(?SERVER, start_timer).

stop_timer() ->
    gen_server:call(?SERVER, stop_timer).


%% oc_httpc pool functions --------------------------------------------
create_pool() ->
    Pools = get_pool_configs(),
    [oc_httpc:add_pool(PoolNameAtom, Config) || {PoolNameAtom, Config} <- Pools, Config /= []],
    ok.

delete_pool() ->
    Pools = get_pool_configs(),
    [ok = oc_httpc:delete_pool(PoolNameAtom) || {PoolNameAtom, _Config} <- Pools],
    ok.

get_pool_configs() ->
    Config = envy:get(oc_chef_wm, ?POOLNAME, [], any),
    [{?POOLNAME, Config}].

%%-------------------------------------------------------------

init([]) ->
    % used to catch worker msgs
    process_flag(trap_exit, true),
    TRef = start_update_timer(),
    {ok, #state{timer=TRef, worker_process = undefined}}.


%%-------------------------------------------------------------
%% CALLS
handle_call(is_queue_at_capacity, _From, #state{queue_at_capacity =
                                                QueueAtCapacity} = State) ->
    {reply, QueueAtCapacity, State};

handle_call({override_queue_at_capacity, AtCapacity}, _From, State) ->
    lager:info("Manually setting Queue Monitor queue_at_capacity ~p", [AtCapacity]),
    {reply, ok, State#state{queue_at_capacity = AtCapacity}};
handle_call(status, _From, State) ->
    % return a term to be converted to JSON
    Stats = [{queue_at_capacity,State#state.queue_at_capacity},
             {dropped_since_last_check, State#state.dropped_since_last_check},
             {max_length, State#state.max_length},
             {last_recorded_length, State#state.last_recorded_length},
             {total_dropped, State#state.total_dropped}],
    {reply, Stats, State};
handle_call(sync_check_current_state, From, #state{sync_response_process = undefined} = State) ->
     self() ! status_ping,
     {noreply, State#state{sync_response_process = From}};
handle_call(stop, _From, State) ->
    lager:info("Stopping Queue Monitor"),
    {stop,normal,ok,State};
handle_call(start_timer, _From, #state{timer = undefined} = State) ->
    TRef = start_update_timer(),
    {reply, ok, State#state{timer = TRef}};
handle_call(start_timer, _From, State) ->
    lager:info("Queue Monitoring timer already started"),
    {reply, ok, State};
handle_call(stop_timer, _From, #state{timer = undefined} = State) ->
    lager:info("Queue Monitoring timer already stopped"),
    {reply, ok, State};
handle_call(stop_timer, _From, #state{timer = Timer} = State) ->
    {ok, cancel} = timer:cancel(Timer),
    {reply, ok, State#state{timer = undefined}};
handle_call(Request, _From, State) ->
    lager:debug("Unknown request: ~p", [Request]),
    {reply, ignored, State}.


%%-------------------------------------------------------------
%% CASTS
handle_cast(message_dropped, #state{total_dropped = TotalDropped,
                                    dropped_since_last_check = Dropped} = State) ->
    {noreply, State#state{total_dropped = TotalDropped + 1,
                          dropped_since_last_check = Dropped + 1}};
handle_cast(Msg, State) ->
    lager:debug("Unknown cast: ~p", [Msg]),
    {noreply, State}.


%%-------------------------------------------------------------
%% INFO
handle_info(reset_dropped_since_last_check, State) ->
    {noreply,State#state{dropped_since_last_check = 0}};
handle_info({'EXIT', From, Reason}, #state{worker_process = WorkerPid,
                                           sync_response_process = SyncPid} = State) ->
    % Check to see if the EXIT came from our worker
    case From == WorkerPid of
        true ->
              % check if we need to reply to a sync_check_current_state() call
              case SyncPid of
                  undefined -> ok;
                  Pid -> gen_server:reply(Pid, ok)
              end,
              % clear the worker_process, allowing for future workers to start
              {noreply, State#state{worker_process = undefined,
                                    sync_response_process = undefined}};
        false ->
            lager:warning("Unknown process exit detected in Queue Monitor ~p ~p", [From, Reason]),
          {noreply, State}
    end;
handle_info({MaxLength, N, AtCap}, State) ->
    % a successful check of the max queue length and current queue length
    % just update the state and carry on
    {noreply,State#state{
               max_length = MaxLength,
               last_recorded_length = N,
               queue_at_capacity = AtCap,
               dropped_since_last_check = 0
              }};
% guard against starting more than one worker process via the
% match to worker_process = undefined.
handle_info(status_ping, #state{worker_process = undefined,
                                dropped_since_last_check = Dropped} = State) ->
    ParentPid = self(),
    Pid = spawn_link(fun () -> check_current_queue_state(ParentPid, Dropped) end),
    {noreply, State#state{worker_process=Pid}};
handle_info(status_ping, State) ->
    lager:info("Queue monitor check still running, skipping next check"),
    {noreply, State};
handle_info(Info, State) ->
    lager:debug("Unknown info: ~p", [Info]),
    {noreply, State}.

terminate(_Reason, #state{timer=undefined}) ->
    ok;
terminate(_Reason, #state{timer=Timer}) ->
    {ok, cancel} = timer:cancel(Timer),
    ok.


code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


%%-------------------------------------------------------------
%% private

% start the timer that updates stats
-spec start_update_timer() -> timer:tref().
start_update_timer() ->
    Interval = envy:get(oc_chef_wm, rabbitmq_queue_length_monitor_millis, pos_integer),
    {ok, TRef} = timer:send_interval(Interval, status_ping),
    TRef.

% this function just returns ok, as a return value is communicated back to
% the gen_server via check_current_queue_length
-spec check_current_queue_state(pid(), integer()) -> ok.
check_current_queue_state(ParentPid, DroppedSinceLastCheck) ->
    case get_max_length() of
        undefined -> ok;
                     % max length isn't configured, or something is broken
                     % don't continue.
                     %
        MaxLength ->
            lager:debug("Queue Monitor max length = ~p", [MaxLength]),
            check_current_queue_length(ParentPid, MaxLength,
                                       DroppedSinceLastCheck)
    end.

% this function just returns ok, as a return value is communicated back to
% the gen_server via ParentPid ! Message
-spec check_current_queue_length(pid(), integer(), integer()) -> ok.
check_current_queue_length(ParentPid, MaxLength, DroppedSinceLastCheck) ->
    CurrentLength = get_current_length(),
    case CurrentLength of
        undefined ->
            % a queue doesn't appear to be bound to the /analytics
            % exchange. The only thing we can do is reset the
            % dropped_since_last_check value to 0
            ParentPid ! reset_dropped_since_last_check,
            ok;
        N ->
                lager:debug("Queue Monitor current length = ~p", [N]),
                QueueAtCapacity = CurrentLength == MaxLength,
                {Ratio, Pcnt} = calc_ratio_and_percent(CurrentLength, MaxLength),
                case Ratio >= ?LOG_THRESHOLD of
                    true ->
                        lager:warning("Queue Monitor has detected RabbitMQ capacity at ~p%", [Pcnt]);
                    false -> ok
                end,
                case QueueAtCapacity of
                    true ->
                        lager:warning("Queue Monitor has dropped ~p messages since last check due to queue limit exceeded",
                                        [DroppedSinceLastCheck]);
                    false -> ok
                end,
                % successfully checked max length and current length
                % update the state of the gen_server
                ParentPid ! {MaxLength, N,QueueAtCapacity},
                ok
    end.


-spec calc_ratio_and_percent(integer(), integer()) -> {float(), float()}.
calc_ratio_and_percent(0, _MaxLength) ->
    {0.0, 0.0};
%% the max_length policy should ensure that CurrentLength <= MaxLength,
%% but return 100% full if this ever happens
calc_ratio_and_percent(CurrentLength, MaxLength) when CurrentLength > MaxLength ->
    {1.0, 100.0};
calc_ratio_and_percent(CurrentLength, MaxLength) ->
    Ratio = CurrentLength / MaxLength,
    Pcnt = round(Ratio * 100.0),
    {Ratio, Pcnt}.

-spec rabbit_mgmt_server_request(string()) -> oc_httpc:response().
rabbit_mgmt_server_request(Path) ->
    oc_httpc:request(?POOLNAME, Path, [], get, []).


% make an http connection to the rabbitmq management console
% and return a integer value or undefined
-spec get_max_length() -> integer() | undefined.
get_max_length() ->
    MaxResult = rabbit_mgmt_server_request(?MAX_LENGTH_PATH),
    case MaxResult of
        {ok, "200", _, MaxLengthJson} ->
            parse_max_length_response(MaxLengthJson);
        {error, {conn_failed,_}} ->
            lager:info("Can't connect to RabbitMQ management console"),
            undefined;
        {ok, "404", _, _} ->
            lager:info("RabbitMQ max-length policy not set"),
            undefined;
        Resp -> lager:error("Unknown response from RabbitMQ management console: ~p", [Resp]),
                 undefined

    end.

% make an http connection to the rabbitmq management console
% and return a integer value or undefined
-spec get_current_length() -> integer() | undefined.
get_current_length() ->
    CurrentResult = rabbit_mgmt_server_request(?QUEUE_LENGTH_PATH),
    case CurrentResult of
        {error, {conn_failed,_}} ->
            lager:info("Can't connect to RabbitMQ management console"),
            undefined;
        {ok, "200", _, CurrentStatusJson} ->
            parse_current_length_response(CurrentStatusJson);
        {ok, "404", _, _} ->
            lager:info("Queue not bound in /analytics exchange"),
            undefined;
        Resp ->
            lager:error("Unknown response from RabbitMQ management console: ~p", [Resp]),
                 undefined
    end.

% NOTE: oc_httpc:responseBody() :: string() | {file, filename()}.
% reach into the JSON returned from the RabbitMQ management console
% and return a current length value, OR undefined if unavailable or
% unparseable. EJ was not convenient for parsing this data.
-spec parse_current_length_response(binary() | {file, oc_httpc:filename()}) -> integer() | undefined.
parse_current_length_response(Message) ->
    try
        CurrentJSON = jiffy:decode(Message),
        % make a proplists of each queue and it's current length
        QueueLengths =
            lists:map(fun (QueueStats) -> {QS} = QueueStats,
                                        {proplists:get_value(<<"name">>, QS),
                                        proplists:get_value(<<"messages">>, QS)}
                    end, CurrentJSON),
        % look for the alaska queue length
        parse_integer(proplists:get_value(<<"alaska">>, QueueLengths, undefined))
    catch
        Error:Rsn ->
            lager:error("Invalid RabbitMQ response while getting queue length ~p ~p",
                                 [Error, Rsn]),
            undefined
    end.

% NOTE: oc_httpc:responseBody() :: string() | {file, filename()}.
% reach into the JSON returned from the RabbitMQ management console
% and return the max_length value, OR undefined if unavailable or
% unparseable. EJ was not convenient for parsing this data.
-spec parse_max_length_response(binary() | {file, oc_httpc:filename()}) -> integer() | undefined.
parse_max_length_response(Message) ->
    try
        {MaxLengthPolicy} = jiffy:decode(Message),
        {Defs} = proplists:get_value(<<"definition">>, MaxLengthPolicy),
        parse_integer(proplists:get_value(<<"max-length">>, Defs, undefined))
    catch
        Error:Rsn->
            lager:error("Invalid RabbitMQ response while getting queue max length ~p ~p",
                        [Error, Rsn]),
            undefined
    end.


-spec parse_integer(any()) -> integer | undefined.
parse_integer(Val) when is_integer(Val) ->
    Val;
parse_integer(Val) when is_list(Val) ->
    case string:to_integer(Val) of
        {error, _Reason} -> undefined;
        {Int, _Rest} -> Int
    end;
parse_integer(_) -> undefined.

