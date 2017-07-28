% -*- erlang-indent-level: 4;indent-tabs-mode: nil; fill-column: 92 -*-
%% ex: ts=4 sw=4 et
%%% @author Dave Parfitt <dparfitt@chef.io>
%%% @doc
%%% RabbitMQ queue length management utils
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

-module(chef_wm_rabbitmq_management).

-ifdef(TEST).
-compile(export_all).
-endif.

-export([calc_ratio_and_percent/2,
         get_max_length/1,
         get_current_length/2,
         create_pool/0,
         delete_pool/0,
         get_pool_configs/0,
         check_current_queue_state/3,
         check_current_queue_length/4,
         sync_check_queue_at_capacity/2,
         get_rabbit_management_setting/2,
         get_rabbit_queue_monitor_setting/2,
         set_rabbit_management_setting/2,
         set_rabbit_queue_monitor_setting/2,
         set_app_value/5,
         check_aliveness/1
        ]).


-define(POOLNAME, rabbitmq_management_service).
-define(LOG_THRESHOLD, 0.8).

-type max_length() :: integer().
-type current_length() :: integer().
-type queue_at_capacity() :: boolean.


% NOTE: oc_httpc client is configured to prepend /api

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
    ServiceConfig = get_rabbit_management_setting(rabbitmq_management_service, []),
    [{?POOLNAME, ServiceConfig}].

get_rabbit_management_setting(Key, Default) ->
    try
        RabbitConfig = envy:get(oc_chef_wm, rabbitmq, [], any),
        MgmtConfig = proplists:get_value(management, RabbitConfig, []),
        Config = proplists:get_value(Key, MgmtConfig, Default),
        add_auth_to_ibrowse_options(Config, MgmtConfig)
    catch Error:Reason ->
        lager:info("Can't get configuration setting ~p ~p: ~p ~p",
                   [Key, Default, Error, Reason]),
        Default
    end.

add_auth_to_ibrowse_options(Config, MgmtConfig) ->
    Username = proplists:get_value(user, MgmtConfig),
    {ok, Password} = chef_secrets:get(<<"rabbitmq">>, <<"management_password">>),
    IbrowseOptions = proplists:get_value(ibrowse_options, Config),
    Config1 = proplists:delete(ibrowse_options, Config),
    IbrowseOptions1 = [{basic_auth, {Username, erlang:binary_to_list(Password)}} | IbrowseOptions],
    [{ibrowse_options, IbrowseOptions1} | Config1].

get_rabbit_queue_monitor_setting(Key, Default) ->
    try
        RabbitConfig = envy:get(oc_chef_wm, rabbitmq, [], any),
        MonitoringConfig = proplists:get_value(monitoring, RabbitConfig, []),
        proplists:get_value(Key, MonitoringConfig, Default)
    catch Error:Reason ->
        lager:info("Can't get configuration setting ~p ~p: ~p ~p",
                   [Key, Default, Error, Reason]),
        Default
    end.

set_rabbit_management_setting(Key, NewVal) ->
    set_app_value(oc_chef_wm, rabbitmq, management, Key, NewVal).

set_rabbit_queue_monitor_setting(Key, NewVal) ->
    set_app_value(oc_chef_wm, rabbitmq, monitoring, Key, NewVal).

%% value MUST already exist in the dict
set_app_value(App, ConfigKey, SubSectionKey, Prop, NewValue) ->
    Cfg = envy:get(App, ConfigKey, [], any),
    NewCfg =
      lists:map(fun ({SectionKey,SectionValue})
                          when SectionKey == SubSectionKey ->
                            {SectionKey,
                            lists:map(fun ({K, _V}) when K == Prop ->
                                                {K, NewValue};
                                            (Val) -> Val
                                        end, SectionValue)
                            };
                    (Val) -> Val
           end, Cfg),
      application:set_env(App, ConfigKey, NewCfg).


-spec calc_ratio_and_percent(integer(), integer()) -> {float(), float()}.
calc_ratio_and_percent(0, _MaxLength) ->
    {0.0, 0.0};
%% the max_length policy should ensure that CurrentLength <= MaxLength,
%% but return 100% full if this ever happens
calc_ratio_and_percent(CurrentLength, MaxLength) when CurrentLength >= MaxLength ->
    {1.0, 100.0};
calc_ratio_and_percent(CurrentLength, MaxLength) ->
    Ratio = CurrentLength / MaxLength,
    Pcnt = round(Ratio * 100.0),
    {Ratio, Pcnt}.

-spec rabbit_mgmt_server_request(string()) -> oc_httpc:response().
rabbit_mgmt_server_request(Path) ->
    oc_httpc:request(?POOLNAME, Path, [], get, []).

-spec mk_max_length_path(string()) -> string().
mk_max_length_path(Vhost) ->
    lists:flatten(io_lib:format("/policies/~s/max_length",
                                [http_uri:encode(Vhost)])).

-spec mk_current_length_path(string()) -> string().
mk_current_length_path(Vhost) ->
    lists:flatten(io_lib:format("/queues/~s", [http_uri:encode(Vhost)])).

-spec mk_aliveness_check_path(string()) -> string().
mk_aliveness_check_path(Vhost) ->
    lists:flatten(io_lib:format("/aliveness-test/~s", [http_uri:encode(Vhost)])).

% make an http connection to the rabbitmq management console
% and return a integer value or undefined
-spec get_max_length(string()) -> integer() | undefined.
get_max_length(Vhost) ->
    MaxResult = rabbit_mgmt_server_request(mk_max_length_path(Vhost)),
    case MaxResult of
        {ok, "200", _, MaxLengthJson} ->
            parse_max_length_response(MaxLengthJson);
        {error, {conn_failed,_}} ->
            lager:info("Can't connect to RabbitMQ management console to fetch max-length"),
            undefined;
        {ok, "404", _, _} ->
            lager:info("RabbitMQ max-length policy not set"),
            undefined;
        Resp ->
            lager:error("Unknown response from RabbitMQ management console: ~p", [Resp]),
            undefined

    end.

% make an http connection to the rabbitmq management console
% and return a integer value or undefined
-spec get_current_length(string(), string()) -> integer() | undefined.
get_current_length(Vhost, Queue) ->
    CurrentResult = rabbit_mgmt_server_request(mk_current_length_path(Vhost)),
    case CurrentResult of
        {error, {conn_failed,_}} ->
            lager:info("Can't connect to RabbitMQ management console to fetch current length"),
            undefined;
        {ok, "200", _, CurrentStatusJson} ->
            parse_current_length_response(CurrentStatusJson, Queue);
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
-spec parse_current_length_response(binary() | {file, oc_httpc:filename()},
                                    string()) -> integer() | undefined.
parse_current_length_response(Message, Queue) ->
    try
        CurrentJSON = jiffy:decode(Message),
        % make a proplists of each queue and it's current length
        QueueLengths =
            lists:map(fun (QueueStats) -> {QS} = QueueStats,
                                        {proplists:get_value(<<"name">>, QS),
                                        proplists:get_value(<<"messages">>, QS)}
                    end, CurrentJSON),

        QBin = list_to_binary(Queue),
        parse_integer(proplists:get_value(QBin, QueueLengths, undefined))
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


-spec sync_check_queue_at_capacity(string(), string()) -> {integer(), integer(), boolean()}.
sync_check_queue_at_capacity(Vhost, Queue) ->
    Result = check_current_queue_state(Vhost, Queue, 0),
    case Result of
      skipped  -> {0, 0, false};
      {MaxLength, reset_dropped_since_last_check} -> {MaxLength, 0, false};
      {MaxLength, N, QueueAtCapacity} -> {MaxLength, N, QueueAtCapacity}
    end.



-spec check_current_queue_state(string(), string(), integer()) ->
                                                skipped |
                                                {max_length(), reset_dropped_since_last_check} |
                                                {max_length(), current_length(), queue_at_capacity()}.
check_current_queue_state(Vhost, Queue, DroppedSinceLastCheck) ->
    case chef_wm_rabbitmq_management:get_max_length(Vhost) of
        undefined -> skipped;
                     % max length isn't configured, or something is broken
                     % don't continue.
        MaxLength ->
            lager:debug("Queue Monitor max length = ~p", [MaxLength]),
            check_current_queue_length(Vhost,
                                       Queue,
                                       MaxLength,
                                       DroppedSinceLastCheck)
    end.

-spec check_current_queue_length(string(), string(), integer(), integer()) ->
                                                {max_length(), reset_dropped_since_last_check} |
                                                {max_length(), current_length(), queue_at_capacity()}.
check_current_queue_length(Vhost, Queue, MaxLength, DroppedSinceLastCheck) ->
    % use ?MODULE here so I can use meck in integrations testing
    % https://github.com/eproxus/meck/issues/142
    CurrentLength = ?MODULE:get_current_length(Vhost, Queue),
    case CurrentLength of
        undefined ->
            % a queue doesn't appear to be bound to the
            % exchange. The only thing we can do is reset the
            % dropped_since_last_check value to 0
            {MaxLength, reset_dropped_since_last_check};
        N ->
                lager:info("Queue Monitor current length = ~p for VHost ~p and Queue ~p", [N, Vhost, Queue]),
                QueueAtCapacity = CurrentLength == MaxLength,
                {Ratio, Pcnt} = chef_wm_rabbitmq_management:calc_ratio_and_percent(CurrentLength, MaxLength),
                case Ratio >= ?LOG_THRESHOLD of
                    true ->
                        lager:warning("Queue Monitor has detected RabbitMQ for VHost ~p, queue ~p capacity at ~p%",
                                      [Vhost, Queue, Pcnt]);
                    false -> ok
                end,
                case QueueAtCapacity of
                    true ->
                        lager:warning("Queue Monitor has dropped ~p messages for VHost ~p, queue ~p since last check due to queue limit exceeded",
                                        [DroppedSinceLastCheck, Vhost, Queue]);
                    false -> ok
                end,
                % successfully checked max length and current length
                % update the state of the gen_server
                {MaxLength, N, QueueAtCapacity}
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

-spec check_aliveness(string()) -> boolean().
check_aliveness(Vhost) ->
    Aliveness = rabbit_mgmt_server_request(mk_aliveness_check_path(Vhost)),
    case Aliveness of
        {ok, "200", _, _} ->
            true;
        Resp ->
            lager:error("Error getting Rabbitmq aliveness: ~p", Resp),
            false
    end.
