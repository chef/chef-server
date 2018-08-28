% -*- erlang-indent-level: 4;indent-tabs-mode: nil; fill-column: 92 -*-
%% ex: ts=4 sw=4 et
%%% @author Dave Parfitt <dparfitt@chef.io>
%% Copyright 2015-2018 Chef Software, Inc.
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

%% Not the most interesting quickcheck property.
%% simulate messages dropped, no queue bound, and
%% "successful" queue checks (where the management console
%% returns max_length and current length).

-module(chef_wm_actions_queue_monitoring_statem).

-behaviour(proper_statem).

-export([queue_monitor_prop/0]).
-export([initial_state/0, command/1, precondition/2, postcondition/3, next_state/3]).

-include_lib("proper/include/proper.hrl").

-record(state, {total_dropped = 0,
                dropped_since_last_check = 0}).


-define(VHOST, "/analytics").
-define(QUEUE_NAME, "alaska").



default_rabbit_config() ->
    [
        {management,
        [{user, <<"rabbitmgmt">>},
            {port, 15672},
            {password, <<"chef123">>},
            % rabbitmq management http connection pool
            {rabbitmq_actions_management_service,
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



queue_monitor_prop() ->
    ?FORALL(Commands, commands(?MODULE),
	    begin
            application:set_env(oc_chef_wm, rabbitmq, default_rabbit_config()),
            chef_wm_actions_queue_monitoring:start_link(?VHOST, ?QUEUE_NAME, 0, 0),
            chef_wm_actions_queue_monitoring:stop_timer(),
            {H,S,Res} = run_commands(?MODULE, Commands),
            clean_up(),
            ?WHENFAIL(
              io:format("History: ~w~nState: ~w~nRes: ~w~n", [H, S, Res]),
              aggregate(command_names(Commands), Res =:= ok))
	    end).

clean_up() ->
    catch(chef_wm_actions_queue_monitoring:stop()),
    ok.

send_msg(Msg) ->
    Pid = whereis(chef_wm_actions_queue_monitoring),
    Pid ! Msg.

reset_dropped_since_last_check(MaxLength) ->
    send_msg({MaxLength, reset_dropped_since_last_check}).

initial_state() -> #state{}.

at_cap(N, MaxLength) ->
    N >= MaxLength.

command(_Props) ->
    ?LET({MaxLength,N},
         {integer(), integer()},
             oneof([
                    {call, ?MODULE, send_msg,[{MaxLength, N, at_cap(N, MaxLength)}]},
                    {call, chef_wm_actions_queue_monitoring, message_dropped,[]},
                    {call, ?MODULE, reset_dropped_since_last_check, [MaxLength]}
                   ])).

% any command can run at any time
precondition(_, _) ->
    true.

postcondition(_State, {call, ?MODULE, send_msg, [{MaxLength, N, AtCap}]}, _Result) ->
    Status = chef_wm_actions_queue_monitoring:status(),
    MaxLength = proplists:get_value(max_length, Status),
    N = proplists:get_value(last_recorded_length, Status),
    AtCap = proplists:get_value(queue_at_capacity, Status),
    true;
postcondition(State, {call, chef_wm_actions_queue_monitoring, message_dropped, []}, _Val) ->
    Status = chef_wm_actions_queue_monitoring:status(),
    N = proplists:get_value(dropped_since_last_check, Status),
    Total = proplists:get_value(total_dropped, Status),
    State#state.total_dropped == (Total - 1)
      andalso
    State#state.dropped_since_last_check == (N - 1);
postcondition(_State, {call, ?MODULE, reset_dropped_since_last_check, [MaxLength]}, _Result) ->
    Status = chef_wm_actions_queue_monitoring:status(),
    0 == proplists:get_value(dropped_since_last_check, Status)
      andalso
    MaxLength == proplists:get_value(max_length, Status);
postcondition(_, Call, Result) ->
    io:format(user, "Unmatched postcondition: ~p ~n    ~p~n", [Call, Result]),
    false.

next_state(State, _Var, {call, ?MODULE, reset_dropped_since_last_check, [_MaxLength]}) ->
    State#state{dropped_since_last_check = 0};
next_state(State, _Var, {call, ?MODULE, send_msg, _}) ->
    State#state{dropped_since_last_check = 0};
next_state(#state{total_dropped = Dropped, dropped_since_last_check = LastDropped} = State,
           _Var, {call, chef_wm_actions_queue_monitoring, message_dropped, []}) ->
    State#state{total_dropped = Dropped + 1, dropped_since_last_check = LastDropped + 1};
next_state(State, Res, Call) ->
    io:format(user, "Unmatched next_state ~p ~p ~p~n", [State, Res, Call]),
    State.
