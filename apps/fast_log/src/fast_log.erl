%
% License:: Apache License, Version 2.0
%
% Licensed under the Apache License, Version 2.0 (the "License");
% you may not use this file except in compliance with the License.
% You may obtain a copy of the License at
%
%     http://www.apache.org/licenses/LICENSE-2.0
%
% Unless required by applicable law or agreed to in writing, software
% distributed under the License is distributed on an "AS IS" BASIS,
% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
% See the License for the specific language governing permissions and
% limitations under the License.
%
% @author Kevin Smith <kevin@opscode.com>
% @copyright Copyright 2011 Opscode, Inc.
% @version 0.1
% @end
-module(fast_log).

-include("fast_log.hrl").

-behaviour(gen_event).

%% API
-export([start_link/1,
         debug/3,
         debug/4,
         info/3,
         info/4,
         warn/3,
         warn/4,
         err/3,
         err/4]).

%% gen_event callbacks
-export([init/1,
         handle_event/2,
         handle_call/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-record(state, {name,
                log_handle}).

start_link(LogConfig) ->
    Name = proplists:get_value(name, LogConfig),
    {ok, Pid} = gen_event:start_link({local, Name}),
    ok = add_handler(LogConfig),
    {ok, Pid}.

add_handler(LogConfig) ->
    Name = proplists:get_value(name, LogConfig),
    gen_event:add_handler(Name, ?MODULE, [LogConfig]).

debug(Name, Token, Args) ->
    Msg = format_msg("~s ~s", [Token, Args]),
    send_event(Name, {debug, Msg}).

debug(Name, Token, Format0, Args) ->
    Format = "~s " ++ Format0,
    Msg = format_msg(Format, [Token|Args]),
    send_event(Name, {debug, Msg}).

info(Name, Token, Args) ->
    Msg = format_msg("~s ~s", [Token, Args]),
    send_event(Name, {info, Msg}).

info(Name, Token, Format0, Args) ->
    Format = "~s " ++ Format0,
    Msg = format_msg(Format, [Token|Args]),
    send_event(Name, {info, Msg}).

warn(Name, Token, Args) ->
    Msg = format_msg("~s ~s", [Token, Args]),
    send_event(Name, {warn, Msg}).

warn(Name, Token, Format0, Args) ->
    Format = "~s " ++ Format0,
    Msg = format_msg(Format, [Token|Args]),
    send_event(Name, {warn, Msg}).

err(Name, Token, Args) ->
    Msg = format_msg("~s ~s", [Token, Args]),
    send_event(Name, {err, Msg}).

err(Name, Token, Format0, Args) ->
    Format = "~s " ++ Format0,
    Msg = format_msg(Format, [Token|Args]),
    send_event(Name, {err, Msg}).

init([LogConfig]) ->
    Name = proplists:get_value(name, LogConfig),
    FileName = proplists:get_value(file, LogConfig),
    FileSize = proplists:get_value(file_size, LogConfig, 100),
    FileCount = proplists:get_value(files, LogConfig, 3),
    MinLogLevel = proplists:get_value(log_level, LogConfig, ?LOG_DEBUG),
    _Tid = ets:new(Name, [public, named_table]),
    fast_log_util:put_config(Name, #config{min_log=MinLogLevel}),
    {ok, LogHandle} = fast_log_writer:open(Name, FileName, FileCount, FileSize),
    {ok, #state{name=Name, log_handle=LogHandle}}.

handle_event({LogLevel, Msg}, #state{log_handle=LogHandle}=State) ->
    ok = fast_log_writer:write(LogHandle, LogLevel, Msg),
    {ok, State};
handle_event(_Event, State) ->
    {ok, State}.

handle_call(_Request, State) ->
    Reply = ok,
    {ok, Reply, State}.

handle_info(_Info, State) ->
    {ok, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% Internal functions
format_msg(Format, Args) ->
    io_lib:format(Format, Args).

send_event(Name, {LogLevel, _}=Msg) ->
    case fast_log_util:should_log(Name, LogLevel) of
        true ->
            case is_logger_overloaded(Name) of
                true ->
                    ok;
                false ->
                    gen_event:notify(Name, Msg)
            end;
        false ->
            ok
    end.

is_logger_overloaded(Name) ->
    #config{overloaded=Overloaded} = Config = fast_log_util:get_config(Name),
    CheckProb = case Overloaded of
                    true ->
                        1.0;
                    false ->
                        random:uniform()
                end,
    maybe_check(Name, CheckProb, Config, Overloaded).

maybe_check(Name, CheckProb, Config, Overloaded) when CheckProb >= 0.7 ->
    Current = case erlang:process_info(whereis(Name), [message_queue_len]) of
                  [{message_queue_len, Len}] when Len >= 5000 ->
                      true;
                  [{message_queue_len, _Len}] ->
                      false
              end,
    if
        Current =:= Overloaded ->
            Current;
        true ->
            Config1 = Config#config{overloaded=Current},
            fast_log_uutil:put_config(Name, Config1),
            Current
    end;
maybe_check(_Name, _CheckProb, _Config, _Overloaded) ->
    false.
