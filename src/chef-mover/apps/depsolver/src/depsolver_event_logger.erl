%% -*- erlang-indent-level: 4; indent-tabs-mode: nil; fill-column: 80 -*-
%% ex: ts=4 sx=4 et
%%
%% Copyright 2013-2018 Chef Software, Inc.
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
%%
%% @doc Capture depsolver problems and solutions to files for debugging
%%
%% You can use this module to capture the input parameters and result
%% of calls to {@link depsolver:solve/2}. In a running system, you can
%% enable solution logging as follows:
%% ```
%% Config = [{root_dir, "/tmp/depsolver"}, {max_files, 200}].
%% depsolver_event_logger:add_handler(Config).
%% '''
%% 
%% The above will enable the solution event logging. You will find
%% directories `ok', `error', `timeout', and `unreachable' under the
%% specified `root_dir'. Each directory will contain no more than
%% `max_files' files named by number starting at 1. Each file contains
%% the result of `term_to_binary' and records the input graph (as a
%% list), the goals, and the result.
%%
%% The `max_files' parameter is intended to make the logging safe to
%% leave running in that it will not fill the disk. It would be safest
%% to turn off the logging before copying off the log files since a
%% given file could be overwritten at any point while the logger is
%% enabled.
%%
%% You can turn off the event logging like this:
%% ```
%% depsolver_event_logger:remove_handler().
%% '''
%%
%% @end
-module(depsolver_event_logger).

-behaviour(gen_event).


-export([
         add_handler/0,
         add_handler/1,
         log/3,
         remove_handler/0,
         start_link/0
        ]).

%% gen_event callbacks
-export([
         code_change/3,
         handle_call/2, 
         handle_event/2,
         handle_info/2,
         init/1,
         terminate/2
        ]).

-define(SERVER, ?MODULE). 

-define(DIRS, ["ok", "timeout", "unreachable", "error"]).
-define(DEFAULT_MAX_FILES, 200).

-record(state, {
          root_dir = "/tmp/depsolver",
          max_files = 200,
          ok_count = 0,
          timeout_count = 0,
          unreachable_count = 0,
          error_count = 0}).

start_link() ->
    gen_event:start_link({local, ?SERVER}).

add_handler() ->
    gen_event:add_handler(?SERVER, ?MODULE, []).

add_handler(Config) ->
    gen_event:add_handler(?SERVER, ?MODULE, Config).

remove_handler() ->
    gen_event:delete_handler(?SERVER, ?MODULE, []).

log(Graph, Goal, Result) ->
    gen_event:notify(?SERVER, {log_result, {Graph, Goal, Result}}).

init([]) ->
    State = #state{},
    ok = init_dirs(State#state.root_dir),
    {ok, State};
init(Config) ->
    State = #state{
               root_dir = proplists:get_value(root_dir,
                                              Config, "/tmp/depsolver"),
               max_files = proplists:get_value(max_files, Config, 200)},
    ok = init_dirs(State#state.root_dir),
    error_logger:info_msg("depsolver event logging started"),
    {ok, State}.

init_dirs(Root) ->
    Res = [ filelib:ensure_dir(filename:join([Root, Dir, "ensure"]))
            || Dir <- ?DIRS ],
    %% crash if any directory setup failed
    [] = [ R || R <- Res, R =/= ok ],
    ok.

handle_event({log_result, Event}, State) ->
    NewState = log_event(Event, State),
    {ok, NewState};
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

log_event(Event = {_Graph, _Goals, Result}, State) ->
    {LogFile, NewState} = result_path(Result, State),
    ok = write_event(LogFile, Event),
    NewState.

result_path(Result, #state{root_dir = Dir, max_files = Max} = State) ->
    Count = next_counter(get_count(Result, State), Max),
    CountStr = integer_to_list(Count),
    NewState = set_count(Result, State, Count),
    {filename:join([Dir, result_label(Result), CountStr]), NewState}.

write_event(LogFile, {Graph, Goal, Result}) ->
    file:write_file(LogFile,
                    term_to_binary([{graph, Graph}, {goal, Goal}, {result, Result}])).

next_counter(_Max, _Max) ->
    1;
next_counter(Count, Max) when Count < Max ->
    Count + 1.

set_count({ok, _}, State, NewCount) ->
    State#state{ok_count = NewCount};
set_count({error, {unreachable_package, _}}, State, NewCount) ->
    State#state{unreachable_count = NewCount};
set_count({error, resolution_timeout}, State, NewCount) ->
    State#state{timeout_count = NewCount};
set_count(_, State, NewCount) ->
    State#state{error_count = NewCount}.

get_count({ok, _}, #state{ok_count = Count}) ->
    Count;
get_count({error, {unreachable_package, _}}, #state{unreachable_count = Count}) ->
    Count;
get_count({error, resolution_timeout}, #state{timeout_count = Count}) ->
    Count;
get_count(_, #state{error_count = Count}) ->
    Count.

result_label({ok, _}) ->    
    "ok";
result_label({error, {unreachable_package, _}}) ->
    "unreachable";
result_label({error, resolution_timeout}) ->
    "timeout";
result_label(_) ->
    "error".
