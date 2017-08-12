%% -*- erlang-indent-level: 4;indent-tabs-mode: nil; fill-column: 92 -*-
%% ex: ts=4 sw=4 et
%% @author Mark Anderson <mark@chef.io>
%% Copyright 2017 Chef, Inc. All Rights Reserved.
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

-module(bksw_executor).

-behaviour(gen_server).

%% API
-export([start_link/0,
         add_task/2,
         stats/0,
         error_list/0,
         is_idle/0,
         wait_for_idle/0,
         log_state/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {
          tasks_started = 0 :: integer,
          tasks_executed = 0 :: integer,

          tasks_queued = 0 :: integer,
          task_queue = undefined :: queue:queue(),

          tasks_errored = 0 :: integer,

          max_retries = 3,
          max_tasks = 10,
          active_tasks = #{},

          error_list = []

         }).

%%%===================================================================
%%% API
%%%===================================================================

add_task(Task, Data) ->
    gen_server:call(?MODULE, {add_task, Task, Data}).

stats() ->
    gen_server:call(?MODULE, stats).

error_list() ->
    gen_server:call(?MODULE, error_list).

is_idle() ->
    gen_server:call(?MODULE, is_idle).

wait_for_idle() ->
    case is_idle() of
        false ->
            timer:sleep(1000),
            wait_for_idle();
        _ ->
            ok
    end.

log_state() ->
    gen_server:call(?MODULE, log_state).

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================
init([]) ->
    process_flag(trap_exit, true), %% We don't want to die
    {ok, #state{task_queue = queue:new(),
                active_tasks = #{}
               }}.

handle_call({add_task, Task, Data}, _From, State) ->
    TaskDesc = #{task => Task,
                 data => Data,
                 retries => 0},
    State1 = queue_task(State, TaskDesc),
    State2 = kick_tasks(State1),
    {reply, ok, State2};
handle_call(stats, _From, #state{tasks_started = S,
                                 tasks_executed = E,
                                 tasks_queued = L,
                                 active_tasks = A} = State) ->
    {reply, #{tasks_started => S,
              tasks_executed => E,
              tasks_queued => L,
              active_tasks => A}, State};
handle_call(error_list, _From, #state{error_list = L} = State) ->
    {reply, L, State};
handle_call(is_idle, _From, #state{active_tasks = #{}, tasks_queued = 0} = State) ->
    {reply, true, State};
handle_call(is_idle, _From, #state{} = State) ->
    {reply, false, State};
handle_call(log_state, _From, #state{tasks_started = S,
                                     tasks_executed = E,
                                     tasks_queued = L,
                                     active_tasks = A} = State) ->
    error_logger:info_msg("Executor task status: queued ~p started ~p executed ~p in_flight ~p",
                          [L, S, E, map_size(A)]),
    {reply, ok, State};
handle_call(Msg, _, State) ->
    error_logger:error_msg("Unexpected call ~p", [Msg]),
    {reply, ok, State}.

handle_cast(Msg, State) ->
    error_logger:error_msg("Unexpected cast ~p", [Msg]),
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
%% This would be neater in R19 when we have 'take'
handle_info({'EXIT', Pid, normal}, #state{tasks_executed = E,
                                          active_tasks = Active} = State) ->
    %% One of our linked processes has exited normally
    State1 = case maps:get(Pid, Active, undefined) of
                 undefined ->
                     %% huh, no record of this task, strange
                     error_logger:error_msg("bksw_executor unexpected task exit ~p", [Pid]);
                 _Task ->
                     Active2 = maps:remove(Pid, Active),
                     State#state{tasks_executed = E+1,
                                 active_tasks = Active2}
             end,
    State2 = kick_tasks(State1),
    {noreply, State2};
handle_info({'EXIT', Pid, Error}, #state{tasks_executed = Execs,
                                         tasks_errored = Errors,
                                         active_tasks = Active,
                                         max_retries = MaxRetries,
                                         error_list = ErrorList} = State) ->
    %% One of our linked processes has exited with an error
    error_logger:error_msg("bksw_executor unexpected exit ~p ~p~n~p", [Pid, Error, State]),
    State1 = case maps:get(Pid, Active, undefined) of
                 error ->
                     %% huh, no record of this task, strange
                     error_logger:error_msg("bksw_executor unexpected task exit ~p", [Pid]);
                 Task ->
                     Active2 = maps:remove(Pid, Active),
                     case Task of
                         #{retries := R} when R < MaxRetries ->
                             %% Do a retry
                             queue_task(State#state{tasks_executed = Execs+1,
                                                    tasks_errored = Errors+1,
                                                    active_tasks = Active2},
                                        Task#{retries => R+1} );
                         _ ->
                             %% no more retires, mark as an error
                             State#state{tasks_executed = Execs+1,
                                         tasks_errored = Errors+1,
                                         active_tasks = Active2,
                                         error_list = [{Task, Error} | ErrorList]}
                     end
             end,
    State2 = kick_tasks(State1),
    {noreply, State2};
handle_info(_Info, State) ->
    error_logger:error_msg("Unexpected info ~p", [_Info]),
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

%% Add a task to the task queue
queue_task(#state{tasks_queued = L, task_queue = Q} = State, TaskDesc) ->
    State#state{tasks_queued = L+1, task_queue = queue:in(TaskDesc, Q) }.

%% If we have an available slot, remove a task from the task queue and run it
kick_tasks(#state{tasks_started = S, tasks_queued = L, task_queue = Q,
                  active_tasks = Active, max_tasks = MaxTasks} = State)
  when map_size(Active) < MaxTasks ->
    case queue:out(Q) of
        %% get a task to start
        {{value, #{task := Task, data := Data} = TaskDesc}, Q1} ->
            %% Start a task
            Pid = spawn_link(fun() -> Task(Data) end),

            %% We may have multiple open slots available, so recurse
            kick_tasks(State#state{tasks_started = S + 1,
                                   tasks_queued = L - 1,
                                   task_queue = Q1,
                                   active_tasks = maps:put(Pid, TaskDesc, Active) });
        {empty, Q} ->
            %% Nothing to do
            State
    end;
kick_tasks(State) ->
    State.
