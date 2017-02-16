%% Copyright 2017 Chef Software, Inc
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
-module(chef_wait_group).

-behaviour(gen_server).

%% API Exports
-export([start_link/2,
         add/3,
         state/1,
         wait/1
        ]).

%% gen_server Exports
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-record(chef_wait_group_state, {
          waiting_pid :: {pid(), term()},
          waiting_workers = [] :: list(),
          done_jobs = [] :: list(),
          failed_jobs = [] :: list(),
          job_fun :: fun()
         }).

%%===================================================================
%% API
%%===================================================================
%%
%% Example Usage
%%
%% Fun = fun(SomeArgs) -> do_stuff(SomeArgs) end,
%% {ok, Pid} = chef_wait_group:start(Fun, Config)
%% chef_wait_group:add(Pid, JobId0, JobArgs0)
%% chef_wait_group:add(Pid, JobId1, JobArgs1)
%% chef_wait_group:add(Pid, JobId2, JobArgs2)
%% Result = chef_wait_group:wait()
%%

-spec start_link(fun(), proplists:proplist()) -> {ok, pid()} | ignored | {error, term()}.
start_link(Fun, Config) ->
    %% Use start_link/3 here as the caller
    gen_server:start_link(?MODULE, [Fun, Config], []).

-spec add(pid(), term(), list()|term()) -> ok | {error, wait_called}.
add(Pid, JobName, JobArgs) when is_list(JobArgs)->
    gen_server:call(Pid, {add_job, JobName, JobArgs});
add(Pid, JobName, JobArgs) ->
    add(Pid, JobName, [JobArgs]).

-spec wait(pid()) -> {ok, list()} | {error, already_waiting} | {error, list(), list()}.
wait(Pid) ->
    gen_server:call(Pid, gather, infinity).

-spec state(pid()) -> #chef_wait_group_state{}.
state(Pid) ->
    gen_server:call(Pid, get_state).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================
init([Fun, _Config]) ->
    process_flag(trap_exit, true),
    {ok, #chef_wait_group_state{job_fun = Fun}}.

handle_call({add_job, JobName, JobArgs}, _From, State = #chef_wait_group_state{waiting_pid = undefined}) ->
    {ok, State1} = spawn_worker(JobName, JobArgs, State),
    {reply, ok, State1};
%% We don't allow jobs to be added once someone is waiting
handle_call({add_job, _Data}, _From, State = #chef_wait_group_state{waiting_pid = _Pid}) ->
    {reply, {error, wait_called}, State};
%% If someone calls wait and we are already done, return immediately
handle_call(gather, _From, #chef_wait_group_state{waiting_pid = undefined,
                                                  waiting_workers = [],
                                                  done_jobs = Done,
                                                  failed_jobs = Failed}) ->
    {stop, normal, make_gather_reply(Done, Failed), #chef_wait_group_state{}};
%% If someone calls wait and we aren't done, store their pid so we can reply later
handle_call(gather, From, State = #chef_wait_group_state{waiting_pid = undefined,
                                                         waiting_workers = _StillWaiting}) ->
    {noreply, State#chef_wait_group_state{waiting_pid = From}};
%% If someone is already waiting, error
handle_call(gather, _From, State = #chef_wait_group_state{waiting_pid = _Pid}) ->
    {reply, {error, already_waiting}, State};
%% If a job is done, remove it from the waiting list.  If this is the
%% last job, rely to the waiting pid if we have one.
handle_call({job_done, Result}, {Pid, _Tag}, State) ->
    State1 = mark_job_done(Pid, Result, State),
    case maybe_reply_to_waiter(State1) of
        true ->
            {stop, normal, ok, #chef_wait_group_state{}};
        false ->
            {reply, ok, State1}
    end;
handle_call({job_failed, Result}, {Pid, _Tag}, State) ->
    State1 = mark_job_failed(Pid, Result, State),
    case maybe_reply_to_waiter(State1) of
        true ->
            {stop, normal, ok, #chef_wait_group_state{}};
        false ->
            {reply, ok, State1}
    end;
handle_call(get_state, _From, State) ->
    {reply, State, State}.

handle_info({'EXIT', _From, normal}, State) ->
    {noreply, State};
handle_info({'EXIT', From, Reason}, State) ->
    lager:error("Worker ~p failed unexpectedly: ~p", [From, Reason]),
    State1 = mark_job_failed(From, {worker_failed, Reason}, State),
    case maybe_reply_to_waiter(State1) of
        true ->
            {stop, normal, State1};
        false ->
            {noreply, State1}
    end.

handle_cast(_Msg, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
spawn_worker(JobName, JobArgs, State = #chef_wait_group_state{job_fun = Fun,
                                                              waiting_workers = Workers}) ->
    Parent = self(),
    Pid = spawn_link(fun() -> run_user_callback(Fun, JobArgs, Parent) end),
    {ok, State#chef_wait_group_state{waiting_workers = [{Pid, JobName}|Workers]}}.

run_user_callback(Fun, JobArgs, Parent) ->
    try apply(Fun, JobArgs) of
        Res -> gen_server:call(Parent, {job_done, Res})
    catch
        Exception:Reason -> gen_server:call(Parent, {job_failed, {Exception, Reason}})
    end.

-spec maybe_reply_to_waiter(#chef_wait_group_state{}) -> boolean().
maybe_reply_to_waiter(#chef_wait_group_state{waiting_pid = undefined}) ->
    false;
maybe_reply_to_waiter(#chef_wait_group_state{waiting_pid = Pid,
                                             waiting_workers = [],
                                             done_jobs = Done,
                                             failed_jobs = Failed}) ->
    gen_server:reply(Pid, make_gather_reply(Done, Failed)),
    true;
maybe_reply_to_waiter(#chef_wait_group_state{waiting_pid = _Pid,
                                             waiting_workers = _NotEmpty}) ->
    false.

-spec mark_job_done(pid(), term(), #chef_wait_group_state{}) -> #chef_wait_group_state{}.
mark_job_done(Pid, Result, State = #chef_wait_group_state{
                                      waiting_workers = Workers,
                                      done_jobs = DoneList}) ->
    {ok, {_Pid, JobName}, WaitingWorkers} = worker_from_list(Pid, Workers),
    State#chef_wait_group_state{done_jobs = [{JobName, Result}|DoneList],
                                waiting_workers = WaitingWorkers}.

-spec mark_job_failed(pid(), term(), #chef_wait_group_state{}) -> #chef_wait_group_state{}.
mark_job_failed(Pid, Result, State = #chef_wait_group_state{
                                        waiting_workers = Workers,
                                        failed_jobs = FailedList}) ->
    {ok, {_Pid, JobName}, WaitingWorkers} = worker_from_list(Pid, Workers),
    State#chef_wait_group_state{failed_jobs = [{JobName, Result}|FailedList],
                                waiting_workers = WaitingWorkers}.

-spec worker_from_list(pid(), [{pid(), term()}]) -> {ok, {pid(), term()}, [{pid(), term()}]} | {error, no_worker}.
worker_from_list(Pid, Workers) ->
    worker_from_list(Pid, Workers, []).

worker_from_list(_Pid, [], _Acc) ->
    {error, no_worker};
worker_from_list(Pid, [Found = {Pid, _JobName}|Rest], Acc) ->
    {ok, Found, lists:append(Acc, Rest)};
worker_from_list(Pid, [NotFound|Rest], Acc)->
    worker_from_list(Pid, Rest, [NotFound|Acc]).

-spec make_gather_reply(list(), list()) -> {ok, list()} | {error, list(), list()}.
make_gather_reply(Done, []) ->
    {ok, Done};
make_gather_reply(Done, Failed) ->
    {error, Done, Failed}.
