%% Copyright 2012 Opscode, Inc. All Rights Reserved.
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

-module(bksw_coordinator).

-behaviour(gen_server).

%% API
-export([start_link/0,
         start_read/1,
         end_read/1,
         end_read/2,
         commit/1,
         end_commit/1,
         end_commit/2,
         path_stats/1]).

-record(reader, {path,
                 pid,
                 tag,
                 mref}).

-record(committer, {path,
                    pid,
                    tag,
                    mref}).

-record(state, {readers,
                committers}).

-record('DOWN', {mref,
                 type,
                 obj,
                 info}).

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-define(SERVER, ?MODULE).
-define(GSCALL(Msg), gen_server:call(?MODULE, Msg, 30000)).
-define(MS_ALL_COMMITTERS(Path), {committer, Path, '$1', '_', '_'}).
-define(MS_ALL_READERS(Path), {reader, Path, '$1', '$2', '$3'}).
-define(MS_THIS_READER(Path, Who), {reader, Path, Who, '$1', '_'}).
-define(MS_WHICH_READER(Who), {reader, '$1', Who, '$2', '$3'}).
-define(MS_WHICH_COMMITTER(Who), {committer, '$1', Who, '$2', '$3'}).
-define(MS_THIS_COMMITTER(Path, Who), {committer, Path, Who, '$1', '$2'}).
-define(ACTIVE_COMMITTER(Committer), Committer#committer.tag == undefined).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

start_read(Path) ->
    ?GSCALL({start_read, Path}).

end_read(Path) ->
    end_read(self(), Path).

end_read(ReaderPid, Path) ->
    ?GSCALL({end_read, ReaderPid, Path}).

commit(Path) ->
    ?GSCALL({start_commit, Path}).

end_commit(Path) ->
    end_commit(self(), Path).

end_commit(Committer, Path) ->
    ?GSCALL({end_commit, Committer, Path}).

path_stats(Path) ->
    ?GSCALL({stats, Path}).

init([]) ->
    Readers = ets:new(bksw_coord_readers, [duplicate_bag, protected,
                                           {write_concurrency, true},
                                           {read_concurrency, true}]),
    Committers = ets:new(bksw_coord_committers, [duplicate_bag, protected,
                                                 {write_concurrency, true},
                                                 {read_concurrency, true}]),
    {ok, #state{readers=Readers, committers=Committers}}.

handle_call({stats, Path}, _From, #state{readers=Readers, committers=Writers}=State) ->
    RCount = length(fetch_all(Readers, ?MS_ALL_READERS(Path))),
    WCount = length(fetch_all(Writers, ?MS_ALL_COMMITTERS(Path))),
    {reply, [{readers, RCount}, {writers, WCount}], State};

handle_call({start_read, Path}, {Who, Tag}, #state{readers=Readers}=State) ->
    case is_duplicate_reader(Path, Who, State) of
        true ->
            error_logger:info_msg("~p is a dupe reader for ~p~n", [Who, Path]),
            {reply, ok, State};
        false ->
            MRef = erlang:monitor(process, Who),
            case can_read(Path, State) of
                true ->
                    ets:insert(Readers, #reader{path=Path, pid=Who, mref=MRef}),
                    {reply, ok, State};
                false ->
                    ets:insert(Readers, #reader{path=Path, pid=Who, tag=Tag, mref=MRef}),
                    {noreply, State}
            end
    end;
handle_call({end_read, Who, Path}, _From, #state{readers=Readers}=State) ->
    Reply = case ets:match_object(Readers, ?MS_THIS_READER(Path, Who)) of
                [] ->
                    ok;
                [#reader{tag=undefined, mref=MRef}=Reader] ->
                    erlang:demonitor(MRef, [flush]),
                    ets:delete_object(Readers, Reader),
                    ok;
                [#reader{tag=Tag, mref=MRef}=Reader] ->
                    erlang:demonitor(MRef, [flush]),
                    ets:delete_object(Readers, Reader),
                    gen_server:reply({Who, Tag}, aborted),
                    ok
            end,
    {reply, Reply, State};
handle_call({start_commit, Path}, {Who, Tag}, #state{committers=Committers}=State) ->
    case is_duplicate_committer(Path, Who, State) of
        true ->
            {reply, ok, State};
        false ->
            MRef = erlang:monitor(process, Who),
            case can_write(Path, State) of
                true ->
                    ets:insert(Committers, #committer{path=Path, pid=Who, mref=MRef}),
                    {reply, ok, State};
                false ->
                    ets:insert(Committers, #committer{path=Path, pid=Who, mref=MRef, tag=Tag}),
                    {noreply, State}
            end
    end;
handle_call({end_commit, Who, Path}, _From, #state{committers=Committers}=State) ->
    Reply = case ets:match_object(Committers, ?MS_THIS_COMMITTER(Path, Who)) of
                [] ->
                    ok;
                [#committer{tag=undefined, mref=MRef}=Committer] ->
                    erlang:demonitor(MRef, [flush]),
                    ets:delete_object(Committers, Committer),
                    ok;
                [#committer{tag=Tag, mref=MRef}=Committer] ->
                    erlang:demonitor(MRef, [flush]),
                    ets:delete_object(Committers, Committer),
                    gen_server:reply({Who, Tag}, aborted),
                    ok
            end,
    {reply, Reply, State};
handle_call(_Request, _From, State) ->
    {reply, ignored, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(#'DOWN'{obj=Pid}, State) ->
    case find_process(Pid, State) of
        unknown ->
            ok;
        Records ->
            dispatch_pending(Records, State)
    end,
    {noreply, State};
handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% Internal functions
dispatch_pending([], _State) ->
    ok;
dispatch_pending([#reader{}=H|T], #state{readers=Readers}=State) ->
    ets:delete_object(Readers, H),
    maybe_write(H#reader.path, State),
    dispatch_pending(T, State);
dispatch_pending([#committer{}=H|T], #state{committers=Committers}=State) ->
    ets:delete_object(Committers, H),
    case ?ACTIVE_COMMITTER(H) of
        true ->
            %% Prefer readers over writers
            case maybe_read(H#committer.path, State) of
                false ->
                    maybe_write(H#committer.path, State);
                true ->
                    ok
            end;
        false ->
                    ok
    end,
    dispatch_pending(T, State).

maybe_write(Path, #state{readers=Readers, committers=Committers}) ->
    maybe_write(Path, has_none(Readers, ?MS_ALL_READERS(Path)), Committers).

maybe_read(Path, #state{readers=Readers}) ->
    case fetch_all(Readers, ?MS_ALL_READERS(Path)) of
        [] ->
            false;
        PendingReaders ->
            [notify_reader(Readers, PendingReader) || PendingReader <- PendingReaders],
            true
    end.

%% Notify any waiting reader but skip over those already running
notify_reader(Readers, #reader{pid=Who, tag=Tag}=Reader) when Tag /= undefined ->
    Reader1 = Reader#reader{tag=undefined},
    ets:delete_object(Readers, Reader),
    ets:insert(Readers, Reader1),
    gen_server:reply({Who, Tag}, ok).

maybe_write(_Path, false, _Committers) ->
    ok;
maybe_write(Path, true, Committers) ->
    maybe_run_committer(fetch_all(Committers, ?MS_ALL_COMMITTERS(Path)), Committers).

maybe_run_committer([], _Committers) ->
    ok;
maybe_run_committer([Committer], Committers) ->
    run_committer(Committer, Committers),
    ok;
maybe_run_committer(AllCommitters, Committers) when is_list(AllCommitters) ->
    RPos = random:uniform(length(AllCommitters)),
    Committer = lists:nth(RPos, AllCommitters),
    run_committer(Committer, Committers),
    ok.

run_committer(#committer{pid=Pid, tag=Tag}=Committer, Committers) when Tag /= undefined ->
    gen_server:reply({Pid, Tag}, ok),
    ets:delete_object(Committers, Committer),
    Committer1 = Committer#committer{tag=undefined},
    ets:insert(Committers, Committer1).

find_process(Pid, #state{readers=Readers, committers=Committers}) ->
    case ets:match_object(Readers, ?MS_WHICH_READER(Pid)) of
        [] ->
            case ets:match_object(Committers, ?MS_WHICH_COMMITTER(Pid)) of
                [] ->
                    unknown;
                C ->
                    C
            end;
        R ->
            R
    end.

can_write(Path, #state{readers=Readers, committers=Committers}) ->
    has_none(Readers, ?MS_ALL_READERS(Path)) andalso
        has_none(Committers, ?MS_ALL_COMMITTERS(Path)).

can_read(Path, #state{committers=Committers}) ->
    has_none(Committers, ?MS_ALL_COMMITTERS(Path)).


is_duplicate_reader(Path, Reader, #state{readers=Readers}) ->
    has_one(Readers, ?MS_THIS_READER(Path, Reader)).

is_duplicate_committer(Path, Committer, #state{committers=Committers}) ->
    has_one(Committers, ?MS_THIS_COMMITTER(Path, Committer)).

fetch_all(Tid, Ms) ->
    ets:match_object(Tid, Ms).

has_one(Tid, Ms) ->
    has_record_count(Tid, Ms, 1).

has_none(Tid, Ms) ->
    has_record_count(Tid, Ms, 0).

has_record_count(Tid, Ms, 0) ->
    case ets:match_object(Tid, Ms, 1) of
        '$end_of_table' ->
            true;
        _ ->
            false
    end;
has_record_count(Tid, Ms, Count) ->
    case ets:match_object(Tid, Ms, Count) of
        '$end_of_table' ->
            false;
        {Objs, _} ->
            length(Objs) == Count
    end.
