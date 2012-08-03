-module(bksw_coordinator).

-behaviour(gen_server).

%% API
-export([start_link/0,
         reader/1,
         commit/1]).

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
-define(MS_WHICH_READER(Who), {reader, '$1', Who, '$2'}).
-define(MS_WHICH_COMMITTER(Who), {committer, '$1', Who, '$2', '$3'}).
-define(MS_THIS_COMMITTER(Path, Who), {committer, Path, Who, '$1', '$2'}).
-define(ACTIVE_COMMITTER(Committer), Committer#committer.tag == undefined).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

reader(Path) ->
    ?GSCALL({reader, Path}).

commit(Path) ->
    ?GSCALL({committer, Path}).

init([]) ->
    Readers = ets:new(bksw_coord_readers, [duplicate_bag, protected,
                                           {write_concurrency, true},
                                           {read_concurrency, true}]),
    Committers = ets:new(bksw_coord_committers, [duplicate_bag, protected,
                                                 {write_concurrency, true},
                                                 {read_concurrency, true}]),
    {ok, #state{readers=Readers, committers=Committers}}.

handle_call({reader, Path}, {Who, Tag}, #state{readers=Readers}=State) ->
    MRef = erlang:monitor(process, Who),
    case can_read(Path, State) of
        true ->
            case is_duplicate_reader(Path, Who, State) of
                false ->
                    ets:insert(Readers, #reader{path=Path, pid=Who, mref=MRef}),
                    {reply, ok, State};
                true ->
                    {reply, ok, State}
            end;
        false ->
            ets:insert(Readers, #reader{path=Path, pid=Who, tag=Tag, mref=MRef}),
            {noreply, State}
    end;
handle_call({committer, Path}, {Who, Tag}, #state{committers=Committers}=State) ->
    MRef = erlang:monitor(process, Who),
    Reply = case can_write(Path, State) of
                true ->
                    case is_duplicate_committer(Path, Who, State) of
                        false ->
                            ets:insert(Committers, #committer{path=Path, pid=Who, mref=MRef}),
                            ok;
                        true ->
                            ok
                    end;
                false ->
                    ets:insert(Committers, #committer{path=Path, pid=Who, mref=MRef, tag=Tag}),
                    noreply
            end,
    {reply, Reply, State};

handle_call(_Request, _From, State) ->
    {reply, ignored, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(#'DOWN'{obj=Pid}, #state{readers=Readers, committers=Committers}=State) ->
    case find_process(Pid, State) of
        unknown ->
            ok;
        #reader{}=Reader ->
            ets:delete_object(Readers, Reader),
            maybe_write(Reader#reader.path, State);
        #committer{}=Committer ->
            ets:delete_object(Committers, Committer),
            case ?ACTIVE_COMMITTER(Committer) of
                true ->
                    %% Prefer readers over writers
                    case maybe_read(Committer#committer.path, State) of
                        false ->
                            maybe_write(Committer#committer.path, State);
                        true ->
                            ok
                    end;
                false ->
                    ok
            end
    end,
    {noreply, State};
handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% Internal functions
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
                [Committer] ->
                    Committer
            end;
        [Reader] ->
            Reader
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
