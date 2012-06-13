%% -*- erlang-indent-level: 4;indent-tabs-mode: nil; fill-column: 92 -*-
%% ex: ts=4 sw=4 et
%% @author Eric B Merritt <ericbmerritt@gmail.com>
%% @copyright Copyright 2012 Opscode, Inc.
-module(bkss_bucket_server).

-behaviour(gen_server).

%% API
-export([start_link/1, bucket_server_exists/1, lock_path/2, unlock_path/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(WAIT_MARKER, '$$__wait_for_me__$$').
-define(WAIT_TIMEOUT, 100000).
-record(state, {bucket_name, store, locks, work_queue}).

%%%===================================================================
%%% Types
%%%===================================================================
-type state() :: record(state).

%%%===================================================================
%%% API
%%%===================================================================

-spec start_link(bookshelf_store:bucket_name()) ->
                        {ok, pid()} | ignore | {error, Error::term()}.
start_link(BucketName) ->
    gen_server:start_link(?MODULE, [BucketName], []).

bucket_server_exists(BucketName) ->
    gproc:where({n,l,BucketName}) =/= undefined.

lock_path(BucketName, Path) ->
    gen_server:cast(gproc:where({n,l,BucketName}), {lock, Path}).

unlock_path(BucketName, Path) ->
    gen_server:cast(gproc:where({n,l,BucketName}), {unlock, Path}).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

-spec init([bookshelf_store:bucket_name()]) -> {ok, state()}.
init([BucketName]) ->
    {ok,DiskStore} = application:get_env(disk_store),
    State1 =
        case bkss_store:bucket_create(bkss_store:new(bkss_fs, DiskStore), BucketName) of
            {State0, ok} ->
                State0;
            {State0, {error,eexist}} ->
                State0
    end,
    gproc:reg({n,l,BucketName}),
    {ok,#state{bucket_name=BucketName, store=State1, locks=[], work_queue=[]}}.

-spec handle_call(Request::term(), From::term(), state()) ->
                         {reply, Reply::term(), state()}.
handle_call(bucket_exists, _From, State = #state{bucket_name = BucketName,
                                                 store = Store}) ->
    {reply,bkss_store:bucket_exists(Store, BucketName), State};
handle_call(bucket_delete, _From, #state{bucket_name = BucketName,
                                         store = Store0}) ->
    {State1, Reply} = bkss_store:bucket_delete(Store0, BucketName),
    {reply, Reply, {die_nicely, State1}, 0};
handle_call(obj_list, From, State = #state{bucket_name = BucketName,
                                          store = Store0}) ->
    bkss_obj_sup:start_child({obj_list, From, [BucketName, Store0]}),
    {noreply, State};
handle_call({obj_exists, Path}, From, State = #state{bucket_name = BucketName,
                                                     store = Store0}) ->
    bkss_obj_sup:start_child({obj_exists, From, [BucketName, Store0, Path]}),
    {noreply, State};
handle_call({obj_delete, Path}, From, State = #state{bucket_name = BucketName,
                                                      store = Store0}) ->
    Work = {obj_delete, From, [BucketName, Store0, Path]},
    {noreply, queue_work(Path, Work, State)};
handle_call({obj_meta, Path}, From, State = #state{bucket_name = BucketName,
                                                      store = Store}) ->
    Work = {obj_meta, From, [BucketName, Store, Path]},
    {noreply, queue_work(Path, Work, State)};
handle_call({obj_create, Path, Data}, From, State = #state{bucket_name = BucketName,
                                                           store = Store}) ->
    Work = {obj_create, From, [BucketName, Store, Path, Data]},
    {noreply, queue_work(Path, Work, State)};
handle_call({obj_get, Path}, From, State = #state{bucket_name = BucketName,
                                                   store = Store}) ->
    Work = {obj_get, From, [BucketName, Store, Path]},
    {noreply, queue_work(Path, Work, State)};
handle_call({obj_copy, FromPath, ToBucket, ToPath}, From, State = #state{bucket_name = BucketName,
                                                                         store = Store}) ->
    Work = {obj_copy, From, [BucketName, Store, FromPath, ToBucket, ToPath]},
    {noreply, queue_work(FromPath, Work, State)};
handle_call({obj_send, Path, Trans}, From, State = #state{bucket_name = BucketName,
                                                          store = Store}) ->
    Work = {obj_send, From, [BucketName, Store, Path, Trans]},
    {noreply, queue_work(Path, Work, State)};
handle_call({obj_recv, Path, Trans, Buffer, Length}, From, State = #state{bucket_name = BucketName,
                                                                           store = Store}) ->
    Work = {obj_recv, From, [BucketName, Store, Path, Trans, Buffer, Length]},
    {noreply, queue_work(Path, Work, State)}.

-spec handle_cast(Msg::term(), state()) ->
                         {noreply, state()}.
handle_cast({lock, Path}, State = #state{locks = Locks}) ->
    {noreply, State#state{locks = [Path | Locks]}};
handle_cast({unlock, Path}, State = #state{locks = Locks0, work_queue = WQ}) ->
    Locks1 = lists:delete(Path, Locks0),
    State1 = case lists:keytake(Path, 1, WQ) of
                {value, {Path, Work}, NewQueue} ->
                    queue_work(Path, Work, State#state{locks = Locks1,
                                                       work_queue=NewQueue});
                false ->
                    State#state{locks = Locks1}
            end,
    {noreply, State1}.


-spec handle_info(Info::term(), {die_nicely, state()}) -> {stop, normal, state()}.
handle_info(timeout, {die_nicely, State}) ->
    %% This is just so we can die nicely in the call
    {stop, normal, State}.

-spec terminate(Reason::term(), state()) -> ok.
terminate(_Reason, _State) ->
    ok.

-spec code_change(OldVsn::term(), state(), Extra::term()) -> {ok, state()}.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
%%====================================================================
%% Internal Functions
%%====================================================================
-spec is_locked(bookshelf_store:path(), state()) -> boolean().
is_locked(Path, #state{locks=Locks}) ->
    lists:member(Path, Locks).

-spec queue_work(bookshelf_store:path(), bkss_obj_worker:work(), state()) -> state().
queue_work(Path, Work, State = #state{locks = Locked,
                                      work_queue = WorkQueue}) ->
    case is_locked(Path, State) of
        false ->
            bkss_obj_sup:start_child(Work),
            State#state{locks = [Path | Locked]};
        true ->
            %% Add the work onto the back, so the we get a FIFO queue
            State#state{work_queue =  WorkQueue ++ [{Path, Work}]}
    end.
