%%%-------------------------------------------------------------------
%%% @author Eric B Merritt <ericbmerritt@gmail.com>
%%% @copyright (C) 2012, Opscode, Inc
%%% @doc
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(bkss_bucket_server).

-behaviour(gen_server).

%% API
-export([start_link/1, bucket_server_exists/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).
-include("internal.hrl").

-define(WAIT_MARKER, '$$__wait_for_me__$$').
-define(WAIT_TIMEOUT, 100000).

%%%===================================================================
%%% Types
%%%===================================================================
-type state() ::{bookshelf_store:bucket_name(), bkss_store:store()}.

%%%===================================================================
%%% API
%%%===================================================================

-spec start_link(bookshelf_store:bucket_name()) ->
                        {ok, pid()} | ignore | {error, Error::term()}.
start_link(BucketName) ->
    gen_server:start_link(?MODULE, [BucketName], []).

bucket_server_exists(BucketName) ->
    gproc:where({n,l,BucketName}) =/= undefined.

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

-spec init([bookshelf_store:bucket_name()]) -> {ok, state()}.
init([BucketName]) ->
    {ok,DiskStore} = opset:get_value(disk_store, ?BOOKSHELF_CONFIG),
    State1 =
        case bkss_store:bucket_create(bkss_store:new(bkss_fs, DiskStore), BucketName) of
            {State0, ok} ->
                State0;
            {State0, {error,eexist}} ->
                State0
    end,
    gproc:reg({n,l,BucketName}),
    opset:link_config(?BOOKSHELF_CONFIG),
    {ok,{BucketName, State1}}.

-spec handle_call(Request::term(), From::pid(), state()) ->
                         {reply, Reply::term(), state()} |
                         {reply, Reply::term(), state(), Timeout::non_neg_integer()}.
handle_call(bucket_list, _From, State={_, Store}) ->
    {reply,bkss_store:bucket_list(Store), State};
handle_call(bucket_exists, _From, State={BucketName, Store}) ->
    {reply,bkss_store:bucket_exists(Store, BucketName), State};
handle_call(bucket_delete, _From, {BucketName, Store0}) ->
    {State1, Reply} = bkss_store:bucket_delete(Store0, BucketName),
    {reply, Reply, {die_nicely, State1}, 0};
handle_call(obj_list, _From, State={BucketName, Store}) ->
    {reply, bkss_store:obj_list(Store, BucketName), State};
handle_call({obj_exists, Path}, _From, State={BucketName, Store}) ->
    {reply, bkss_store:obj_exists(Store, BucketName, Path), State};
handle_call({obj_delete, Path}, _From, {BucketName, Store0}) ->
    {Store1, Reply} = bkss_store:obj_delete(Store0, BucketName, Path),
    {reply, Reply, {BucketName, Store1}};
handle_call({obj_meta, Path}, _From, State={BucketName, Store}) ->
    {reply, bkss_store:obj_meta(Store, BucketName, Path), State};
handle_call({obj_create, Path, Data}, _From, {BucketName, Store0}) ->
    {Store1, Reply} = bkss_store:obj_create(Store0, BucketName, Path, Data),
    {reply, Reply, {BucketName, Store1}};
handle_call({obj_get, Path}, _From, State={BucketName, Store}) ->
    {reply, bkss_store:obj_get(Store, BucketName, Path), State};
handle_call({obj_copy, FromPath, ToBucket, ToPath}, _From, {BucketName, Store0}) ->
    {Store1, Result} = do_copy(Store0, BucketName, FromPath, ToBucket, ToPath),
    {reply, Result, {BucketName, Store1}};
handle_call({obj_send, Path, Bridge}, _From, {BucketName, Store0}) ->
    {Store1, Reply} = bkss_store:obj_send(Store0, BucketName, Path, Bridge),
    {reply, Reply, {BucketName, Store1}};
handle_call({obj_recv, Path, Bridge, Buffer, Length}, _From, {BucketName, Store0}) ->
    {Store1, Reply} = bkss_store:obj_recv(Store0, BucketName, Path,
                                          Bridge, Buffer, Length),
    {reply, Reply, {BucketName, Store1}};
handle_call({wait, RecPid, Ref}, _From, State) ->
    %% We got a wait call and that returned a timeout. A wait call is
    %% the only way we should enter this. This is mostly designed to
    %% guard this bucket from updates while another bucket is doing
    %% work inside. Here we just wait on the message

    %% Let the other bucket know we are waiting
    RecPid ! {?WAIT_MARKER, waiting, Ref},
    receive
        {?WAIT_MARKER, done, Ref} -> ok
    after ?WAIT_TIMEOUT ->
            erlang:error(invalid_wait)
    end,
    {reply, ok, State}.

-spec handle_cast(Msg::term(), state()) ->
                         {noreply, state()}.
handle_cast(really_should_die_horribly, State) ->
    {noreply, State}.

-spec handle_info(Info::term(), state()) -> {noreply, state()}.
handle_info(timeout, {die_nicely, State}) ->
    %% This is just so we can die nicely in the call
    {stop, normal, State};
handle_info({_, ?BOOKSHELF_CONFIG, Values}, State0={BucketName, _Store}) ->
    %% This is where we get notifications about config changes. The
    %% one we are interested in is "{set, [{disk_store, NewValue}]}"
    %% When we get that we create a new bkss_store and go back to
    %% handling things. It does *not* copy the bucket contents from
    %% the old location to the new or *anything* like that. So there
    %% may be unintended consequences.
    State1 =
        case proplists:get_value(disk_store, proplists:get_value(set, Values, []),
                                 undefined) of
            undefined ->
                State0;
            DiskStore ->
                {BucketName, bkss_store:new(bkss_fs, DiskStore)}
        end,
    {noreply, State1}.

-spec terminate(Reason::term(), state()) -> ok.
terminate(_Reason, _State) ->
    ok.

-spec code_change(OldVsn::term(), state(), Extra::term()) -> {ok, state()}.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
%%====================================================================
%% Internal Functions
%%====================================================================
do_copy(Store0, BucketName, FromPath, BucketName, ToPath) ->
    %% No need to lock here because FromBucket and ToBucket are the
    %% same name, so we can just do the copy
    bkss_store:obj_copy(Store0, BucketName, FromPath, BucketName, ToPath);
do_copy(Store0, BucketName, FromPath, ToBucket, ToPath) ->
    WaitRef = erlang:make_ref(),
    ToPid = bkss_store_server:get_bucket_reference(ToBucket),
    %% If we die we want the other bucket process to die too
    erlang:link(ToPid),
    %% We want to wait until the other bucket is free before doing
    %% anything
    Self = self(),
    proc_lib:spawn_link(fun() ->
                                gen_server:call(ToPid, {wait, Self, WaitRef}, ?WAIT_TIMEOUT)
                        end),
    %% We dont want to do anything until we get confirmation that the
    %% other bucket is ready for us
    receive
        {?WAIT_MARKER, waiting, WaitRef} ->
            ok
    after ?WAIT_TIMEOUT ->
            erlang:error(invalid_copy)
    end,
    Ret = bkss_store:obj_copy(Store0, BucketName, FromPath, ToBucket, ToPath),
    ToPid ! {?WAIT_MARKER, done, WaitRef},
    %% No need to remain linked now,
    erlang:unlink(ToPid),
    Ret.
