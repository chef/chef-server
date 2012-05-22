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
-export([start_link/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).
-include("internal.hrl").

-define(WAIT_MARKER, '$$__wait_for_me__$$').

%%%===================================================================
%%% Types
%%%===================================================================
-type state() ::{bucket_store:bucket_name(), bkss_store:store()}.

%%%===================================================================
%%% API
%%%===================================================================

-spec start_link(bucket_store:bucket_name()) ->
                        {ok, pid()} | ignore | {error, Error::term()}.
start_link(BucketName) ->
    gen_server:start_link(?MODULE, [BucketName], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

-spec init([]) -> {ok, state()} |
                  {ok, state(), Timeout::non_neg_integer()} |
                  ignore |
                  {stop, Reason::term()}.
init([BucketName]) ->
    {ok,DiskStore} = opset:get_value(?BOOKSHELF_CONFIG, disk_store),
    State = bkss_store:new(bkss_fs, DiskStore),
    {ok,_} = bkss_store:bucket_create(State, BucketName),
    gproc:reg({n,l,BucketName}),
    opset:link_config(?BOOKSHELF_CONFIG),
    {ok,{BucketName, State}}.

-spec handle_call(Request::term(), From::pid(), state()) ->
                         {reply, Reply::term(), state()}.
handle_call(bucket_list, _From, State={_, Store}) ->
    {reply,bkss_store:bucket_list(Store), State};
handle_call(bucket_exists, _From, State={BucketName, Store}) ->
    {reply,bkss_store:bucket_exists(Store, BucketName), State};
handle_call(bucket_delete, _From, State={BucketName, Store}) ->
    bkss_store:bucket_delete(Store, BucketName),
    {stop, normal, State};
handle_call(obj_list, _From, State={BucketName, Store}) ->
    {reply, bkss_store:obj_list(Store, BucketName), State};
handle_call({obj_exists, Path}, _From, State={BucketName, Store}) ->
    {reply, bkss_store:obj_exists(Store, BucketName, Path), State};
handle_call({obj_delete, Path}, _From, State={BucketName, Store}) ->
    {reply, bkss_store:obj_delete(Store, BucketName, Path), State};
handle_call({obj_meta, Path}, _From, State={BucketName, Store}) ->
    {reply, bkss_store:obj_meta(Store, BucketName, Path), State};
handle_call({obj_copy, FromPath, ToBucket, ToPath}, _From, State={BucketName, Store}) ->
    WaitRef = erlang:make_ref(),
    ToPid = bkss_registery:get_bucket_reference(ToBucket),
    %% If we die we want the other bucket process to die too
    erlang:link(ToPid),
    gen_server:cast(ToPid, {wait, WaitRef}),
    Result = bkss_store:obj_copy(Store, BucketName, FromPath, ToBucket, ToPath),
    ToPid ! {?WAIT_MARKER, WaitRef},
    %% No need to remain linked now,
    erlang:unlink(ToPid),
    {reply, Result, State};
handle_call({obj_send, Path, Bridge}, _From, State={BucketName, Store}) ->
    {reply, bkss_store:obj_send(Store, BucketName, Path, Bridge), State};
handle_call({obj_recv, Path, Bridge, Buffer, Length}, _From, State={BucketName, Store}) ->
    {reply, bkss_store:obj_recv(Store, BucketName, Path,
                                Bridge, Buffer, Length), State}.

-spec handle_cast(Msg::term(), state()) ->
                         {noreply, state()}.
handle_cast({wait, Ref}, State) ->
    %% We got a wait call. This is mostly designed to guard this
    %% bucket from updates while another bucket is doing work
    %% inside. Here we just wait on the message
    receive
        {?WAIT_MARKER, Ref} -> ok
    end,
    {noreply, State}.

-spec handle_info(Info::term(), state()) -> {noreply, state()}.
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
