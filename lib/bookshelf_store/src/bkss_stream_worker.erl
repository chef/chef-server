%% -*- erlang-indent-level: 4;indent-tabs-mode: nil; fill-column: 92 -*-
%% ex: ts=4 sw=4 et
%% @author Eric B Merritt <ericbmerritt@gmail.com>
%% @copyright Copyright 2012 Opscode, Inc.
-module(bkss_stream_worker).

-behaviour(gen_server).

%% API
-export([start_link/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-export_type([work/0, state/0]).

-define(WAIT_TIMEOUT, 100000).
-record(state, {type, bucket, path, ref, store, size}).

%%%===================================================================
%%% Types
%%%===================================================================
-type state() :: record(state).
-type work() :: {in | out,
                 From::term(),
                 bookshelf_store:bucket_name(),
                 bookshelf_store:path(),
                 Store::bkss_store:store(),
                 Size::non_neg_integer()}.

%%%===================================================================
%%% API
%%%===================================================================

-spec start_link(work()) ->
                        {ok, pid()} | ignore | {error, Error::term()}.
start_link(Work) ->
    gen_server:start_link(?MODULE, [Work], []).
%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

-spec init([work()]) -> {ok, state(), non_neg_integer()}.
init([{Type, From, BucketName, Path, Store0, Size}]) ->
    ServerRef = {node(), erlang:make_ref()},
    gproc:reg({n,l,ServerRef}),
    {Store2, Ref1} =
        case Type of
            in ->
                {Store1, {ok, Ref0}} =
                    bkss_store:obj_in_start(Store0, BucketName, Path),
                {Store1, Ref0};
            out ->
                {Store1, {ok, Ref0}} =
                    bkss_store:obj_out_start(Store0, BucketName, Path, Size),
                {Store1, Ref0}
        end,
    gen_server:reply(From, {ok, ServerRef}),
    {ok,#state{type = Type, bucket = BucketName, path = Path,
               ref = Ref1, store = Store2, size = Size}, ?WAIT_TIMEOUT}.

-spec handle_call(Request::term(), From::term(), state()) ->
                         {reply, Reply::term(), state(), non_neg_integer()} |
                         {stop, Reason::term, state()}.
handle_call({obj_in, Data}, _From, State = #state{type=in, store=Store0, ref=Ref0}) ->
    {Store1, Rep} = bkss_store:obj_in(Store0, Ref0, Data),
    case Rep of
        {ok, Ref1} ->
            {reply, ok, State#state{store=Store1, ref=Ref1}, ?WAIT_TIMEOUT};
        _ ->
            {stop, read_error, Rep, State#state{store=Store1}}
    end;
handle_call(obj_in_end, _From, State = #state{type=in, store=Store0, ref=Ref}) ->
    {Store1, Rep} = bkss_store:obj_in_end(Store0, Ref),
    {stop, normal, Rep, State#state{store=Store1}};
handle_call(obj_out, _From, State = #state{type=out, store=Store0, ref=Ref}) ->
    {Store1, Rep} = bkss_store:obj_out(Store0, Ref),
    case Rep of
        done ->
            {stop, normal, Rep, State#state{store=Store1}};
        _ ->
            {reply, Rep, State#state{store=Store1}, ?WAIT_TIMEOUT}
    end.

-spec handle_cast(Msg::term(), state()) ->
                         {noreply, state()}.
handle_cast(Msg, _State) ->
    erlang:error({handle_cast_not_implemented, Msg}).

-spec handle_info(Info::term(), state()) -> {stop, timeout, state()}.
handle_info(timeout, State) ->
    %% This is just so we can die nicely in the call
    {stop, timeout, State}.

-spec terminate(Reason::term(), state()) -> ok.
terminate(normal, #state{bucket=BucketName, path=Path}) ->
    bkss_bucket_server:unlock_path(BucketName, Path),
    ok;
terminate(_Error, #state{type=in, bucket=BucketName, path=Path, store=Store}) ->
    bkss_store:obj_delete(Store, BucketName, Path),
    bkss_bucket_server:unlock_path(BucketName, Path),
    ok;
terminate(_Error, #state{bucket=BucketName, path=Path}) ->
    bkss_bucket_server:unlock_path(BucketName, Path),
    ok.

-spec code_change(OldVsn::term(), state(), Extra::term()) -> {ok, state()}.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
