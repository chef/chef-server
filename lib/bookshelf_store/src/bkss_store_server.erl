%%%-------------------------------------------------------------------
%%% @author Eric B Merritt <ericbmerritt@gmail.com>
%%% @copyright (C) 2012, Opscode, Inc.
%%% @doc
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(bkss_store_server).

-behaviour(gen_server).

%% API
-export([start_link/0,
         get_bucket_reference/1,
         create_bucket/1,
         list_buckets/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-include("internal.hrl").

-define(SERVER, ?MODULE).

-record(state, {}).

%%%===================================================================
%%% Types
%%%===================================================================
-type state() :: record(state).

%%%===================================================================
%%% API
%%%===================================================================

-spec start_link() -> {ok, pid()} | ignore | {error, Error::term()}.
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

-spec get_bucket_reference(bookshelf_store:bucket_name()) -> pid().
get_bucket_reference(BucketName) ->
    gen_server:call(?SERVER, {get_bucket_reference, BucketName}).

-spec create_bucket(bookshelf_store:bucket_name()) -> pid().
create_bucket(BucketName) ->
    gen_server:call(?SERVER, {create_bucket, BucketName}).

-spec list_buckets() -> [bookshelf_store:bucket()].
list_buckets() ->
    gen_server:call(?SERVER, list_buckets).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

-spec init([]) -> {ok, state()} |
                    {ok, state(), Timeout::non_neg_integer()} |
                    ignore |
                    {stop, Reason::term()}.
init([]) ->
    {ok, #state{}}.

-spec handle_call(Request::term(), From::pid(), state()) ->
                         {reply, Reply::term(), state}.
handle_call({get_bucket_reference, BucketName}, _From, State) ->
    {Pid, _} = gproc:await({n,l,BucketName}),
    {reply, Pid, State};
handle_call({create_bucket, BucketName}, _From, State) ->
    bkss_bucket_sup:get_bucket(BucketName),
    {Pid, _} = gproc:await({n,l,BucketName}),
    {reply, Pid, State};
handle_call(list_buckets, _From, State) ->
    {ok,DiskStore} = opset:get_value(?BOOKSHELF_CONFIG, disk_store),
    Store = bkss_store:new(bkss_fs, DiskStore),
    {reply, bkss_store:list_buckets(Store), State}.


-spec handle_cast(Msg::term(), state()) ->
                         {noreply, state()}.

handle_cast(make_sure_we_crash, State) ->
    {noreply, State}.

-spec handle_info(Info::term(), state()) ->
                         {noreply, state()} |
                         {noreply, state(), Timeout::term()} |
                         {stop, Reason::term(), state()}.
handle_info(make_sure_we_crash, State) ->
    {noreply, State}.


-spec terminate(Reason::term(), state()) -> ok.
terminate(_Reason, _State) ->
    ok.

-spec code_change(OldVsn::term(), state(), Extra::term) -> {ok, state()}.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
