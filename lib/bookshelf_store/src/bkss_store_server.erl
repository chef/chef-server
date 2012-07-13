%% -*- erlang-indent-level: 4;indent-tabs-mode: nil; fill-column: 92 -*-
%% ex: ts=4 sw=4 et
%% @author Eric B Merritt <ericbmerritt@gmail.com>
%% @copyright Copyright 2012 Opscode, Inc.
-module(bkss_store_server).

-behaviour(gen_server).

%% API
-export([start_link/0,
         get_bucket_reference/2,
         create_bucket/1,
         bucket_list/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-include_lib("bookshelf_store/include/bookshelf_store.hrl").

-define(SERVER, ?MODULE).
-define(AWAIT_TIMEOUT, 10000).
-define(GEN_SERVER_TIMEOUT, 10000).
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

-spec get_bucket_reference(bookshelf_store:bucket_name(), boolean()) -> term().
 %% get_bucket_reference(BucketName) ->
%%     case catch gproc:await(make_key(BucketName), ?AWAIT_TIMEOUT) of
%%         {'EXIT', {timeout, _}} ->
%%             {error, timeout};
%%         {Pid, _} ->
%%             Pid
%%     end.

get_bucket_reference(BucketName, true) ->
    create_or_get_bucket_ref(make_key(BucketName), 3);
get_bucket_reference(BucketName, false) ->
    BucketKey = make_key(BucketName),
    case gproc:where(BucketKey) of
        undefined ->
            {error, not_found};
        Pid ->
            Pid
    end.

create_or_get_bucket_ref(_BucketKey, 0) ->
    {error, failed_lookup};
create_or_get_bucket_ref({_, _, BucketName}=BucketKey, Count) ->
    case gproc:where(BucketKey) of
        undefined ->
            case bkss_bucket_sup:start_child(BucketName) of
                {ok, Pid} ->
                    Pid;
                _Error ->
                    create_or_get_bucket_ref(BucketKey, Count - 1)
            end;
        Pid when is_pid(Pid)  ->
            Pid;
        _Error ->
            create_or_get_bucket_ref(BucketKey, Count - 1)
    end.


-spec create_bucket(bookshelf_store:bucket_name()) -> pid().
create_bucket(BucketName) ->
    gen_server:call(?SERVER, {create_bucket, BucketName}, ?GEN_SERVER_TIMEOUT).

-spec bucket_list() -> [bookshelf_store:bucket()].
bucket_list() ->
    gen_server:call(?SERVER, bucket_list).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

-spec init([]) -> {ok, state()}.
init([]) ->
    {ok,DiskStore} = application:get_env(bookshelf_store, disk_store),
    Store = bkss_store:new(bkss_fs, DiskStore),
    lists:foreach(fun(#bucket{name=BucketName}) ->
                          bkss_bucket_sup:start_child(BucketName)
                  end, bkss_store:bucket_list(Store)),
    {ok, #state{}}.

-spec handle_call(Request::term(), From::term(), state()) ->
                         {reply, Reply::term(), state}.
handle_call({create_bucket, BucketName}, _From, State) ->
    case bkss_bucket_server:bucket_server_exists(BucketName) of
        true ->
            ok;
        false ->
            bkss_bucket_sup:start_child(BucketName)
    end,
    {Pid, _} = gproc:await(make_key(BucketName), ?AWAIT_TIMEOUT),
    {reply, Pid, State};
handle_call(bucket_list, _From, State) ->
    {ok,DiskStore} = application:get_env(disk_store),
    Store = bkss_store:new(bkss_fs, DiskStore),
    {reply, bkss_store:bucket_list(Store), State}.


-spec handle_cast(handle_cast_not_implemented, state()) ->
                         no_return().
handle_cast(handle_cast_not_implemented, _State) ->
    erlang:error(handle_cast_not_implemented).

-spec handle_info(handle_info_not_implemented, state()) ->
                         no_return().
handle_info(handle_info_not_implemented, _State) ->
    erlang:error(handle_info_not_implemented).

-spec terminate(Reason::term(), state()) -> ok.
terminate(_Reason, _State) ->
    ok.

-spec code_change(OldVsn::term(), state(), Extra::term()) -> {ok, state()}.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
-spec make_key(bookshelf_store:bucket_name()) -> term().
make_key(BucketName) ->
    {n, l, BucketName}.
