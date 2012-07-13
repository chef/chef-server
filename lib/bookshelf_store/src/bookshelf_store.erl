%% -*- erlang-indent-level: 4;indent-tabs-mode: nil; fill-column: 92 -*-
%% ex: ts=4 sw=4 et
%% @author Eric B Merritt <ericbmerritt@gmail.com>
%% @copyright Copyright 2012 Opscode, Inc.
%% @doc
%%  This provides the interface to the 'bookshelf_store' otp app and
%%  serves as the single point of interface for code outside of the
%%  bookshelf_store app.
%% @end
-module(bookshelf_store).

-export([bucket_create/1,
         bucket_list/0,
         bucket_exists/1,
         bucket_delete/1,
         obj_list/1,
         obj_exists/2,
         obj_delete/2,
         obj_meta/2,
         obj_create/3,
         obj_get/2,
         obj_copy/4,
         obj_in_start/2,
         obj_in/2,
         obj_in_end/1,
         obj_out_start/3,
         obj_out/1
        ]).

-export_type([bucket_name/0, bucket/0, object/0, path/0]).

-define(GEN_SERVER_TIMEOUT, 20000).

%%%===================================================================
%%% Types
%%%===================================================================

-type bucket_name() :: bkss_store:bucket_name().
-type bucket() :: bkss_store:bucket().
-type object() :: bkss_store:object().
-type path() :: bkss_store:path().
-opaque stream_token() :: term().

%%===================================================================
%% External API
%%===================================================================
-spec bucket_create(bucket_name()) -> ok | fail.
bucket_create(BucketName) ->
    Pid = bkss_store_server:create_bucket(BucketName),
    case Pid of
        Pid when erlang:is_pid(Pid) ->
            ok;
        _ ->
            fail
    end.

-spec bucket_list() -> [bucket()].
bucket_list() ->
    bkss_store_server:bucket_list().


-spec bucket_exists(bucket_name()) -> boolean().
bucket_exists(BucketName) ->
    %% This works differently then most of the other functions because in this case, it is
    %% expected that the bucket may or may not exist. So we check to see of the bucket
    %% exists. If it does not exist then we simply return false (rather then blow up). If it    %% does exist then we forward the call to the bucket server to check to see if the
    %% directory exists.
    bkss_bucket_server:bucket_server_exists(BucketName) andalso
        call(BucketName, bucket_exists).

-spec bucket_delete(bucket_name()) -> ok | {error, Reason::term()}.
bucket_delete(BucketName) ->
    call(BucketName, bucket_delete).

-spec obj_list(bucket_name()) -> [object()].
obj_list(BucketName) ->
    call(BucketName, obj_list).

-spec obj_exists(bucket_name(), path()) -> boolean().
obj_exists(BucketName, Path) ->
    case call(BucketName, {obj_exists, Path}) of
        {error, timeout} ->
            false;
        V ->
            V
    end.

-spec obj_delete(bucket_name(), path()) -> ok | {error, Reason::term()}.
obj_delete(BucketName, Path) ->
    call(BucketName, {obj_delete, Path}).

-spec obj_meta(bucket_name(), path())  ->
                      {ok, object()} | {error, Reason::term()}.
obj_meta(BucketName, Path) ->
    call(BucketName, {obj_meta, Path}).

-spec obj_create(bucket_name(), path(), Data::binary()) ->
                        ok | {error, Reason::term()}.
obj_create(BucketName, Path, Data) ->
    call(BucketName, {obj_create, Path, Data}).

-spec obj_get(bucket_name(), path()) -> {ok, binary()} | {error, Reason::term()}.
obj_get(BucketName, Path) ->
    call(BucketName, {obj_get, Path}).

-spec obj_copy(FromBucket::bucket_name(), FromPath::path(),
               ToBucket::bucket_name(), ToPath::path()) ->
                      {ok, BytesCopied::non_neg_integer()} | {error, Reason::term()}.
obj_copy(FromBucket, FromPath, ToBucket, ToPath) ->
    call(FromBucket, {obj_copy, FromPath, ToBucket, ToPath}).

-spec obj_in_start(bucket_name(), path()) ->
                          {ok, StreamToken::term()} | {error, Reason::term()}.
obj_in_start(BucketName, Path) ->
    call(BucketName, {obj_in_start, Path}).

-spec obj_in(stream_token(), Data::iolist()) ->
                           ok | {error, Reason::term()}.
obj_in(StreamToken, Data) ->
    {Pid, _} = gproc:await({n, l, StreamToken}, ?GEN_SERVER_TIMEOUT),
    gen_server:call(Pid, {obj_in, Data}, ?GEN_SERVER_TIMEOUT).

-spec obj_in_end(stream_token()) ->
                        {ok, MD5::term()} | {error, Reason::term()}.
obj_in_end(StreamToken) ->
    {Pid, _} = gproc:await({n, l, StreamToken}, ?GEN_SERVER_TIMEOUT),
    gen_server:call(Pid, obj_in_end, ?GEN_SERVER_TIMEOUT).

-spec obj_out_start(bucket_name(), path(), HunkSize::non_neg_integer()) ->
                           {ok, StreamToken::term()} | {error, Reason::term()}.
obj_out_start(BucketName, Path, HunkSize) ->
    call(BucketName, {obj_out_start, Path, HunkSize}).

-spec obj_out(stream_token()) ->
                     {ok, Data::binary()} | done | {error, Reason::term()}.
obj_out(StreamToken) ->
    {Pid, _} = gproc:await({n, l, StreamToken}, ?GEN_SERVER_TIMEOUT),
    gen_server:call(Pid, obj_out, ?GEN_SERVER_TIMEOUT).

%%===================================================================
%% Internal Functions
%%===================================================================
-spec call(bucket_name(), Msg::term()) -> term().
call(BucketName, Msg) ->
    case bkss_store_server:get_bucket_reference(BucketName, true) of
        Pid when is_pid(Pid) ->
            gen_server:call(Pid, Msg, ?GEN_SERVER_TIMEOUT);
        {error, _} = Error ->
            Error
    end.

