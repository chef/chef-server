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
         obj_send/3,
         obj_recv/5]).

-export_type([bucket_name/0, bucket/0, object/0, path/0]).

%%%===================================================================
%%% Types
%%%===================================================================

-type bucket_name() :: bkss_store:bucket_name().
-type bucket() :: bkss_store:bucket().
-type object() :: bkss_store:object().
-type path() :: bkss_store:path().

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
    %% exists. If it does not exist then we simply return false (rather then blow up). If it
    %% does exist then we forward the call to the bucket server to check to see if the
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
    call(BucketName, {obj_exists, Path}).

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

-spec obj_send(bucket_name(), path(), Trans::bkss_transport:trans()) ->
                      {ok, MD5::term()} | {error, Reason::term()}.
obj_send(BucketName, Path, Trans) ->
    call(BucketName, {obj_send, Path, Trans}).

-spec obj_recv(bucket_name(), path(), Trans::bkss_transport:trans(),
               Buffer::binary(), Length::non_neg_integer()) ->
                      {ok, MD5::term()} | {error, Reason::term()}.
obj_recv(BucketName, Path, Trans, Buffer, Length) ->
    call(BucketName, {obj_recv, Path, Trans, Buffer, Length}).

%%===================================================================
%% Internal Functions
%%===================================================================
-spec call(bucket_name(), Msg::term()) -> term().
call(BucketName, Msg) ->
    Pid = bkss_store_server:get_bucket_reference(BucketName),
    gen_server:call(Pid, Msg).
