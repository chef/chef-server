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
    if
        erlang:is_pid(Pid) ->
            ok;
        true ->
            fail
    end.

-spec bucket_list() -> [bucket()].
bucket_list() ->
    bkss_store_server:bucket_list().

bucket_exists(BucketName) ->
    bkss_bucket_server:bucket_server_exists(BucketName) andalso
        call(BucketName, bucket_exists).

bucket_delete(BucketName) ->
    call(BucketName, bucket_delete).

obj_list(BucketName) ->
    call(BucketName, obj_list).

obj_exists(BucketName, Path) ->
    call(BucketName, {obj_exists, Path}).

obj_delete(BucketName, Path) ->
    call(BucketName, {obj_delete, Path}).

obj_meta(BucketName, Path) ->
    call(BucketName, {obj_meta, Path}).

obj_create(BucketName, Path, Data) ->
    call(BucketName, {obj_create, Path, Data}).

obj_get(BucketName, Path) ->
    call(BucketName, {obj_get, Path}).

obj_copy(FromBucket, FromPath, ToBucket, ToPath) ->
    call(FromBucket, {obj_copy, FromPath, ToBucket, ToPath}).

obj_send(BucketName, Path, Bridge) ->
    call(BucketName, {obj_send, Path, Bridge}).

obj_recv(BucketName, Path, Bridge, Buffer, Length) ->
    call(BucketName, {obj_recv, Path, Bridge, Buffer, Length}).

%%===================================================================
%% Internal Functions
%%===================================================================
call(BucketName, Msg) ->
    Pid = bkss_store_server:get_bucket_reference(BucketName),
    gen_server:call(Pid, Msg).
