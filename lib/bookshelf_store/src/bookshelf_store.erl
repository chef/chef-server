%%%-------------------------------------------------------------------
%%% @author Eric B Merritt <ericbmerritt@gmail.com>
%%% @copyright (C) 2012, Opscode, Inc.
%%% @doc
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(bookshelf_store).

-export([bucket_create/1,
         bucket_list/0,
         bucket_exists/1,
         bucket_delete/1,
         obj_list/1,
         obj_exists/2,
         obj_delete/2,
         obj_meta/2,
         obj_copy/4,
         obj_send/3,
         obj_recv/5]).

-export_type([bucket_name/0]).

%%%===================================================================
%%% Types
%%%===================================================================

-type bucket_name() :: bkss_store:bucket_name().
-type bucket() :: bkss_store:bucket().
%-type object() :: bkss_store:object().

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
    Pid = bkss_registery:get_bucket_reference(BucketName),
    gen_server:call(Pid, Msg).
