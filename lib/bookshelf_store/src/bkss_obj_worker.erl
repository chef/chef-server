 %% -*- erlang-indent-level: 4;indent-tabs-mode: nil; fill-column: 92 -*-
%% ex: ts=4 sw=4 et
%% @author Eric B Merritt <ericbmerritt@gmail.com>
%% @copyright Copyright 2012 Opscode, Inc.
-module(bkss_obj_worker).

%% API
-export([start_link/1]).

-include("internal.hrl").

-define(WAIT_MARKER, '$$__wait_for_me__$$').
-define(WAIT_TIMEOUT, 100000).

%%%===================================================================
%%% Types
%%%===================================================================
-type work() :: {WorkType::atom(), From::pid(), [WorkDetails::term()]}.

%%%===================================================================
%%% API
%%%===================================================================

-spec start_link(work()) ->
                        {ok, pid()} | ignore | {error, Error::term()}.
start_link(Work) ->
    proc_lib:spawn_link(fun() -> do_work(Work) end).

do_work({obj_list, From, [BucketName, Store]}) ->
    gen_server:reply(From, bkss_store:obj_list(Store, BucketName));
do_work({obj_exists, From, [BucketName, Store, Path]}) ->
    gen_server:reply(From, bkss_store:obj_exists(Store, BucketName, Path));
do_work({obj_delete, From, [BucketName, Store, Path]}) ->
    {_, Reply} = bkss_store:obj_delete(Store, BucketName, Path),
    gen_server:reply(From,  Reply),
    bkss_bucket_server:unlock_path(BucketName, Path);
do_work({obj_meta, From, [BucketName, Store, Path]}) ->
    gen_server:reply(From, bkss_store:obj_meta(Store, BucketName, Path)),
    bkss_bucket_server:unlock_path(BucketName, Path);
do_work({obj_create, From, [BucketName, Store, Path, Data]}) ->
    {_, Reply} = bkss_store:obj_create(Store, BucketName, Path, Data),
    gen_server:reply(From, Reply),
    bkss_bucket_server:unlock_path(BucketName, Path);
do_work({obj_get, From, [BucketName, Store, Path]}) ->
    gen_server:reply(From,  bkss_store:obj_get(Store, BucketName, Path)),
    bkss_bucket_server:unlock_path(BucketName, Path);
do_work({obj_copy, From, [BucketName, Store, FromPath, ToBucket, ToPath]}) ->
    bkss_bucket_server:lock_path(ToBucket, ToPath),
    {_, Result} = bkss_store:obj_copy(Store, BucketName, FromPath, ToBucket, ToPath),
    gen_server:reply(From, Result),
    bkss_bucket_server:unlock_path(BucketName, FromPath),
    bkss_bucket_server:unlock_path(ToBucket, ToPath);
do_work({obj_send, From, [BucketName, Store, Path, Bridge]}) ->
    {_, Reply} = bkss_store:obj_send(Store, BucketName, Path, Bridge),
    gen_server:reply(From, Reply),
    bkss_bucket_server:unlock_path(BucketName, Path);
do_work({obj_recv, From, [BucketName, Store, Path, Bridge, Buffer, Length]}) ->
    {_, Reply} = bkss_store:obj_recv(Store, BucketName, Path,
                                     Bridge, Buffer, Length),
    gen_server:reply(From, Reply),
    bkss_bucket_server:unlock_path(BucketName, Path).
