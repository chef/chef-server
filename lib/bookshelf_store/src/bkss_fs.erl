%% @copyright 2012 Opscode, Inc. All Rights Reserved
%% @author Tim Dysinger <dysinger@opscode.com>
%%
%% Licensed to the Apache Software Foundation (ASF) under one or more
%% contributor license agreements.  See the NOTICE file distributed
%% with this work for additional information regarding copyright
%% ownership.  The ASF licenses this file to you under the Apache
%% License, Version 2.0 (the "License"); you may not use this file
%% except in compliance with the License.  You may obtain a copy of
%% the License at http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or
%% implied.  See the License for the specific language governing
%% permissions and limitations under the License.

-module(bkss_fs).

-behaviour(bkss_store).

-export([new/1,
         bucket_create/2,
         bucket_delete/2,
         bucket_exists/2,
         bucket_list/1,
         obj_copy/5,
         obj_delete/3,
         obj_exists/3,
         obj_list/2,
         obj_meta/3,
         obj_recv/6,
         obj_send/4
        ]).

-export_type([fs_data/0, dir/0]).

-include_lib("bookshelf_store/include/bookshelf_store.hrl").
-include_lib("kernel/include/file.hrl").

-define(BLOCK_SIZE, 16384).
-define(TIMEOUT_MS, 4096).

%%===================================================================
%% Types
%%===================================================================

-opaque fs_data() :: {atom(), dir()}.
-type dir() :: string().

%%===================================================================
%% Signature API
%%===================================================================
-spec new(dir()) -> fs_data().
new(Dir) when is_binary(Dir) ->
    {?MODULE, Dir}.

-spec bucket_list(fs_data()) -> [bkss_store:bucket()].
bucket_list({?MODULE, Dir}) ->
    %% crash if no access to base dir
    {ok, Files} = file:list_dir(Dir),
    [dir_2_bucket(filename:join(Dir, P))
     || P <- Files, filelib:is_dir(filename:join(Dir, P))].

-spec bucket_exists(fs_data(), bkss_store:bucket_name()) -> boolean().
bucket_exists({?MODULE, Dir}, Bucket) ->
    filelib:is_dir(filename:join(Dir, Bucket)).

-spec bucket_create(fs_data(), bkss_store:bucket_name()) ->
                           {fs_data(), ok | {error, Reason::term()}}.
bucket_create(State={?MODULE, Dir}, Bucket) ->
    {State, file:make_dir(filename:join(Dir, Bucket))}.

-spec bucket_delete(fs_data(), bkss_store:bucket_name()) ->
                           {fs_data(), ok | {error, Reason::term()}}.
bucket_delete(State={?MODULE, Dir}, Bucket) ->
    {State, file:del_dir(filename:join(Dir, Bucket))}.

-spec obj_list(fs_data(), bkss_store:bucket_name()) -> [bkss_store:object()].
obj_list({?MODULE, Dir}, BucketName) when is_binary(Dir), is_binary(BucketName) ->
    BucketPath = filename:join(Dir, BucketName),
    lists:flatten(filelib:fold_files(
                    BucketPath,
                    ".*",
                    true,
                    fun(FilePath, Acc) ->
                            [file_2_object(Dir, BucketPath, BucketName, FilePath) | Acc]
                    end,
                    [])).

-spec obj_exists(fs_data(), bkss_store:bucket_name(), bkss_store:path()) ->
                        boolean().
obj_exists({?MODULE, Dir}, Bucket, Path)
  when is_binary(Dir), is_binary(Bucket), is_binary(Path) ->
    filelib:is_regular(filename:join([Dir, Bucket, Path])).

-spec obj_delete(fs_data(), bkss_store:bucket_name(), bkss_store:path()) ->
                        {fs_data(), ok | {error, Reason::term()}}.
obj_delete(State={?MODULE, Dir}, Bucket, Path)
  when is_binary(Dir), is_binary(Bucket), is_binary(Path) ->
    ObjectPath = filename:join([Dir, Bucket, Path]),
    {State, file:delete(ObjectPath)}.

-spec obj_meta(fs_data(), bkss_store:bucket_name(), bkss_store:path()) ->
                      {ok, bkss_store:object()} | {error, Reason::term()}.
obj_meta({?MODULE, Dir}, Bucket, Path) ->
    Filename = filename:join([Dir, Bucket, Path]),
    case file:open(Filename, [binary,raw,read_ahead]) of
        {ok, File} ->
            case file_md5(File, erlang:md5_init()) of
                {ok, Md5} ->
                    case file:read_file_info(Filename) of
                        {ok, #file_info{mtime=Date, size=Size}} ->
                            [UTC|_] = %% FIXME This is a hack until R15B
                                calendar:local_time_to_universal_time_dst(Date),
                            {ok, #object{name=Path,
                                         date=UTC,
                                         size=Size,
                                         digest=Md5}};
                        Any -> Any
                    end;
                Any -> Any
            end;
        Any -> Any
    end.

-spec obj_copy(fs_data(), bkss_store:bucket_name(), bkss_store:path(),
               bkss_store:bucket_name(), bkss_store:path()) ->
                      {fs_data(), ok | {error, Reason::term()}}.
obj_copy(State={?MODULE, Dir}, FromBucket, FromPath, ToBucket, ToPath) ->
    {State, file:copy(filename:join([Dir, FromBucket, FromPath]),
                      filename:join([Dir, ToBucket, ToPath]))}.

-spec obj_send(fs_data(), bkss_store:bucket_name(),
               bkss_store:path(), bkss_transport:trans()) ->
                      {fs_data(), ok | {error, Reason::term()}}.
obj_send(State={?MODULE, Dir}, Bucket, Path, Bridge) ->
    Resp =
        case obj_open_r(Dir, Bucket, Path) of
            {ok, FsSt} ->
                case read(FsSt, Bridge) of
                    {ok, FsSt2}      -> obj_close(FsSt2);
                    {error, timeout} -> obj_close(FsSt),
                                        {error, timeout};
                    Any              -> obj_close(FsSt),
                                        Any
                end;
            Any -> Any
        end,
    {State, Resp}.

-spec obj_recv(fs_data(), bkss_store:bucket_name(), bkss_store:path(),
               bkss_transport:trans(), binary(), non_neg_integer()) ->
                      {fs_data(), ok | {error, Reason::term()}}.
obj_recv(State={?MODULE, Dir}, Bucket, Path, Bridge, Buffer, Length) ->
    filelib:ensure_dir(filename:join([Dir, Bucket, Path])),
    Resp =
        case obj_open_w(Dir, Bucket, Path) of
            {ok, FsSt} ->
                case write(FsSt, Bridge, Length, Buffer) of
                    {ok, FsSt2} -> obj_close(FsSt2);
                    {error, timeout} ->
                        obj_close(FsSt),
                        obj_delete(Dir, Bucket, Path),
                        {error, timeout};
                    Any ->
                        obj_close(FsSt),
                        obj_delete(Dir, Bucket, Path),
                        Any
                end;
            Any -> Any
        end,
    {State, Resp}.

%%===================================================================
%% Internal Functions
%%===================================================================
-spec read({file:file(), term()}, bkss_transport:trans()) ->
                  ok | {error, Reason::term()}.
read({File, _}=FsSt, Bridge) ->
    case file:read(File, ?BLOCK_SIZE) of
        {ok, Chunk} -> bkss_transport:send(Bridge, Chunk),
                       read(FsSt, Bridge);
        eof         -> ok;
        Any         -> Any
    end.

write(FsSt, Bridge, Length, <<>>) ->
    write(FsSt, Bridge, Length);
write(FsSt, Bridge, Length, Buf) ->
    case obj_write(FsSt, Buf) of
        {ok, NewFsSt} ->
            write(NewFsSt, Bridge, Length-byte_size(Buf));
        Any -> Any
    end.

write(FsSt, _Bridge, 0) ->
    obj_write(FsSt, <<>>);
write(FsSt, Bridge, Length) when Length =< ?BLOCK_SIZE ->
    case bksw_socket_transport:recv(Bridge, Length) of
        {ok, Chunk} -> obj_write(FsSt, Chunk);
        Any         -> Any
    end;
write(FsSt, Bridge, Length) ->
    case bksw_socket_transport:recv(Bridge, ?BLOCK_SIZE) of
        {ok, Chunk} ->
            case obj_write(FsSt, Chunk) of
                {ok, NewFsSt} ->
                    write(NewFsSt, Bridge, Length-?BLOCK_SIZE);
                Any -> Any
            end;
        Any -> Any
    end.

-spec dir_2_bucket(dir()) -> bkss_store:bucket().
dir_2_bucket(Dir) ->
    %% crash if no access to any bucket dir
    {ok, #file_info{ctime=Date}} =
        file:read_file_info(Dir),
    [UTC|_] =
        calendar:local_time_to_universal_time_dst(Date),
    #bucket{name=filename:basename(Dir), date=UTC}.

-spec file_2_object(dir(), string(), bkss_store:bucket_name(), string()) ->
                           bkss_store:object() | [].
file_2_object(Dir, BucketPath, BucketName, FilePath) ->
    case filelib:is_regular(FilePath) of
        true ->
            Pos = byte_size(FilePath),
            Len = byte_size(BucketPath) + 1
                - byte_size(FilePath),
            Name = binary:part(FilePath, Pos, Len),
            case obj_meta({?MODULE, Dir}, BucketName, Name) of
                {ok, Object} ->
                    Object;
                _ ->
                    []
            end;
        _ ->
            []
    end.
file_md5(File, Ctx) ->
    case file:read(File, ?BLOCK_SIZE) of
        {ok, Bin} ->
            file_md5(File, erlang:md5_update(Ctx, Bin));
        eof ->
            file:close(File),
            {ok, erlang:md5_final(Ctx)}
    end.
obj_open(Dir, Bucket, Path, Opts) ->
    case file:open(filename:join([Dir, Bucket, Path]), Opts) of
        {ok, File} -> {ok, {File, erlang:md5_init()}};
        Any        -> Any
    end.

obj_open_w(Dir, Bucket, Path) ->
    obj_open(Dir, Bucket, Path, [raw, binary, write]).

obj_open_r(Dir, Bucket, Path) ->
    obj_open(Dir, Bucket, Path, [raw, binary, read_ahead]).

obj_write({File, Ctx}, Chunk) ->
    case file:write(File, Chunk) of
        ok  -> {ok, {File, erlang:md5_update(Ctx, Chunk)}};
        Any -> Any
    end.

obj_close({File, Ctx}) ->
    case file:close(File) of
        ok  -> {ok, erlang:md5_final(Ctx)};
        Any -> Any
    end.

%% ===================================================================
%%                          Eunit Tests
%% ===================================================================
