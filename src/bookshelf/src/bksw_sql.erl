%% @copyright 2015 Chef, Inc. All Rights Reserved
%% @author Mark Anderson <mark@chef.io>
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
-module(bksw_sql).

-include("internal.hrl").

%-ifdef(namespaced_types). %% workaround for sqerl complaint
-type dict() :: dict:dict().
%-endif.

-export([ statements/1,
          ping/0,
          create_bucket/1,
          delete_bucket/1,
          find_bucket/1,
          list_buckets/0,
          list_bucket/1,
          find_file/1,
          find_file/2,
          create_file/2,
          create_file_with_data/3,
          update_file_with_data/2,
          delete_file/2, %% deprecate
          delete_file/1,
          rename_file_with_overwrite/3,
          insert_file_data/0,
          delete_file_data/1,
          add_file_chunk/3,
          mark_file_done/6,
          get_chunk_data/2,
          replace_chunk_data/1,
          init_transfer_state/0,
          update_transfer_state/3,
          finalize_transfer_state/2
        ]).

-include_lib("sqerl/include/sqerl.hrl").

ping() ->
    case sqerl:select(ping, [], first_as_scalar, [ping]) of
        {ok, <<"pong">>} -> ok;
        {error, Reason} -> {error, Reason}
    end.

%% @doc bookshelf queries.
%% Referenced by sys.config in {sqerl, [{ prepared_staements...
statements(_) ->
    Path = filename:join([code:priv_dir(bookshelf), "pgsql_statements.config"]),
    {ok, Statements} = file:consult(Path),
    Statements.

create_bucket(BucketName) ->
    case sqerl:statement(insert_bucket, [BucketName], count) of
        {ok, 1} ->
            ok;
        {conflict, _} ->
            error("Never should happen");
        Error ->
            Error
    end.

delete_bucket(BucketName) ->
    case sqerl:statement(delete_bucket, [BucketName], count) of
        {ok, 1} ->
            ok;
        Error ->
            Error
    end.

find_bucket(BucketName) ->
    case sqerl:select(find_bucket, [BucketName], first_as_scalar, [bucket_id]) of
        {ok, BucketId} ->
            {ok, BucketId};
        {error, Reason} ->
            {error, Reason}
    end.

list_buckets() ->
    case sqerl:select(list_buckets, [], rows_as_records, ?DB_BUCKET_TX_FM) of
        {ok, none} ->
            [];
        {ok, Buckets} ->
            Buckets;
        {error, Reason} ->
            {error, Reason}
    end.

list_bucket(BucketName) ->
    case sqerl:select(list_bucket_with_hash, [BucketName], rows_as_records, ?DB_FILE_TX_FM) of
        {ok, Buckets} ->
            Buckets;
        {error, Reason} ->
            {error, Reason}
    end.

-spec create_file(binary(), binary()) -> {ok, integer()} | {error, any()}.
create_file(Bucket, Name) ->
    case sqerl:select(insert_file, [Bucket, Name], first_as_scalar, [data_id]) of
        {ok, File} ->
            {ok, File};
        {error, Reason} ->
            {error, Reason}
    end.


create_file_with_data(BucketId, FileName, DataId) ->
    case sqerl:statement(insert_file_with_data_id, [BucketId, FileName, DataId], count) of
        {ok, 1} ->
            ok;
        Error ->
            Error
    end.

update_file_with_data(FileId, DataId) ->
    case sqerl:statement(update_file_with_data_id, [FileId, DataId], count) of
        {ok, 1} ->
            ok;
        Error ->
            Error
    end.

find_file(Bucket, Name) ->
    case sqerl:select(find_file, [Bucket, Name], {first_as_record, ?DB_FILE_TX_FM }) of
        %% Awkward sanity check that we got back the expected record type here.
        {ok, Object} when db_file =:= element(1, Object) ->
            {ok, Object};
        {ok, none} ->
            {ok, not_found};
        {error, Error} ->
            {error, Error}
    end.

find_file(FileId) ->
    case sqerl:select(find_file_by_id, [FileId], {first_as_record, ?DB_FILE_TX_FM }) of
        %% Awkward sanity check that we got back the expected record type here.
        {ok, Object} when db_file =:= element(1, Object) ->
            {ok, Object};
        {ok, none} ->
            {ok, not_found};
        {error, Error} ->
            {error, Error}
    end.

%% TODO IMPROVE
delete_file(Bucket, Name) ->
    case find_file(Bucket, Name) of
        {ok, not_found} ->
            {error, file_not_found};
        {ok, #db_file{file_id = FileId}} ->
            delete_file(FileId);
        Error ->
            Error
    end.


delete_file(FileId) ->
    case sqerl:statement(delete_file, [FileId], count) of
        {ok, 1} ->
            ok;
        Error ->
            Error
    end.

rename_file_with_overwrite(BucketId, FileId, NewName) ->
    case sqerl:select(rename_file_with_overwrite, [BucketId, FileId, NewName], first_as_scalar, [success]) of
        {ok, true} ->
            ok;
        Error ->
            Error
    end.

mark_file_done(DataId, Size, Chunks, SumMD5, SumSha256, SumSha512) ->
    case sqerl:statement(update_file_data_done, [DataId, Size, Chunks, SumMD5, SumSha256, SumSha512], count) of
        {ok, 1} ->
            ok;
        Error ->
            Error
    end.

insert_file_data() ->
    case sqerl:statement(insert_file_data, [], first_as_scalar, [data_id]) of
        {ok, 1, DataId} ->
            {ok, DataId};
        {error, _} = Error ->
            Error
    end.

delete_file_data(DataId) ->
    case sqerl:statement(delete_file_data, [DataId], count) of
        {ok, 1} ->
            ok;
        Error ->
            Error
    end.

-spec add_file_chunk(integer(), integer(), binary()) -> ok.
add_file_chunk(DataId, Sequence, Chunk) ->
    case sqerl:statement(add_file_chunk, [DataId, Sequence, Chunk], count) of
        {ok, 1} ->
            ok;
        {conflict, _} ->
            error("Never should happen");
        Error ->
            Error
    end.

get_chunk_data(Id, ChunkSequence) ->
    case sqerl:select(fetch_file_chunk_data, [Id, ChunkSequence], first_as_scalar, [data]) of
        {ok, none} ->
            {ok, not_found};
        {ok, Data} ->
            {ok, Data};
        {error, Error} ->
            {error, Error}
    end.

replace_chunk_data(FileId) ->
    case sqerl:statement(replace_chunk_data, [FileId], first_as_scalar, [replace_chunk_data]) of
        {ok, DataId} ->
            {ok, DataId};
        Error ->
            Error
    end.

init_transfer_state() ->
    #file_transfer_state{
       size = 0,
       next_chunk = 0,
       hash_context_md5 = crypto:hash_init(md5),
       hash_context_sha256 = crypto:hash_init(sha256),
       hash_context_sha512 = crypto:hash_init(sha512)
      }.

update_transfer_state(#file_transfer_state{next_chunk = NextChunk} = UploadState, Data, ChunkIncr) when size(Data) == 0 ->
    UploadState#file_transfer_state{next_chunk = NextChunk + ChunkIncr};
update_transfer_state(#file_transfer_state{
                       size = Size,
                       next_chunk = NextChunk,
                       hash_context_md5 = ContextMd5,
                       hash_context_sha256 = ContextSha256,
                       hash_context_sha512 = ContextSha512
                      } = UploadState, Data, ChunkIncr) ->
    Size1 = Size + size(Data),

    UploadState#file_transfer_state{size = Size1,
                                    next_chunk = NextChunk + ChunkIncr,
                                    hash_context_md5 = crypto:hash_update(ContextMd5, Data),
                                    hash_context_sha256 = crypto:hash_update(ContextSha256, Data),
                                    hash_context_sha512 = crypto:hash_update(ContextSha512, Data)
                                 }.

finalize_transfer_state(#file_transfer_state{
                         size = Size,
                         hash_context_md5    = ContextMd5,
                         hash_context_sha256 = ContextSha256,
                         hash_context_sha512 = ContextSha512
                        },
                      #db_file{} = File) ->

    HashMd5    = crypto:hash_final(ContextMd5),
    HashSha256 = crypto:hash_final(ContextSha256),
    HashSha512 = crypto:hash_final(ContextSha512),

    File#db_file{data_size = Size,
                 hash_md5 = HashMd5, hash_sha256 = HashSha256, hash_sha512 = HashSha512}.
