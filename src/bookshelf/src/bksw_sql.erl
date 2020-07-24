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
          create_bucket_b/1,
          delete_bucket/1,
          delete_bucket_b/1,
          find_bucket/1,
          list_buckets/0,
          list_bucket/1,
          bucket_exists/1,
          find_file/2,
          create_file_link_data/3,
          link_file_data/2,
          delete_file/1,
          insert_file_data/0,
          add_file_chunk/3,
          update_metadata/6,
          get_chunk_data/2,
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
            {error, conflict};
        Error ->
            Error
    end.

-spec create_bucket_b(binary()) -> boolean().
create_bucket_b(Bucket) ->
    case create_bucket(Bucket) of
        ok ->
            true;
        {error, conflict} ->
            true;
        _Error ->
            false
    end.

delete_bucket(BucketName) ->
    case sqerl:statement(delete_bucket, [BucketName], count) of
        {ok, 1} ->
            ok;
        Error ->
            Error
    end.

-spec delete_bucket_b(binary()) -> boolean().
delete_bucket_b(Bucket) ->
    case delete_bucket(Bucket) of
        ok ->
            true;
        {ok, none} ->
            true;
        _ ->
            false
    end.

find_bucket(BucketName) ->
    case sqerl:select(find_bucket, [BucketName], first_as_scalar, [bucket_id]) of
        {ok, BucketId} ->
            {ok, BucketId};
        {error, Reason} ->
            {error, Reason}
    end.

-spec bucket_exists(binary()) -> boolean().
bucket_exists(Bucket) ->
    case find_bucket(Bucket) of
        {ok, none} ->
            false;
        {ok, _} ->
            true
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

create_file_link_data(BucketName, Name, DataId) ->
    case sqerl:select(create_file_link_data, [BucketName, Name, DataId], first_as_scalar, [success]) of
        {ok, FileId} ->
            {ok, FileId};
        {conflict, Reason} ->
            {error, Reason};
        {error, Reason} ->
            {error, Reason}
    end.

link_file_data(FileId, DataId) ->
    case sqerl:select(link_file_data, [FileId, DataId], first_as_scalar, [success]) of
        {ok, _} ->
            ok;
        {error, Reason} ->
            {error, Reason}
    end.

find_file(Bucket, Name) ->
    case sqerl:select(find_file, [Bucket, Name], {first_as_record, ?DB_FILE_TX_FM }) of
        {ok, none} ->
            {ok, not_found};
        {ok, Object} ->
            {ok, Object};
        {error, Error} ->
            {error, Error}
    end.

delete_file(FileId) ->
    case sqerl:statement(delete_file, [FileId], count) of
        {ok, 1} ->
            ok;
        Error ->
            Error
    end.

update_metadata(DataId, Size, Chunks, SumMD5, SumSha256, SumSha512) ->
    case sqerl:statement(update_metadata, [DataId, Size, Chunks, SumMD5, SumSha256, SumSha512], count) of
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

-spec add_file_chunk(integer(), integer(), binary()) -> ok.
add_file_chunk(DataId, Sequence, Chunk) ->
    case sqerl:statement(add_file_chunk, [DataId, Sequence, Chunk], count) of
        {ok, 1} ->
            ok;
        {conflict, _} ->
            {error, conflict};
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


%%%
%%% Transfer state functions
%%%
%%% A #file_transfer_state{} record tracks a transfer either to or from
%%% a client.  The intended flow is:
%%%
%%%   - initialize the record with init_transfer_state/0.
%%%   - call update_transfer_state/3 every time data is written or
%%%   read (depending on the transfer direction).
%%%   - call finalizes_transfer_state/2. finalize_transfer_state will
%%%   update a caller-supplied #db_file{} record with the appropriate
%%%   checksums.
%%%
init_transfer_state() ->
    #file_transfer_state{
       size = 0,
       next_chunk = 0,
       hash_context_md5 = erlang:md5_init(),
       hash_context_sha256 = crypto:hash_init(sha256),
       hash_context_sha512 = crypto:hash_init(sha512)
      }.

update_transfer_state(#file_transfer_state{next_chunk = NextChunk} = TransferState, Data, ChunkIncr) when size(Data) == 0 ->
    TransferState#file_transfer_state{next_chunk = NextChunk + ChunkIncr};
update_transfer_state(#file_transfer_state{
                         size = Size,
                         next_chunk = NextChunk,
                         hash_context_md5 = ContextMd5,
                         hash_context_sha256 = ContextSha256,
                         hash_context_sha512 = ContextSha512
                        } = TransferState, Data, ChunkIncr) ->
    Size1 = Size + size(Data),

    TransferState#file_transfer_state{size = Size1,
                                      next_chunk = NextChunk + ChunkIncr,
                                      hash_context_md5 = erlang:md5_update(ContextMd5, Data),
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

    HashMd5    = erlang:md5_final(ContextMd5),
    HashSha256 = crypto:hash_final(ContextSha256),
    HashSha512 = crypto:hash_final(ContextSha512),

    File#db_file{data_size = Size,
                 hash_md5 = HashMd5, hash_sha256 = HashSha256, hash_sha512 = HashSha512}.
