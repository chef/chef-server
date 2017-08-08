%% -*- erlang-indent-level: 4;indent-tabs-mode: nil; fill-column: 92 -*-
%% ex: ts=4 sw=4 et
%% @author Mark Anderson <mark@chef.io>
%% Copyright 2017 Chef, Inc. All Rights Reserved.
%%
%% This file is provided to you under the Apache License,
%% Version 2.0 (the "License"); you may not use this file
%% except in compliance with the License.  You may obtain
%% a copy of the License at
%%
%%   http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing,
%% software distributed under the License is distributed on an
%% "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
%% KIND, either express or implied.  See the License for the
%% specific language governing permissions and limitations
%% under the License.
%%

-module(bksw_migrate_file).

-export([migrate_buckets/0,
         migrate_bucket/1,
         list_files/1,
         migrate_file/2]).

-include("internal.hrl").


%% Generally only have one bucket in practice, so this can be kind of dumb
migrate_buckets() ->
    BucketsFS = bksw_io:bucket_list(),
    BucketsSql = bksw_sql:list_buckets(),
    BucketsMissing = BucketsFS -- BucketsSql,
    Results = lists:foldl(fun(B, Errors) ->
                                  case bksw_sql:create_bucket_b(B) of
                                      true ->
                                          Errors;
                                      Error ->
                                          [{B, Error} | Errors]
                                  end
                          end,
                          [],
                          BucketsMissing),
    case Results of
        [] ->
            {ok, BucketsFS};
        Errors ->
            {error, Errors}
    end.


migrate_bucket(Bucket) ->
    case bksw_sql:create_bucket_b(Bucket) of
        %% existing bucket
        true ->
            Bucket;
        _ ->
            {error, failed_to_create_bucket, Bucket}
    end.

%% This is slow, and can consume a lot of memory in systems with lots of files
list_files(Buckets) ->
    [ { Bucket, bksw_io:entry_list(Bucket) } || Bucket <- Buckets ].

%% This uses an optimistic strategy; if a file with the same name
%% exists in SQL we skip migration. We might want a more pessimistic strategy
%% in the future, as it's possible that the contents are not the same
%% But since this is for the purposes of Chef cookbooks, which are
%% essentially immutable, I think we are safe.
migrate_file(Bucket, Entry) ->
    case bksw_sql:find_file(Bucket, Entry) of
        {ok, none} ->
            migrate_file_safe(Bucket, Entry);
        {ok, _Object} ->
            ok;
        Error -> Error
    end.

%% This is a pessimistic migrator; if we should discover the file exists (perhaps created separately)
%% we fail if the SHAs don't match, otherwise we succeed.
migrate_file_safe(Bucket, Entry) ->
    maybe_finalize_file(handle_open_and_copy(bksw_io:open_for_read(Bucket,Entry)), Bucket, Entry).

handle_open_and_copy({ok, #entryref{} = FileRef}) ->
    maybe_read_loop(FileRef, bksw_sql:insert_file_data());
handle_open_and_copy(Error) ->
    error_logger:error_msg("Error occurred opening file for migration: ~p~n", [Error]),
    Error.

maybe_read_loop(#entryref{} = Ref, {ok, DataId}) ->
    read_copy_loop(Ref, DataId, bksw_sql:init_transfer_state());
maybe_read_loop(#entryref{} = Ref, {error, _} = Error) ->
    cleanup_read_copy_loop(Ref, undefined),
    error_logger:error_msg("Error occurred opening file for migration: ~p~n", [Error]),
    Error.

read_copy_loop(Ref, DataId, #file_transfer_state{next_chunk = ChunkId} = State) ->
    case bksw_io:read(Ref, ?BLOCK_SIZE) of
        {ok, eof} ->
            finish_read_copy_loop(Ref, DataId, State);
        {ok, Data} ->
            ok = bksw_sql:add_file_chunk(DataId, ChunkId, Data),
            State1 = bksw_sql:update_transfer_state(State, Data, 1),
            case byte_size(Data) < ?BLOCK_SIZE of
                true ->
                    finish_read_copy_loop(Ref, DataId, State1);
                false ->
                    read_copy_loop(Ref, DataId, State1)
             end;
        Error = {error, _} ->
            cleanup_read_copy_loop(Ref, DataId),
            error_logger:error_msg("Error occurred during migration: ~p~n", [Error]),
            Error
     end.

finish_read_copy_loop(Ref, DataId, State) ->
    ok = bksw_io:finish_read(Ref),
    {DataId, bksq_sql:finalize_transfer_state(State)}.

cleanup_read_copy_loop(Ref, undefined) ->
    ok = bksw_io:finish_read(Ref);
cleanup_read_copy_loop(Ref, _DataId) ->
    ok = bksw_io:finish_read(Ref),
    %% TODO cleanup sql here too (however it is safe to abandon, because we clean up elsewhere)
    ok.


maybe_finalize_file({DataId, #db_file{data_size = Size,
                                      hash_md5 = HashMd5,
                                      hash_sha256 = HashSha256,
                                      hash_sha512 = HashSha512,
                                      chunk_count = ChunkCount}},
                    Bucket, Entry) ->
    bksw_sql:update_metadata(DataId, Size, ChunkCount, HashMd5, HashSha256, HashSha512),
    %% TODO Figure out what's happening with file creation if exists already
    %% Right now we fail out. This should be protected by the existence check before we
    %% start the copy.
    case bksw_sql:create_file_link_data(Bucket, Entry, DataId) of
        {ok, _} ->
            ok;
        {error, Reason} ->
            error_logger:error_msg("create_file_link_data ~p~n", [Reason]),
            fail
    end;
maybe_finalize_file(Error, _, _) ->
    Error.
