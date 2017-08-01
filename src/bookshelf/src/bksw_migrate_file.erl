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

%% This uses an optimistic strategy; if a file with the same name
%% exists we skip migrationg We might want a more pessimistic strategy
%% in the future, as it's possible that the contents are not the same
%% But since this is for the purposes of Chef cookbooks, which are
%% essentially immutable, I think we are safe.
migrate_file(Bucket, Entry) ->
    case find_file(Bucket, Entry) of
        {ok, Object} ->
            ok;
        {ok, none} ->
            migrate_file_safe(Bucket, Entry);
        Error -> Error.

%% This is a pessimistic migrator; if we should discover the file exists (perhaps created separately)
%% we fail if the SHAs don't match, otherwise we succeed.
migrate_file_safe(Bucket, Entry) ->
    maybe_finalize_file(handle_open_and_copy(bksw_io:open_for_read(Bucket,Entry), Bucket, Entry)).

handle_open_and_copy({ok, #entryref{} = Ref}) ->
    maybe_read_loop(FileRef, bksw_sql:insert_file_data());
handle_open_and_copy(Error) ->
    error_logger:error_msg("Error occurred opening file for migration: ~p~n", [Error]),
    Error.

maybe_read_loop(#entryref{} = Ref, {ok, DataId}) ->
    read_loop(Ref, DataId, bksw_sql:init_transfer_state());
maybe_read_loop(#entryref{} = Ref, {error, _} = Error) ->
    finish_read(Ref),
    error_logger:error_msg("Error occurred opening file for migration: ~p~n", [Error]),
    Error.

read_copy_loop(Ref, DataId, #file_transfer_state{next_chunk = ChunkId} = State) ->
    case bksw_io:read(Ref, ?BLOCK_SIZE) of
        {ok, eof} ->
            finalize_read_copy_loop(Ref, DataId, State);
        {ok, Data} ->
            ok = bksw_sql:add_file_chunk(DataId, ChunkId, Data),
            State1 = bksw_sql:update_transfer_state(State, Data, 1),
            case byte_size(Data) < ?BLOCK_SIZE of
                true ->
                    finalize_read_copy_loop(Ref, DataId, State1);
                false ->
                    read_copy_loop(Ref, DataId, State1)
             end;
        Error = {error, _} ->
            % cleanup sql here too
            bksw_io:finish_read(Ref),
            error_logger:error_msg("Error occurred during migration: ~p~n", [Error]),
            Error
     end.

finalize_read_copy_loop(Ref, DataId, State) ->
    ok = bksw_io:finish_read(Ref),
    {DataId, bksq_sql:finalize_transfer_state(State)}.


maybe_finalize_file({DataId, #db_file{data_size = Size,
                                      hash_md5 = HashMd5,
                                      hash_sha256 = HashSha256,
                                      hash_sha512 = HashSha512} = File},
                    Bucket, Entry) ->
    bksw_sql:update_metadata(DataId, Size, ChunkCount, HashMd5, HashSha256, HashSha512),
    %% TODO Figure out what's happening with file creation if exists already
    File1 = File#db_file{name = Entry, bucket = Bucket, 
                         file_id = undefined, %% may exist?
                         data_id = DataId},
    case bksw_sql:create_file_link_data(BucketName, Name, DataId) of
        {ok, _} ->
            ok;
        {error, Reason} ->
            error_logger:error_msg("create_file_link_data ~p~n", [Reason]),
            fail
    end;
maybe_finalize_file(Error, _, _) ->
    Error.
