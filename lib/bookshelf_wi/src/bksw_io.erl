%% Copyright 2012 Opscode, Inc. All Rights Reserved.
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

-module(bksw_io).

-export([bucket_list/0,
         bucket_exists/1,
         bucket_delete/1,
         bucket_create/1]).

-export([entry_list/1,
         entry_delete/2,
         entry_exists/2,
         open_for_read/2,
         open_for_write/2,
         entry_md/2,
         entry_md/1,
         write/2,
         read/2,
         finish_read/1,
         abort_write/1,
         finish_write/1]).

-record(entryref, {fd :: file:io_device(),
                   path :: binary(),
                   ctx :: undefined | binary()}).

-include_lib("kernel/include/file.hrl").
-include("bksw_obj.hrl").

-define(MAGIC_NUMBER, <<16#b00c:16/integer>>).
-define(MAGIC_NUMBER_SIZE_BYTES, 2).
-define(CHECKSUM_SIZE_BYTES, 16).
-define(TOTAL_HEADER_SIZE_BYTES, ?MAGIC_NUMBER_SIZE_BYTES + ?CHECKSUM_SIZE_BYTES).

%% Matches file names without "._bkwbuf_" in the name
%%-define(DISCARD_WRITE_BUFS, "^(?:.(?<!\\._bkwbuf_))*$").
-define(WRITE_BUFS, ". [0-9][0-9][0-9]_bkwbuf$").

-spec bucket_list() -> [#bucket{}] | [].
bucket_list() ->
    Root = bksw_conf:disk_store(),
    make_buckets(Root, [Dir || Dir <- filelib:wildcard("*", Root),
                               filelib:is_dir(filename:join([Root, Dir]))]).

-spec entry_list(binary()) -> [#object{}] | [].
entry_list(Bucket) ->
    BucketPath = bksw_io_names:bucket_path(Bucket),
    filter_entries(Bucket, filelib:wildcard("*", BucketPath)).

-spec bucket_exists(binary()) -> boolean().
bucket_exists(Bucket) ->
    filelib:is_dir(bksw_io_names:bucket_path(Bucket)).

-spec bucket_create(binary()) -> boolean().
bucket_create(Bucket) ->
    case bucket_exists(Bucket) of
        false ->
            BucketPath = bksw_io_names:bucket_path(Bucket),
            case file:make_dir(BucketPath) of
                ok ->
                    true;
                {error, Reason} ->
                    error_logger:error_msg("Error creating bucket ~p on path ~p: ~p~n", [Bucket, BucketPath, Reason]),
                    false
            end;
        true ->
            true
    end.

-spec bucket_delete(binary()) -> boolean().
bucket_delete(Bucket) ->
    case entry_list(Bucket) of
        [] ->
            delete_bucket_dir(Bucket);
        Entries ->
            case delete_entries(Entries) of
                true ->
                    delete_bucket_dir(Bucket);
                 false ->
                    false
            end
    end.

delete_bucket_dir(Bucket) ->
    case os:cmd("rm -rf " ++ bksw_io_names:bucket_path(binary_to_list(Bucket))) of
        [] ->
            true;
        _ ->
            false
    end.


delete_entries([]) ->
    true;
delete_entries([Entry|T]) ->
    case entry_delete(Entry) of
        true ->
            delete_entries(T);
        false ->
            false
    end.

-spec entry_delete(binary(), binary()) -> boolean().
entry_delete(Bucket, Entry) ->
    entry_delete(bksw_io_names:entry_path(Bucket, Entry)).

-spec entry_delete(#object{} | binary()) -> boolean().
entry_delete(#object{path=Path}) ->
    entry_delete(bksw_io_names:entry_path(Path));
entry_delete(FullPath) ->
    ok = bksw_coordinator:commit(FullPath),
    try

        case file:delete(FullPath) of
            ok ->
                true;
            Error ->
                error_logger:error_msg("Error deleting bucket entry ~p: ~p~n", [FullPath, Error]),
                false
        end
    after
        bksw_coordinator:end_commit(FullPath)
    end.

-spec entry_exists(binary(), binary()) -> boolean().
entry_exists(Bucket, Path) ->
    FullPath = bksw_io_names:entry_path(Bucket, Path),
    filelib:is_regular(FullPath).

-spec open_for_write(binary(), binary()) -> {ok, #entryref{}} | {error, term()}.
open_for_write(Bucket, Entry) ->
    EntryPath = bksw_io_names:entry_path(Bucket, Entry),
    FileName = bksw_io_names:write_path(EntryPath),
    filelib:ensure_dir(FileName),
    case file:open(FileName, [exclusive, write, binary]) of
        {ok, Fd} ->
            %% Magic number to guard against file corruption
            case file:write(Fd, ?MAGIC_NUMBER) of
                ok ->
                    {ok, ?TOTAL_HEADER_SIZE_BYTES} = file:position(Fd, {bof, ?TOTAL_HEADER_SIZE_BYTES}),
                    {ok, #entryref{fd=Fd, path=FileName, ctx=erlang:md5_init()}};
                Error ->
                    file:close(Fd),
                    Error
            end;
        Error ->
            Error
    end.

-spec open_for_read(binary(), binary()) -> {ok, #entryref{}} | {error, term()}.
open_for_read(Bucket, Entry) ->
    FileName = bksw_io_names:entry_path(Bucket, Entry),
    ok = bksw_coordinator:start_read(FileName),
    case file:open(FileName, [read, binary]) of
        {ok, Fd} ->
            case file:read(Fd, 2) of
                %% Verify magic number is intact
                {ok, ?MAGIC_NUMBER} ->
                    %% Skip past checksum data for now
                    {ok, ?TOTAL_HEADER_SIZE_BYTES} = file:position(Fd, {bof, ?TOTAL_HEADER_SIZE_BYTES}),
                    {ok, #entryref{fd=Fd, path=FileName}};
                _ ->
                    file:close(Fd),
                    bksw_coordinator:end_read(FileName),
                    {error, corrupt_file}
            end;
        Error ->
            Error
    end.

-spec entry_md(binary(), binary()) -> {ok, #object{}} | {error, term()}.
entry_md(Bucket, Entry) ->
    {ok, Ref} = open_for_read(Bucket, bksw_io_names:decode(Entry)),
    Result = entry_md(Ref),
    finish_read(Ref),
    Result.

-spec entry_md(#entryref{}) -> {ok, #object{}} | {error, term()}.
entry_md(#entryref{fd=Fd, path=Path}) ->
    case file:read_file_info(Path) of
        {ok, #file_info{mtime = Date, size = Size}} ->
            [UTC | _] = %% FIXME This is a hack until R15B
                calendar:local_time_to_universal_time_dst(Date),
            case file_md5(Fd) of
                error ->
                    error;
                {ok, MD5} ->
                    {entry, Bucket, Entry} = bksw_io_names:parse_path(Path),
                    EntryPath = filename:join([Bucket, Entry]),
                    {ok, #object{path=EntryPath,
                                 name=Entry,
                                 date=UTC,
                                 size=Size - ?TOTAL_HEADER_SIZE_BYTES,
                                 digest=MD5}}
            end
    end.


-spec read(#entryref{}, pos_integer()) -> {ok, binary() | eof} | {error, file:posix() | badarg | terminated}.
read(#entryref{fd=Fd}, Size) ->
    case file:read(Fd, Size) of
        eof ->
            {ok, eof};
        Result ->
            Result
    end.

-spec finish_read(#entryref{}) -> ok.
finish_read(#entryref{fd=Fd, path=Path}) ->
    file:close(Fd),
    bksw_coordinator:end_read(Path).

-spec write(#entryref{}, binary()) -> {ok, #entryref{}} | {error, file:posix() | badarg | terminated}.
write(#entryref{fd=Fd, ctx=Ctx}=ERef, Data) when is_binary(Data) ->
    case file:write(Fd, Data) of
        ok ->
            {ok, ERef#entryref{ctx=erlang:md5_update(Ctx, Data)}};
        Error ->
            Error
    end.

-spec abort_write(#entryref{}) -> ok | {error, file:posix() | badarg}.
abort_write(#entryref{fd=Fd, path=Path}) ->
    file:close(Fd),
    file:delete(Path).

-spec finish_write(#entryref{}) -> {ok, binary()} | {error, file:posix() | badarg}.
finish_write(#entryref{fd=Fd, path=Path, ctx=Ctx}) ->
    case file:sync(Fd) of
        ok ->
            Digest = erlang:md5_final(Ctx),
            %% Seek to metadata section of file
            {ok, ?MAGIC_NUMBER_SIZE_BYTES} = file:position(Fd, {bof, ?MAGIC_NUMBER_SIZE_BYTES}),
            file:write(Fd, Digest),
            file:close(Fd),
            Entry = bksw_io_names:write_path_to_entry(Path),
            bksw_coordinator:commit(Entry),
            case file:rename(Path, Entry) of
                ok ->
                    bksw_coordinator:end_commit(Entry),
                    {ok, Digest};
                Error ->
                    bksw_coordinator:end_commit(Entry),
                    Error
            end;
        Error ->
            file:close(Fd),
            Error
    end.

%% Internal functions
filter_entries(_Bucket, []) ->
    [];
filter_entries(Bucket, Entries) ->
    {ok, Ex} = re:compile(?WRITE_BUFS, [unicode]),
    filter_entries(Bucket, Entries, Ex, []).

filter_entries(_Bucket, [], _Ex, Accum) ->
    lists:reverse(Accum);
filter_entries(Bucket, [Entry|T], Ex, Accum) ->
    case re:run(Entry, Ex, [{capture, none}]) of
        nomatch ->
            case entry_md(Bucket, filename:basename(Entry)) of
                {ok, Obj} ->
                    filter_entries(Bucket, T, Ex, [Obj|Accum]);
                _Error ->
                    filter_entries(Bucket, T, Ex, Accum)
            end;
        match ->
            filter_entries(Bucket, T, Ex, Accum)
    end.

file_md5(Fd) ->
    {ok, CurrPos} = file:position(Fd, {cur, 0}),
    {ok, ?MAGIC_NUMBER_SIZE_BYTES} = file:position(Fd, {bof, ?MAGIC_NUMBER_SIZE_BYTES}),
    case file:read(Fd, 16) of
        eof ->
            error;
        {ok, MD5} ->
            {ok, CurrPos} = file:position(Fd, {bof, CurrPos}),
            {ok, MD5}
    end.

make_buckets(Root, BucketDirs) ->
    make_buckets(Root, BucketDirs, []).

make_buckets(_Root, [], Buckets) ->
    lists:reverse(Buckets);
make_buckets(Root, [BucketDir|T], Buckets) ->
    Buckets1 = case file:read_file_info(filename:join([Root, BucketDir])) of
                   {ok, #file_info{mtime=Date}} ->
                       [UTC | _] = %% FIXME This is a hack until R15B
                           calendar:local_time_to_universal_time_dst(Date),
                       [#bucket{name=BucketDir,
                                date=UTC}|Buckets];
                   _Error ->
                       Buckets
               end,
    make_buckets(Root, T, Buckets1).
