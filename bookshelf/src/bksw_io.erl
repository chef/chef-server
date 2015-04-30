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
         entry_md/1,
         entry_md/2,
         write/2,
         read/2,
         finish_read/1,
         abort_write/1,
         finish_write/1]).

-export([
         disk_format_version/0,
         ensure_disk_store/0,
         upgrade_disk_format/0
         ]).

-include_lib("kernel/include/file.hrl").
-include("internal.hrl").

-define(DISK_FORMAT_VERSION, 1).
%% Use _%_ prefix since this cannot appear as a bucket name (we
%% encode bucket names so no bare %'s). Prefer this to a hidden
%% file since that reduces the chance of missing the version file
%% as part of backup/restore.
-define(FORMAT_VERSION_FILE, "_%_BOOKSHELF_DISK_FORMAT").
-define(MAGIC_NUMBER, <<16#b00c:16/integer>>).
-define(MAGIC_NUMBER_SIZE_BYTES, 2).
-define(CHECKSUM_SIZE_BYTES, 16).
-define(TOTAL_HEADER_SIZE_BYTES, ?MAGIC_NUMBER_SIZE_BYTES + ?CHECKSUM_SIZE_BYTES).

%% Matches file names without "._bkwbuf_" in the name
%%-define(DISCARD_WRITE_BUFS, "^(?:.(?<!\\._bkwbuf_))*$").
-define(WRITE_BUFS, ". [0-9][0-9][0-9]_bkwbuf$").

-spec bucket_list() -> [#bucket{}] | [].
bucket_list() ->
    ?LOG_DEBUG("reading bucket list"),
    Root = bksw_conf:disk_store(),
    make_buckets(Root, [Dir || Dir <- filelib:wildcard("*", bksw_util:to_string(Root)),
                               filelib:is_dir(filename:join([Root, Dir]))]).

-spec entry_list(binary()) -> [#object{}] | [].
entry_list(Bucket) ->
    BucketPath = bksw_io_names:bucket_path(Bucket),
    ?LOG_DEBUG("reading entries for bucket '~p'", [Bucket]),
    %% As of R16, second arg to filelib:wildcard must be string
    filter_entries(Bucket, filelib:wildcard("*/*/*/*/*", bksw_util:to_string(BucketPath))).

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
                    ?LOG_DEBUG("created bucket '~p' at '~p'", [Bucket, BucketPath]),
                    true;
                {error, Reason} ->
                    ?LOG_ERROR("Error creating bucket ~p on path ~p: ~p~n", [Bucket, BucketPath, Reason]),
                    false
            end;
        true ->
            true
    end.

-spec bucket_delete(binary()) -> boolean().
bucket_delete(Bucket) ->
    delete_bucket_dir(Bucket).

delete_bucket_dir(Bucket) ->
    case os:cmd("rm -rf " ++ bksw_io_names:bucket_path(binary_to_list(Bucket))) of
        [] ->
            ?LOG_DEBUG("deleted bucket ~p", [Bucket]),
            true;
        Why ->
            ?LOG_ERROR("bucket delete failed for bucket ~p: ~p", [Bucket, Why]),
            false
    end.

-spec entry_delete(binary(), binary()) -> boolean().
entry_delete(Bucket, Entry) ->
    entry_delete(bksw_io_names:entry_path(Bucket, Entry)).

-spec entry_delete(binary()) -> boolean().
entry_delete(FullPath) ->
    case file:delete(FullPath) of
        ok ->
            ?LOG_DEBUG("deleted bucket entry: ~p", [FullPath]),
            true;
        Error ->
            error_logger:error_msg("Error deleting bucket entry ~p: ~p~n", [FullPath, Error]),
            false
    end.

-spec entry_exists(binary(), binary()) -> boolean().
entry_exists(Bucket, Path) ->
    FullPath = bksw_io_names:entry_path(Bucket, Path),
    Ans = filelib:is_regular(FullPath),
    ?LOG_DEBUG("entry_exists ~p ~p ~p", [Bucket, Path, Ans]),
    Ans.

-spec open_for_write(binary(), binary()) -> {ok, #entryref{}} | {error, term()}.
open_for_write(Bucket, Entry) ->
    FileName = bksw_io_names:write_path(Bucket, Entry),
    filelib:ensure_dir(FileName),
    case file:open(FileName, [exclusive, write, binary, raw]) of
        {ok, Fd} ->
            ?LOG_DEBUG("open_for_write ~p ~p at ~p", [Bucket, Entry, FileName]),
            %% Magic number to guard against file corruption
            case file:write(Fd, ?MAGIC_NUMBER) of
                ok ->
                    {ok, ?TOTAL_HEADER_SIZE_BYTES} = file:position(Fd, {bof, ?TOTAL_HEADER_SIZE_BYTES}),
                    {ok, #entryref{fd=Fd, path=FileName,
                                   bucket=Bucket, entry=Entry,
                                   ctx=erlang:md5_init()}};
                Error ->
                    ?LOG_ERROR("header write failed ~p ~p at ~p: ~p",
                               [Bucket, Entry, FileName, Error]),
                    file:close(Fd),
                    Error
            end;
        Error ->
            ?LOG_ERROR("open_for_write failed ~p ~p at ~p: ~p",
                       [Bucket, Entry, FileName, Error]),
            Error
    end.

-spec open_for_read(binary(), binary()) -> {ok, #entryref{}} | {error, term()}.
open_for_read(Bucket, Entry) ->
    FileName = bksw_io_names:entry_path(Bucket, Entry),
    case file:open(FileName, [read, binary, raw]) of
        {ok, Fd} ->
            ?LOG_DEBUG("open_for_read entry ~p ~p at ~p",
                       [Bucket, Entry, FileName]),
            case file:read(Fd, 2) of
                %% Verify magic number is intact
                {ok, ?MAGIC_NUMBER} ->
                    %% Skip past checksum data for now
                    {ok, ?TOTAL_HEADER_SIZE_BYTES} = file:position(Fd, {bof, ?TOTAL_HEADER_SIZE_BYTES}),
                    {ok, #entryref{fd=Fd, path=FileName, bucket=Bucket, entry=Entry}};
                ReadError ->
                    ?LOG_ERROR("open_for_read corrupt file ~p ~p at ~p",
                               [Bucket, Entry, FileName, ReadError]),
                    file:close(Fd),
                    {error, corrupt_file}
            end;
        Error ->
            ?LOG_ERROR("open_for_read failed for ~p ~p at ~p: ~p",
                       [Bucket, Entry, FileName, Error]),
            Error
    end.

-spec entry_md(binary(), binary()) -> {ok, #object{}} | {error, term()}.
entry_md(Bucket, Entry) ->
    {ok, Ref} = open_for_read(Bucket, bksw_io_names:decode(Entry)),
    Result = entry_md(Ref),
    finish_read(Ref),
    ?LOG_DEBUG("entry_md read ~p ~p", [Bucket, Entry]),
    Result.

-spec entry_md(#entryref{}) -> {ok, #object{}} | error.
entry_md(#entryref{fd=Fd, path=Path, bucket=Bucket, entry=Entry}) ->
    case file:read_file_info(Path) of
        {ok, #file_info{mtime = Date, size = Size}} ->
            [UTC | _] = %% FIXME This is a hack until R15B
                calendar:local_time_to_universal_time_dst(Date),
            case file_md5(Fd) of
                error ->
                    ?LOG_ERROR("entry_md md5 read failed ~p ~p at ~p",
                               [Bucket, Entry, Path]),
                    error;
                {ok, MD5} ->
                    {ok, #object{path=Path,
                                 name=Entry,
                                 date=UTC,
                                 size=Size - ?TOTAL_HEADER_SIZE_BYTES,
                                 digest=MD5}}
            end;
        Error ->
            ?LOG_ERROR("entry_md failed ~p ~p at ~p: ~p",
                       [Bucket, Entry, Path, Error]),
            erlang:error(Error)
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
finish_read(#entryref{fd=Fd, bucket=Bucket, entry=Entry}) ->
    ?LOG_DEBUG("finish_read ~p ~p", [Bucket, Entry]),
    file:close(Fd).

-spec write(#entryref{}, binary()) -> {ok, #entryref{}} | {error, file:posix() | badarg | terminated}.
write(#entryref{fd=Fd, ctx=Ctx,
                bucket=Bucket, entry=Entry}=ERef, Data) when is_binary(Data) ->
    case file:write(Fd, Data) of
        ok ->
            {ok, ERef#entryref{ctx=erlang:md5_update(Ctx, Data)}};
        Error ->
            ?LOG_ERROR("write failed ~p ~p: ~p",
                       [Bucket, Entry, Error]),
            Error
    end.

-spec abort_write(#entryref{}) -> ok | {error, file:posix() | badarg}.
abort_write(#entryref{fd=Fd, path=Path}) ->
    file:close(Fd),
    file:delete(Path).

-spec finish_write(#entryref{}) -> {ok, binary()} | {error, file:posix() | badarg}.
finish_write(#entryref{fd=Fd, path=Path, bucket=Bucket, entry=Entry, ctx=Ctx}) ->
    case file:sync(Fd) of
        ok ->
            Digest = erlang:md5_final(Ctx),
            %% Seek to metadata section of file
            {ok, ?MAGIC_NUMBER_SIZE_BYTES} = file:position(Fd, {bof, ?MAGIC_NUMBER_SIZE_BYTES}),
            file:write(Fd, Digest),
            file:close(Fd),
            FinalPath = bksw_io_names:entry_path(Bucket, Entry),
            case filelib:ensure_dir(FinalPath) of
                ok ->
                    case file:rename(Path, FinalPath) of
                        ok ->
                            ?LOG_DEBUG("write completed ~p ~p", [Bucket, Entry]),
                            {ok, Digest};
                        Error ->
                            ?LOG_ERROR("rename failed ~p ~p (~p > ~p): ~p",
                                       [Bucket, Entry, Path, FinalPath, Error]),
                            Error
                    end;
                DirError ->
                    ?LOG_ERROR("mkdir failed ~p ~p (~p): ~p",
                               [Bucket, Entry, FinalPath, DirError]),
                    DirError
            end;
        Error ->
            ?LOG_ERROR("sync failed ~p ~p: ~p", [Bucket, Entry, Error]),
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
                       [#bucket{name=bksw_io_names:decode(BucketDir),
                                date=UTC}|Buckets];
                   _Error ->
                       Buckets
               end,
    make_buckets(Root, T, Buckets1).

%% @doc Return the on disk format version. If no version file is
%% found, returns `{version, 0}' which is the first shipping format.
-spec disk_format_version() -> {version, integer()}.
disk_format_version() ->
    Root = bksw_conf:disk_store(),
    VersionFile = filename:join([Root, ?FORMAT_VERSION_FILE]),
    case filelib:is_file(VersionFile) of
        false ->
            %% we assume version 0 if no version file is found.
            {version, 0};
        true ->
            {version, read_format_version(file:read_file(VersionFile))}
    end.

read_format_version({ok, Bin}) ->
    %% format version data is plain text with integer version number
    %% as first space separated token on first line.
    Line1 = hd(re:split(Bin, "\n")),
    Token1 = hd(string:tokens(binary_to_list(Line1), " ")),
    list_to_integer(Token1).

ensure_disk_store() ->
    Root = bksw_conf:disk_store(),
    ToEnsure = filename:join([Root, "placehold"]),
    case filelib:is_dir(Root) of
        true -> ?LOG_INFO("Found disk_store at ~s", [Root]);
        false -> ?LOG_INFO("Disk store dir did not exist. creating disk_store at ~s", [Root])
    end,
    ok = filelib:ensure_dir(ToEnsure),
    ok.

upgrade_disk_format() ->
    upgrade_disk_format(disk_format_version()).

upgrade_disk_format({version, ?DISK_FORMAT_VERSION}) ->
    ?LOG_INFO("Found disk format version ~p",
              [?DISK_FORMAT_VERSION]),
    ok;
upgrade_disk_format({version, 0}) ->
    ?LOG_INFO("Found disk format version 0. Starting upgrade to version ~p",
              [?DISK_FORMAT_VERSION]),
    ok = upgrade_from_v0(),
    upgrade_disk_format(disk_format_version());
upgrade_disk_format({version, X}) ->
    error({upgrade_disk_format, "unsupported upgrade", X, ?DISK_FORMAT_VERSION}).


%% write in-progress version file?
%% get list of buckets.
%% for each bucket, list of entries (flat, ignore directories).
%% within bucket, move entry to new path
%% write version file.
upgrade_from_v0() ->
    [ upgrade_bucket(B) || #bucket{name = B} <- bucket_list() ],
    write_format_version(),
    ok.

upgrade_bucket(Bucket) ->
    ?LOG_INFO("migrating bucket: ~p~n", [Bucket]),
    RawBucket = bksw_io_names:decode(Bucket),
    BucketPath = bksw_io_names:bucket_path(RawBucket),
    Entries = filelib:wildcard(bksw_util:to_string(BucketPath) ++ "/*"),
    FileEntries = [ F || F <- Entries,
                         filelib:is_dir(F) == false ],
    EntryCount = length(FileEntries),
    ?LOG_INFO("bucket ~p: found ~p entries",
               [Bucket, EntryCount]),
    init_progress_log(["bucket ", Bucket, ": "], EntryCount),
    [
     begin
         BaseName = filename:basename(F),
         %% entry_path wants the decoded name since it encodes.
         NewPath = bksw_io_names:entry_path(RawBucket,
                                            bksw_io_names:decode(BaseName)),
         ok = filelib:ensure_dir(NewPath),
         ok = file:rename(F, NewPath),
         log_progress()
     end || F <- FileEntries ].

write_format_version() ->
    Path = filename:join(bksw_conf:disk_store(), ?FORMAT_VERSION_FILE),
    ok = file:write_file(Path, io_lib:format("~B~n", [?DISK_FORMAT_VERSION])),
    ok.

init_progress_log(Prefix, Total) ->
    put(progress_log, {iolist_to_binary(Prefix), Total, 0}).

log_progress() ->
    log_progress(get(progress_log)).

log_progress(undefined) ->
    erlang:error(uninitialized);
log_progress({Prefix, Total, Current0}) ->
    Current = Current0 + 1,
    Pct = (Current * 100) div Total,
    %% emit log every 10%
    case Pct rem 10 of
        0 ->
            error_logger:info_msg("~s~p% complete (~p/~p)", [Prefix, Pct, Current, Total]);
        _ ->
            ok
    end,
    put(progress_log, {Prefix, Total, Current}),
    ok.
