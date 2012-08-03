-module(bksw_file).

-export([bucket_list/0,
         bucket_exists/1,
         bucket_delete/1,
         bucket_create/1]).

-export([entry_path/2,
         entry_list/1,
         open_for_read/2,
         open_for_write/2,
         write/2,
         read/2,
         finish_read/1,
         abort_write/1,
         finish_write/1]).

%% Lifted from the re module. Too bad they don't export
%% the type for a compiled regex :-(
-type regex() :: {'re_pattern',integer(),integer(),binary()}.

-record(entryref, {fd :: file:io_device(),
                   path :: binary()}).

%% Matches file names without "._bkwbuf_" in the name
-define(DISCARD_WRITE_BUFS, "^(?:.(?<!\\._bkwbuf_))*$").

-spec entry_path(binary(), binary()) -> binary().
entry_path(Bucket, Entry) ->
    Root = bksw_conf:disk_store(),
    filename:join([Root, Bucket, Entry]).

-spec bucket_path(binary()) -> binary().
bucket_path(Bucket) ->
    Root = bksw_conf:disk_store(),
    filename:join([Root, Bucket]).

-spec bucket_list() -> [binary()] | [].
bucket_list() ->
    Root = bksw_conf:disk_store(),
    [Dir || Dir <- filelib:wildcard(Root, "*"),
            filelib:is_dir(Dir)].

-spec entry_list(binary()) -> [binary()] | [].
entry_list(Bucket) ->
    BucketPath = bucket_path(Bucket),
    filter_entries(filelib:wildcard("*", BucketPath)).

-spec bucket_exists(binary()) -> boolean().
bucket_exists(Bucket) ->
    filelib:is_dir(bucket_path(Bucket)).

-spec bucket_create(binary()) -> boolean().
bucket_create(Bucket) ->
    case bucket_exists(Bucket) of
        false ->
            BucketPath = bucket_path(Bucket),
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
    case os:cmd("/bin/rm -r " ++ binary_to_list(bucket_path(Bucket))) of
        [] ->
            true;
        Error ->
            error_logger:error_msg("Error deleting ~p~n", [Error]),
            false
    end.

-spec open_for_write(binary(), binary()) -> {ok, #entryref{}} | {error, term()}.
open_for_write(Bucket, Entry) ->
    EntryPath = entry_path(Bucket, Entry),
    FileName = temp_name(binary_to_list(EntryPath)),
    filelib:ensure_dir(FileName),
    case file:open(FileName, [exclusive, write, binary]) of
        {ok, Fd} ->
            {ok, #entryref{fd=Fd, path=FileName}};
        Error ->
            Error
    end.

-spec open_for_read(binary(), binary()) -> {ok, #entryref{}} | {error, term()}.
open_for_read(Bucket, Entry) ->
    FileName = entry_path(Bucket, Entry),
    case file:open(FileName, [read, binary]) of
        {ok, Fd} ->
            {ok, #entryref{fd=Fd, path=FileName}};
        Error ->
            Error
    end.

-spec read(#entryref{}, pos_integer()) -> {ok, binary() | eof} | {error, file:posix() | badarg | terminated}.
read(#entryref{fd=Fd}, Size) ->
    case file:read(Fd, Size) of
        eof ->
            {ok, eof};
        Result ->
            Result
    end.

-spec finish_read(#entryref{}) -> ok | {error, file:posix() | badarg}.
finish_read(#entryref{fd=Fd}) ->
    file:close(Fd).

-spec write(#entryref{}, binary()) -> ok | {error, file:posix() | badarg | terminated}.
write(#entryref{fd=Fd}, Data) when is_binary(Data) ->
    file:write(Fd, Data).

-spec abort_write(#entryref{}) -> ok | {error, file:posix() | badarg}.
abort_write(#entryref{fd=Fd, path=Path}) ->
    file:close(Fd),
    file:delete(Path).

-spec finish_write(#entryref{}) -> ok | {error, file:posix() | badarg}.
finish_write(#entryref{fd=Fd, path=Path}) ->
    case file:sync(Fd) of
        ok ->
            file:close(Fd),
            Entry = temp_name_to_entry(Path),
            bksw_coordinator:commit(Path),
            file:rename(Path, Entry);
        Error ->
            file:close(Fd),
            Error
    end.

%% Internal functions
-spec temp_name(string()) -> binary().
temp_name(Entry) ->
    {_, _, T} = erlang:now(),
    FileName = lists:flatten([Entry, io_lib:format("._bkwbuf_~p", [T])]),
    case filelib:wildcard(FileName) of
        [] ->
            list_to_binary(FileName);
        [_] ->
            temp_name(Entry)
    end.

temp_name_to_entry(TempName) ->
    filename:join([filename:dirname(TempName), filename:rootname(filename:basename(TempName))]).

filter_entries([]) ->
    [];
filter_entries(Entries) ->
    {ok, Ex} = re:compile(?DISCARD_WRITE_BUFS, [unicode]),
    filter_entries(Entries, Ex, []).

-spec filter_entries([string()] | [binary()] | [], regex(), [] | [binary()]) -> [] | [binary()].
filter_entries([], _Ex, Accum) ->
    lists:reverse(Accum);
filter_entries([Entry|T], Ex, Accum) ->
    case re:run(Entry, Ex, [{capture, first, binary}]) of
        {match, [Entry]} ->
            filter_entries(T, Ex, [Entry|Accum]);
        nomatch ->
            filter_entries(T, Ex, Accum)
    end.
