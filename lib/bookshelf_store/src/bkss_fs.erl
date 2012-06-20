%% -*- erlang-indent-level: 4;indent-tabs-mode: nil; fill-column: 92 -*-
%% ex: ts=4 sw=4 et
%% @author Tim Dysinger <dysinger@opscode.com>
%% @copyright Copyright 2012 Opscode, Inc.
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
         obj_create/4,
         obj_get/3,
         obj_recv/6,
         obj_send/4
        ]).

-export_type([fs_data/0, dir/0]).

-include_lib("bookshelf_store/include/bookshelf_store.hrl").
-include_lib("kernel/include/file.hrl").

-define(BLOCK_SIZE, 16384).
-define(TIMEOUT_MS, 4096).

%% magic number that indicates a bookshelf file.
-define(BKSF_IND, 16#0DDBA11).

%% The total header size
-define(BKSF_HEADER_SIZE, (4 + 14 + 32)).

%% This FILE_PAD is appended to the front of files.
-define(FILE_PAD, <<?BKSF_IND:32,
                    0,0,0,0,0,0,0,0,0,0,
                    0,0,0,0,0,0,0,0,0,0,
                    0,0,0,0,0,0,0,0,0,0,
                    0,0,0,0,0,0,0,0,0,0,
                    0,0,0,0,0,0>>).

%%===================================================================
%% Types
%%===================================================================

-opaque fs_data() :: {?MODULE, dir()}.
-type dir() :: binary().
-type file_desc() :: {file:io_device(), term()}.

%%===================================================================
%% Signature API
%%===================================================================
-spec new(binary() | string()) -> fs_data().
new(Dir) when is_list(Dir) ->
    new(list_to_binary(Dir));
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
    filelib:is_dir(filename:join(Dir, encode(Bucket))).

-spec bucket_create(fs_data(), bkss_store:bucket_name()) ->
                           {fs_data(), ok | {error, Reason::term()}}.
bucket_create(State = {?MODULE, Dir}, Bucket) ->
    {State, file:make_dir(filename:join(Dir, encode(Bucket)))}.

-spec bucket_delete(fs_data(), bkss_store:bucket_name()) ->
                           {fs_data(), ok | {error, Reason::term()}}.
bucket_delete(State = {?MODULE, Dir}, Bucket) ->
    {State, file:del_dir(filename:join(Dir, encode(Bucket)))}.

-spec obj_list(fs_data(), bkss_store:bucket_name()) -> [bkss_store:object()].
obj_list({?MODULE, Dir}, BucketName) when is_binary(Dir), is_binary(BucketName) ->
    EncodedName = encode(BucketName),
    BucketPath = erlang:iolist_to_binary(filename:join(Dir, EncodedName)),
    lists:flatten(filelib:fold_files(
                    BucketPath,
                    ".*",
                    true,
                    fun(FilePath, Acc) ->
                            [file_2_object(Dir, BucketPath, EncodedName,
                                           erlang:iolist_to_binary(FilePath)) | Acc]
                    end,
                    [])).

-spec obj_exists(fs_data(), bkss_store:bucket_name(), bkss_store:path()) ->
                        boolean().
obj_exists({?MODULE, Dir}, Bucket, Path)
  when is_binary(Dir), is_binary(Bucket), is_binary(Path) ->
    filelib:is_regular(filename:join([Dir, encode(Bucket), encode(Path)])).

-spec obj_delete(fs_data(), bkss_store:bucket_name(), bkss_store:path()) ->
                        {fs_data(), ok | {error, Reason::term()}}.
obj_delete(State = {?MODULE, Dir}, Bucket, Path)
  when is_binary(Dir), is_binary(Bucket), is_binary(Path) ->
    ObjectPath = filename:join([Dir, encode(Bucket), encode(Path)]),
    {State, file:delete(ObjectPath)}.

-spec obj_meta(fs_data(), bkss_store:bucket_name(), bkss_store:path()) ->
                      {ok, bkss_store:object()} | {error, Reason::term()}.
obj_meta({?MODULE, Dir}, Bucket, Path) ->
    obj_meta_internal(Dir, encode(Bucket), encode(Path)).

-spec obj_create(fs_data(), bkss_store:bucket_name(),
                 bkss_store:path(), Data::iolist()) ->
                        {fs_data(), ok | {error, Reason::term()}}.
obj_create(State = {?MODULE, Dir}, Bucket, Path, Data) ->
    FilePath = filename:join([Dir, encode(Bucket), encode(Path)]),
    filelib:ensure_dir(FilePath),
    Resp =
        case obj_open_w(FilePath) of
            {ok, FsSt} ->
                case obj_write(FsSt, Data) of
                    {ok, NewFsSt} ->
                        obj_close(NewFsSt);
                    Any ->
                        obj_close(FsSt),
                        obj_delete(State, Bucket, Path),
                        Any
                end;
            Any -> Any
        end,
    {State, Resp}.

-spec obj_get(fs_data(), bkss_store:bucket_name(), bkss_store:path()) ->
                     {ok, binary()} | {error, Reason::term()}.
obj_get({?MODULE, Dir}, Bucket, Path) ->
    AbsPath = filename:join([Dir, encode(Bucket), encode(Path)]),
    case obj_open_r(AbsPath) of
        {ok, {Fs, _}} ->
            slurp_file(Fs, []);
        Any ->
            Any
    end.

-spec obj_copy(fs_data(), FromBucket::bkss_store:bucket_name(), FromPath::bkss_store:path(),
               ToBucket::bkss_store:bucket_name(), ToPath::bkss_store:path()) ->
                      {fs_data(), {ok, BytesCopied::non_neg_integer()} | {error, Reason::term()}}.
obj_copy(State={?MODULE, Dir}, FromBucket, FromPath, ToBucket, ToPath) ->
    {ok, CpySize} =
        file:copy(filename:join([Dir, encode(FromBucket), encode(FromPath)]),
                  filename:join([Dir, encode(ToBucket), encode(ToPath)])),
    {State, {ok, CpySize - ?BKSF_HEADER_SIZE}}.

-spec obj_send(fs_data(), bkss_store:bucket_name(),
               bkss_store:path(), Trans::bkss_transport:trans()) ->
                      {fs_data(), {ok, MD5::term()} | {error, Reason::term()}}.
obj_send(State = {?MODULE, Dir}, Bucket, Path, Trans0) ->
    FilePath = filename:join([Dir, encode(Bucket), encode(Path)]),
    Resp =
        case obj_open_r(FilePath) of
            {ok, FsSt} ->
                case read(FsSt, Trans0) of
                    {Trans1, ok} ->
                        {Trans1, obj_close(FsSt)};
                    {Trans1, {error, timeout}} ->
                        obj_close(FsSt),
                        {Trans1, {error, timeout}};
                    Any ->
                        obj_close(FsSt),
                        Any
                end;
            Any ->
                Any
        end,
    {State, Resp}.

-spec obj_recv(fs_data(), bkss_store:bucket_name(), bkss_store:path(),
               Trans::bkss_transport:trans(), Buffer::binary(), Length::non_neg_integer()) ->
                      {fs_data(), {ok, MD5::term()} | {error, Reason::term()}}.
obj_recv(State = {?MODULE, Dir}, Bucket, Path, Trans, Buffer, Length) ->
    FilePath = filename:join([Dir, encode(Bucket), encode(Path)]),
    filelib:ensure_dir(FilePath),
    Resp =
        case obj_open_w(FilePath) of
            {ok, FsSt} ->
                case write(FsSt, Trans, Length, Buffer) of
                    {Trans1, {ok, FsSt2}} ->
                        {Trans1, obj_close(FsSt2)};
                    {Trans1, {error, timeout}} ->
                        obj_close(FsSt),
                        obj_delete(State, Bucket, Path),
                        {Trans1, {error, timeout}};
                    Any ->
                        obj_close(FsSt),
                        obj_delete(State, Bucket, Path),
                        Any
                end;
            Any -> Any
        end,
    {State, Resp}.

%%===================================================================
%% Internal Functions
%%===================================================================
-spec read(file_desc(), bkss_transport:trans()) ->
                  ok | {error, Reason::term()}.
read({File, _} = FsSt, Trans0) ->
    case file:read(File, ?BLOCK_SIZE) of
        {ok, Chunk} ->
            {Trans1, _} = bkss_transport:send(Trans0, Chunk),
            read(FsSt, Trans1);
        eof ->
            {Trans0, ok};
        Any ->
            {Trans0, Any}
    end.

-spec write(file_desc(), Trans::bkss_transport:trans(),
            Length::non_neg_integer(), Buf::binary()) ->
                   {ok, file_desc()} | {error, Reason::term()}.
write(FsSt, Trans, Length, <<>>) ->
    write(FsSt, Trans, Length);
write(FsSt, Trans, Length, Buf) ->
    case obj_write(FsSt, Buf) of
        {ok, NewFsSt} ->
            write(NewFsSt, Trans, Length - byte_size(Buf));
        Any ->
            {Trans, Any}
    end.

-spec write(file_desc(), Trans::bkss_transport:trans(), Length::non_neg_integer()) ->
                   {ok, file_desc()} | {error, Reason::term()}.
write(FsSt, Trans, 0) ->
    {Trans, obj_write(FsSt, <<>>)};
write(FsSt, Trans0, Length) when Length =< ?BLOCK_SIZE ->
    case bkss_transport:recv(Trans0, Length) of
        {Trans1, {ok, Chunk}} ->
            {Trans1, obj_write(FsSt, Chunk)};
        Any ->
            {Trans0, Any}
    end;
write(FsSt, Trans0, Length) ->
    case bkss_transport:recv(Trans0, ?BLOCK_SIZE) of
        {Trans1, {ok, Chunk}} ->
            case obj_write(FsSt, Chunk) of
                {ok, NewFsSt} ->
                    write(NewFsSt, Trans1, Length-?BLOCK_SIZE);
                Any ->
                    {Trans1, Any}
            end;
        Any ->
            {Trans0, Any}
    end.

-spec dir_2_bucket(dir()) -> bkss_store:bucket().
dir_2_bucket(Dir) ->
    %% crash if no access to any bucket dir
    {ok, #file_info{ctime = Date}} =
        file:read_file_info(Dir),
    [UTC | _] =
        calendar:local_time_to_universal_time_dst(Date),
    #bucket{name = decode(filename:basename(Dir)), date = UTC}.

-spec file_2_object(dir(), binary(), bkss_store:bucket_name(),binary()) ->
                           bkss_store:object() | [].
file_2_object(Dir, BucketPath, BucketName, FilePath) ->
    case filelib:is_regular(FilePath) of
        true ->
            Pos = byte_size(FilePath),
            Len = byte_size(BucketPath) + 1
                - byte_size(FilePath),
            Name = binary:part(FilePath, Pos, Len),
            case obj_meta_internal(Dir, BucketName, Name) of
                {ok, Object} ->
                    Object;
                _ ->
                    []
            end;
        _ ->
            []
    end.

-spec file_md5(file:io_device()) -> {ok, MD5::binary()}.
file_md5(File) ->
    case file:read(File, ?BKSF_HEADER_SIZE) of
        {ok, <<?BKSF_IND:32,
               _TimeStamp:14/binary, MD5:32/binary>>} ->
            {ok, MD5};
        _ ->
            {error, invalid_file}
    end.

-spec obj_open(file:filename(), list()) ->
                      {ok, {file:io_device(), binary()}}  |
                      {error, Reason::term()}.
obj_open(Path, Opts) ->
    Exists = filelib:is_regular(Path),
    case file:open(Path, Opts) of
        {ok, File} ->
            handle_file_pad(File, Exists, Opts),
            {ok, {File, erlang:md5_init()}};
        Any        ->
            Any
    end.

-spec handle_file_pad(file:io_device(), boolean(), list()) -> ok.
handle_file_pad(File, true, _Opts) ->
    %% Reading or writing we want to make sure we are
    %% avoding the header
    {ok, ?BKSF_HEADER_SIZE} = file:position(File, ?BKSF_HEADER_SIZE);
handle_file_pad(File, false, Opts) ->
    case lists:member(write, Opts) of
        true ->
            ok = file:write(File, ?FILE_PAD);
        false ->
            ok
    end.

-spec obj_open_w(file:filename()) ->
                        {ok, {file:io_device(), binary()}}  |
                        {error, Reason::term()}.
obj_open_w(Path) ->
    obj_open(Path, [raw, binary, write]).

-spec obj_open_r(file:filename()) ->
                        {ok, {file:io_device(), binary()}}  |
                        {error, Reason::term()}.
obj_open_r(Path) ->
    obj_open(Path, [raw, binary, read_ahead]).

obj_write({File, Ctx}, Chunk) ->
    case file:write(File, Chunk) of
        ok  ->
            {ok, {File, erlang:md5_update(Ctx, Chunk)}};
        Any ->
            Any
    end.

-spec obj_close(file_desc()) -> {ok, MD5::term()} | {error, Reason::term()}.
obj_close({File, Ctx}) ->
    FinalMD5 = erlang:md5_final(Ctx),
    %% The first four bytes are the BKSF indicator
    {ok, 4} = file:position(File, 4),
    file:write(File, create_timestamp()),
    file:write(File, to_hex(FinalMD5)),
    case file:close(File) of
        ok  ->
            {ok, FinalMD5};
        Any ->
            Any
    end.

-spec encode(string() | binary()) -> binary().
encode(Name) when is_binary(Name) ->
    encode(erlang:binary_to_list(Name));
encode(Name) when is_list(Name) ->
    erlang:list_to_binary(http_uri:encode(Name)).

-spec decode(string() | binary()) -> binary().
decode(Name) when is_binary(Name) ->
    decode(erlang:binary_to_list(Name));
decode(Name) ->
    erlang:list_to_binary(http_uri:decode(Name)).

-spec obj_meta_internal(dir(), bkss_store:bucket_name(), bkss_store:path()) ->
                               {ok, bkss_store:object()} |
                               {error, Reason::term()}.
obj_meta_internal(Dir, Bucket, Path) ->
    %% Bucket and path are already encoded by callers
    Filename = filename:join([Dir, Bucket, Path]),
    case file:open(Filename, [binary,raw,read_ahead]) of
        {ok, File} ->
            {ok, Md5} = file_md5(File),
            case file:read_file_info(Filename) of
                {ok, #file_info{mtime = Date, size = Size}} ->
                    [UTC | _] = %% FIXME This is a hack until R15B
                        calendar:local_time_to_universal_time_dst(Date),
                    {ok, #object{name = decode(Path),
                                 date = UTC,
                                 size = Size,
                                 digest = Md5}};
                Any ->
                    Any
            end;
        Any ->
            Any
    end.


-spec to_hex(binary()) -> binary().
to_hex(Bin) ->
    erlang:iolist_to_binary(
      string:to_lower(lists:flatten([io_lib:format("~2.16.0b",
                                                   [N])
                                     || <<N>> <= Bin]))).

-spec create_timestamp() -> binary().
create_timestamp() ->
    DateTime = calendar:now_to_datetime(now()),
    {{Year, Month, Day}, {Hour, Min, Sec}} = DateTime,
    erlang:iolist_to_binary(io_lib:format("~4..0B~2..0B~2..0B~2..0B~2..0B~2..0B",
                                          [Year, Month, Day, Hour, Min, Sec])).


-spec slurp_file(file:io_device(), [binary()]) -> {ok, binary()}.
slurp_file(File, Acc) ->
    case file:read(File, ?BLOCK_SIZE) of
        {ok, Chunk} ->
            slurp_file(File, [Chunk, Acc]);
        eof ->
            file:close(File),
            {ok, erlang:iolist_to_binary(lists:reverse(Acc))}
    end.
