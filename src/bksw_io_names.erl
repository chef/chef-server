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

-module(bksw_io_names).

-export([encode/1,
         decode/1,
         bucket_path/1,
         entry_path/1,
         entry_path/2,
         parse_path/1,
         write_path/2,
         write_path/1,
         write_path_to_entry/1]).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

encode(Data) when is_binary(Data) ->
    list_to_binary(encode(binary_to_list(Data)));
encode(Data) when is_list(Data) ->
    http_uri:encode(Data).

decode(Data) when is_binary(Data) ->
    list_to_binary(decode(binary_to_list(Data)));
decode(Data) when is_list(Data) ->
    http_uri:decode(Data).

bucket_path(Bucket) when Bucket =/= <<>> ->
    Root = bksw_conf:disk_store(),
    filename:join([Root, encode(Bucket)]).

entry_path(BucketEntryPath) when BucketEntryPath =/= <<>> ->
    Root = bksw_conf:disk_store(),
    filename:join([Root, BucketEntryPath]).

entry_path(Bucket, Entry) when Bucket =/= <<>> andalso Entry =/= <<>> ->
    Root = bksw_conf:disk_store(),
    filename:join([Root, encode(Bucket), encode(Entry)]).

parse_path(Path) when is_binary(Path) ->
    parse_path(binary_to_list(Path));
parse_path(Path) when is_list(Path) ->
    Root = bksw_conf:disk_store(),
    case filename:dirname(Path) -- Root of
        "" ->
            case Path == Root orelse (Root -- Path == "/") of
                false ->
                    {bucket, decode(filename:basename(Path))};
                true ->
                    {error, bad_bucket}
            end;
        Bucket ->
            {entry, decode(Bucket), decode(filename:basename(Path))}
    end.

write_path(Bucket, Path) ->
    write_path(entry_path(Bucket, Path)).

-spec write_path(string() | binary()) -> string() | binary().
write_path(Entry) when is_binary(Entry) ->
    list_to_binary(write_path(binary_to_list(Entry)));
write_path(Entry) when is_list(Entry) ->
    {T1, T2, T3} = erlang:now(),
    FileName = lists:flatten([Entry, io_lib:format(". ~p~p~p_bkwbuf", [T1, T2, T3])]),
    case filelib:wildcard(FileName) of
        [] ->
            FileName;
        [_] ->
            write_path(Entry)
    end.

write_path_to_entry(TempName) ->
    filename:join([filename:dirname(TempName), filename:rootname(filename:basename(TempName))]).

-ifdef(TEST).
encode_decode_test() ->
    ?assertMatch(<<"testing%20123">>, encode(<<"testing 123">>)),
    ?assertMatch("testing%20123", encode("testing 123")).

bucket_path_test() ->
    ?assertMatch(<<"/tmp/foo">>, bucket_path(<<"foo">>)),
    ?assertMatch(<<"/tmp/hello%20world">>, bucket_path(<<"hello world">>)).

entry_path_test() ->
    ?assertMatch(<<"/tmp/foo/bar">>, entry_path(<<"foo">>, <<"bar">>)),
    ?assertMatch(<<"/tmp/foo/entry%20path">>, entry_path(<<"foo">>, <<"entry path">>)).

parse_path_test() ->
    ?assertMatch({entry, "foo", "test entry"}, parse_path("/tmp/foo/test%20entry")),
    ?assertMatch({bucket, "foo"}, parse_path(<<"/tmp/foo">>)),
    ?assertMatch({error, bad_bucket}, parse_path("/tmp")).

-endif.
