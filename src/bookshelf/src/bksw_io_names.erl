%% Copyright Chef Software, Inc. All Rights Reserved.
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
         entry_path/2,
         write_path/2
        ]).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

hex2dec(X) when (X>=$0) andalso (X=<$9) -> X-$0;
hex2dec(X) when (X>=$A) andalso (X=<$F) -> X-$A+10;
hex2dec(X) when (X>=$a) andalso (X=<$f) -> X-$a+10.

-type uri() :: string() | binary().
-type hex_uri() :: string() | binary(). %% Hexadecimal encoded URI.
-type maybe_hex_uri() :: string() | binary(). %% A possibly hexadecimal encoded URI.

-spec http_uri_decode(maybe_hex_uri()) -> uri().
http_uri_decode(String) when is_list(String) ->
    do_decode(String).

do_decode([$%,Hex1,Hex2|Rest]) ->
    [hex2dec(Hex1)*16+hex2dec(Hex2)|do_decode(Rest)];
do_decode([First|Rest]) ->
    [First|do_decode(Rest)];
do_decode([]) ->
    [].

reserved() ->
    sets:from_list([$;, $:, $@, $&, $=, $+, $,, $/, $?,
            $#, $[, $], $<, $>, $\", ${, $}, $|, %"
                               $\\, $', $^, $%, $ ]).

-spec http_uri_encode(uri()) -> hex_uri().
http_uri_encode(URI) when is_list(URI) ->
    Reserved = reserved(),
    lists:append([uri_encode(Char, Reserved) || Char <- URI]).

%% In this version of the function, we no longer need
%% the Scheme argument, but just in case...
uri_encode(Char, Reserved) ->
    case sets:is_element(Char, Reserved) of
        true ->
            [ $% | http_util:integer_to_hexlist(Char)];
        false ->
            [Char]
    end.

encode(Data) when is_binary(Data) ->
    list_to_binary(encode(binary_to_list(Data)));
encode(Data) when is_list(Data) ->
    http_uri_encode(Data).

decode(Data) when is_binary(Data) ->
    list_to_binary(decode(binary_to_list(Data)));
decode(Data) when is_list(Data) ->
    http_uri_decode(Data).

bucket_path(Bucket) when Bucket =/= <<>> ->
    Root = bksw_conf:disk_store(),
    filename:join([Root, encode(Bucket)]).

entry_path(Bucket, Entry) when Bucket =/= <<>> andalso Entry =/= <<>> ->
    Root = bksw_conf:disk_store(),
    EP = filename:join([Root, encode(Bucket), entry_path_sha(Entry)]),
    iolist_to_binary(EP).

entry_path_sha(Path) ->
    EncodedPath = encode(Path),
    [D11, D12, D21, D22, D31, D32, D41, D42 | _PSHA] = sha_str(EncodedPath),
    filename:join([[D11, D12], [D21, D22], [D31, D32], [D41, D42], EncodedPath]).

sha_str(X) ->
    sha_to_hex_str(crypto:hash(sha, X)).

sha_to_hex_str(<<SHA:160/big-unsigned-integer>>) ->
    lists:flatten(io_lib:format("~40.16.0b", [SHA])).

-spec write_path(string() | binary(), string() | binary()) -> binary().
write_path(Bucket, Path) ->
    Root = bksw_conf:disk_store(),

    %% Note 2017-02-21 sr: technically, this could collide when called by two
    %% uploads happening at the same moment. Using erlang:unique_integer is not
    %% bounded -- so we add a random integer, similar to what ruby does.
    {T1, T2, T3} = os:timestamp(),
    Rand = rand:uniform(9999999),
    UniqueExt = io_lib:format(".~p~p~p~7..0B_bkwbuf", [T1, T2, T3, Rand]),
    iolist_to_binary([Root, "/", encode(Bucket), "-", sha_str(encode(Path)), UniqueExt]).

-ifdef(TEST).
encode_decode_test() ->
    ?assertEqual(<<"testing%20123">>, encode(<<"testing 123">>)),
    ?assertEqual("testing%20123", encode("testing 123")).

bucket_path_test() ->
    ?assertEqual(<<"/tmp/foo">>, bucket_path(<<"foo">>)),
    ?assertEqual(<<"/tmp/hello%20world">>, bucket_path(<<"hello world">>)).

entry_path_test() ->
    ?assertEqual(<<"/tmp/foo/62/cd/b7/02/bar">>,
                 entry_path(<<"foo">>, <<"bar">>)),

    ?assertEqual(<<"/tmp/foo/74/a0/4a/95/entry%20path%2Fabc">>,
                 entry_path(<<"foo">>, <<"entry path/abc">>)).

-endif.
