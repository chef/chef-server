%% @copyright Chef Software, Inc. All Rights Reserved
%% @author Tim Dysinger <dysinger@chef.io>
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
%% @doc Formatting utilities for bookshelf HTTP responses.
%%
%% All functions in this module are pure (no side effects, no process state).
%% They convert internal Erlang values into the string/binary representations
%% required by the S3-compatible bookshelf API.
-module(bksw_format).

-export([to_base64/1,
         to_date/1,
         to_etag/1,
         to_hex/1]).

%%===================================================================
%% API functions
%%===================================================================

%% @doc Format a datetime value as an ISO 8601 binary string.
%%
%% Handles three input shapes:
%%   - `undefined`         → epoch sentinel "1970-01-01T00:00:00.000Z"
%%   - `{datetime, Date}`  → unwrap the tagged tuple, then format
%%   - bare `Date`         → format directly via iso8601
-spec to_date(undefined | {datetime, calendar:datetime()} | calendar:datetime()) -> binary().
to_date(undefined) ->
    <<"1970-01-01T00:00:00.000Z">>;
to_date({datetime, Date}) ->
    iso8601:format(Date);
to_date(Date) ->
    iso8601:format(Date).

%% @doc Base64-encode a binary value, returning a flat string.
%%
%% Raises function_clause if Bin is not a binary.
-spec to_base64(binary()) -> string().
to_base64(Bin) when is_binary(Bin) ->
    base64:encode_to_string(Bin).

%% @doc Convert a binary to a hexadecimal string.
%%
%% Each byte is formatted as exactly two hex digits (zero-padded).
%%
%% The case of hex letters is controlled by the `hex_encoding_case`
%% bookshelf application environment key:
%%
%%   * `lowercase` (default) — "0aff"  (S3-standard; current behavior)
%%   * `uppercase`           — "0AFF"  (compatibility with tools that require it)
%%
%% Toggle at runtime (takes effect on the next call; no restart required):
%%
%%   application:set_env(bookshelf, hex_encoding_case, uppercase)
%%   application:set_env(bookshelf, hex_encoding_case, lowercase)
%%
%% Raises function_clause if Bin is not a binary.
-spec to_hex(binary()) -> string().
to_hex(Bin) when is_binary(Bin) ->
    Hex = lists:flatten([byte_to_hex(B) || <<B>> <= Bin]),
    case application:get_env(bookshelf, hex_encoding_case, lowercase) of
        uppercase -> string:to_upper(Hex);
        _         -> Hex
    end.

%% @doc Wrap a value in HTTP ETag double-quotes.
%%
%% If given a binary, it is first hex-encoded via to_hex/1.
%% If given a string, it is wrapped directly.
%% Example: <<"abc">> → `"616263"`
-spec to_etag(binary() | string()) -> string().
to_etag(Tag) when is_binary(Tag) ->
    to_etag(to_hex(Tag));
to_etag(Tag) ->
    io_lib:format("\"~s\"", [Tag]).

%%===================================================================
%% Internal functions
%%===================================================================

%% @private
%% @doc Format a single byte as a two-digit lowercase hex string.
-spec byte_to_hex(0..255) -> string().
byte_to_hex(Byte) ->
    io_lib:format("~2.16.0b", [Byte]).
