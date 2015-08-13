%% -*- erlang-indent-level: 4;indent-tabs-mode: nil; fill-column: 92 -*-
%% ex: ts=4 sw=4 et
%% @author Kevin Smith <kevin@opscode.com>
%% @author Seth Falcon <seth@opscode.com>
%% Copyright 2011-2012 Opscode, Inc. All Rights Reserved.
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


-module(chef_db_compression).

-export([decompress/1,
         decompress_and_decode/1]).

-include("chef_types.hrl").

-spec decompress(binary()) -> binary().
%% @doc Decompresses gzip data and lets non-gzip data pass through
decompress(<<31, 139, _Rest/binary>>=GzipData) ->
    zlib:gunzip(GzipData);
decompress(Data) ->
     Data.

%% @doc Does what it says on the tin.  If `Data' isn't GZipped, the decompression is a
%% no-op.
-spec decompress_and_decode(Data :: binary()) -> ejson_term().
decompress_and_decode(Data) ->
    chef_json:decode(chef_db_compression:decompress(Data)).
