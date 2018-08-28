%% -*- erlang-indent-level: 4;indent-tabs-mode: nil; fill-column: 92 -*-
%% ex: ts=4 sw=4 et
%% @author Kevin Smith <kevin@chef.io>
%% @author Seth Falcon <seth@chef.io>
%% Copyright 2011-2018 Chef Software, Inc.
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

-export([compress/2,
         decompress/1,
         decompress_and_decode/1]).

-include("chef_types.hrl").

-type chef_compressable() :: 'chef_data_bag_item'
                          | 'chef_environment'
                          | 'chef_client'
                          | 'chef_node'
                          | 'chef_role'
                          | 'chef_cookbook_version'
                          | 'cookbook_meta_attributes'
                          | 'cookbook_metadata'
                          | 'cookbook_long_desc'.

%% Define CHEF_UNCOMPRESSED_NODE_DATA at compile time if your schema requires uncompressed
%% node data.
-ifdef(CHEF_UNCOMPRESSED_NODE_DATA).
%% skip compression of node data for quirk in node table schema.
-spec compress(chef_compressable(), binary()) -> binary().
compress(chef_node, Data) ->
    Data;
compress(_Type, Data) ->
    zlib:gzip(Data).
-else.
%% All types are compressed
-spec compress(chef_compressable(), binary()) -> binary().
compress(_Type, Data) ->
    zlib:gzip(Data).
-endif.

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
