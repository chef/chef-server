%% -*- erlang-indent-level: 4;indent-tabs-mode: nil; fill-column: 92 -*-
%% ex: ts=4 sw=4 et
%% @author Kevin Smith <kevin@opscode.com>
%% @author Seth Falcon <seth@opscode.com>
%% @copyright 2011-2012 Opscode, Inc.
%% @end

-module(chef_db_compression).

-export([compress/3,
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

-spec compress(db_type(), chef_compressable(), binary()) -> binary().
%% @doc Compresses data for database storage based on `DbType' and `Type'.
compress(mysql, _Type, Data) ->
    zlib:gzip(Data);
compress(pgsql, chef_node, Data) ->
    Data;
compress(pgsql, _Type, Data) ->
    zlib:gzip(Data).

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
    ejson:decode(chef_db_compression:decompress(Data)).
