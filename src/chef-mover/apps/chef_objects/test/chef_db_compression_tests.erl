%% Copyright 2012-2018 Chef Software, Inc.
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

-module(chef_db_compression_tests).

-include_lib("eunit/include/eunit.hrl").

-define(DATA, <<"This is a test of the emergency broadcast system. "
                "This is only a test.">>).

-define(TYPES, [chef_node, chef_role, chef_data_bag_item]).

always_compressed_test_() ->
    [ {"compressed for: " ++ atom_to_list(Type),
       fun() ->
               assert_compressed(chef_db_compression:compress(Type, ?DATA))
       end} || Type <- [chef_role, chef_data_bag_item] ].

assert_compressed(CData) ->
    ?assert(size(CData) =/= size(?DATA)),
    ?assertNot(?DATA =:= CData),
    ?assertEqual(?DATA, chef_db_compression:decompress(CData)).

-ifdef(CHEF_UNCOMPRESSED_NODE_DATA).
sometimes_compressed_test_() ->
    Tests = [{chef_node, fun assert_not_compressed/1},
             {chef_role, fun assert_compressed/1},
             {chef_data_bag_item, fun assert_compressed/1}],
    [ fun() ->
              Assert(chef_db_compression:compress(Type, ?DATA))
      end || {Type, Assert} <- Tests ].

assert_not_compressed(CData) ->
    ?assertEqual(?DATA, CData),
    ?assertEqual(?DATA, chef_db_compression:decompress(CData)).
-endif.
