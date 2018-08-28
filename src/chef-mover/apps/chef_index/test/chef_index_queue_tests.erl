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

-module(chef_index_queue_tests).

-include_lib("eunit/include/eunit.hrl").
-define(ObjectID, <<"ffffffffffffffffffffffffffffffff">>).
-define(ChefDB, <<"chef_00000000000000000000000000000000">>).

package_data_for_insert_test() ->
  UUID = ?ObjectID,
  ChefDB = ?ChefDB,
  SampleNodeData = {[{name, <<"sample_node">>}, {run_list, []}]},
  Actual = chef_index_queue:package_for_set(node, UUID, ChefDB, SampleNodeData, undefined),
  {[{action, add}, {payload, InnerEnvelope}]} = Actual,
  {[{type, node},
    {id, ?ObjectID},
    {database, ?ChefDB},
    {item, Item},
    {enqueued_at, _Time}]} = InnerEnvelope,
  SampleNodeData = Item.

package_data_for_delete_test() ->
  UUID = ?ObjectID,
  ChefDB = ?ChefDB,
  Actual = chef_index_queue:package_for_delete(node, UUID, ChefDB, undefined),
  {[{action, delete}, {payload, InnerEnvelope}]} = Actual,
  {[{type, node},
    {id, ?ObjectID},
    {database, ?ChefDB},
    {item, {[]}},
    {enqueued_at, _Time}]} = InnerEnvelope.

send_data_to_amqp_test_() ->
    {foreach,
      fun() ->
              meck:new(bunnyc)
      end,
      fun(_) ->
              meck:unload(bunnyc)
      end,
    [
      {"add an item to the index",
        fun() ->
            SampleNodeData = {[{<<"name">>, <<"sample_node">>}, {<<"run_list">>, []}]},
            %% Kinda lame to stuff all this in the function stub, but this lets
            %% us use pattern matching to check small pieces at a time
            AssertPublishDataCorrect =
                    fun(ServerName, RoutingKey, Data) ->
                            ?assertEqual('chef_index_queue/chef', ServerName),
                            ?assertEqual(<<"vnode-257">>, RoutingKey),
                            {DecodedData} = jiffy:decode(Data),
                            {Payload} = proplists:get_value(<<"payload">>, DecodedData),
                            ?assertEqual(<<"node">>, proplists:get_value(<<"type">>, Payload)),
                            ?assertEqual(?ObjectID, proplists:get_value(<<"id">>, Payload)),
                            ?assertEqual(?ChefDB, proplists:get_value(<<"database">>, Payload)),
                            ?assertEqual(SampleNodeData, proplists:get_value(<<"item">>, Payload)),
                            ?assert(is_integer(proplists:get_value(<<"enqueued_at">>, Payload)))
            end,
            meck:expect(bunnyc, publish, AssertPublishDataCorrect),
            chef_index_queue:set(<<"/chef">>, node, ?ObjectID, ?ChefDB, SampleNodeData)
        end},

     {"add an item to the index with custom solr URL",
        fun() ->
                SampleNodeData = {[{<<"name">>, <<"sample_node">>}, {<<"run_list">>, []}]},
                %% Kinda lame to stuff all this in the function stub, but this lets
                %% us use pattern matching to check small pieces at a time
                SolrUrl = "http://custom-solr",
                AssertPublishDataCorrect =
                    fun(_ServerName, RoutingKey, Data) ->
                            ?assertEqual(<<"vnode-257">>, RoutingKey),
                            {DecodedData} = jiffy:decode(Data),
                            {Payload} = proplists:get_value(<<"payload">>, DecodedData),
                            ?assertEqual(<<"node">>, proplists:get_value(<<"type">>, Payload)),
                            ?assertEqual(?ObjectID, proplists:get_value(<<"id">>, Payload)),
                            ?assertEqual(?ChefDB, proplists:get_value(<<"database">>, Payload)),
                            ?assertEqual(list_to_binary(SolrUrl), proplists:get_value(<<"solr_url">>, Payload)),
                            ?assertEqual(SampleNodeData, proplists:get_value(<<"item">>, Payload)),
                            ?assert(is_integer(proplists:get_value(<<"enqueued_at">>, Payload)))
            end,
            meck:expect(bunnyc, publish, AssertPublishDataCorrect),
            chef_index_queue:set(<<"/chef">>, node, ?ObjectID, ?ChefDB, SampleNodeData, SolrUrl)
        end},

     {"delete an item from the index",
        fun() ->
                AssertPublishDataCorrect =
                    fun(_ServerName, RoutingKey, Data) ->
                            ?assertEqual(<<"vnode-257">>, RoutingKey),
                            {DecodedData} = jiffy:decode(Data),
                            {Payload} = proplists:get_value(<<"payload">>, DecodedData),
                            ?assertEqual(<<"node">>, proplists:get_value(<<"type">>, Payload)),
                            ?assertEqual(?ObjectID, proplists:get_value(<<"id">>, Payload)),
                            ?assertEqual(?ChefDB, proplists:get_value(<<"database">>, Payload)),
                            ?assertEqual({[]}, proplists:get_value(<<"item">>, Payload)),
                            ?assert(is_integer(proplists:get_value(<<"enqueued_at">>, Payload)))
                    end,
                meck:expect(bunnyc, publish, AssertPublishDataCorrect),
                chef_index_queue:delete(<<"/chef">>, node, ?ObjectID, ?ChefDB)
        end}
    ]}.
