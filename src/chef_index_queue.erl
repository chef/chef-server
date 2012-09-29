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

-module(chef_index_queue).

-export([set/4, delete/3]).

-ifdef(TEST).
-export([package_for_set/4, package_for_delete/3, unix_time/0]).
-endif.

-define(SERVER, ?MODULE).

-include("chef_index.hrl").

-type uuid_binary() :: <<_:288>> | <<_:256>>. %% with|without hypens are both allowed
-type chef_db_name() :: binary().
-type ejson() :: {maybe_improper_list()}. %% Is an ejson object, but the type defn for that is recursive.

%%%%
%% Public API
%%%%

-spec set(chef_indexable_type(), uuid_binary(), chef_db_name(), ejson()) -> ok.
%% @doc Insert or update an object in Solr.
set(Type, ID, DatabaseName, Item) ->
  PackagedData = package_for_set(Type, ID, DatabaseName, Item),
  publish(PackagedData, routing_key(ID)).

-spec delete(chef_indexable_type(), uuid_binary(), chef_db_name()) -> ok.
%% @doc Delete an object from Solr.
%% @end
%%
%% Note that the guard for this function recapitulates the chef_indexable_type()
%% custom type.  This is done out of an abundance of caution and
%% paranoia :)
delete(Type, ID, DatabaseName) when Type =:= 'client';
                                    Type =:= 'data_bag';
                                    Type =:= 'data_bag_item';
                                    Type =:= 'environment';
                                    Type =:= 'node';
                                    Type =:= 'role' ->
    PackagedData = package_for_delete(Type, ID, DatabaseName),
    publish(PackagedData, routing_key(ID)).

%%%%
%% Public for eunit
%%%%

-spec package_for_set(chef_indexable_type(), uuid_binary(), chef_db_name(), ejson()) ->
                             {[{'action','add'} | {'payload',{_}},...]}.
%% @doc wraps a chef object in the necessary envelopes for expander to index it.
package_for_set(Type, ID, DatabaseName, Item) ->
  InnerEnvelope = inner_envelope(Type, ID, DatabaseName, Item),
  {[{action, add},
    {payload, InnerEnvelope}]}.

-spec package_for_delete(chef_indexable_type(), uuid_binary(), chef_db_name()) ->
                                {[{'action','delete'} | {'payload',{_}},...]}.
%% @doc wraps a chef object in the necessary envelopes for expander to de-index it.
package_for_delete(Type, ID, DatabaseName) ->
  InnerEnvelope = inner_envelope(Type, ID, DatabaseName, {[]}),
  {[{action, delete}, {payload, InnerEnvelope}]}.

%%%%
%% Internal
%%%%

-spec inner_envelope(chef_indexable_type(), uuid_binary(), chef_db_name(), ejson()) -> ejson().
inner_envelope(Type, ID, DatabaseName, Item) ->
  %% SAMPLE ENVELOPE:
  %%   {[{type, <<"node">>},
  %%     {id, <<"abc123def456">>},
  %%     {database, <<"chef_def123abd567">>},
  %%     {item, NodeForIndexing}, %% DEEP MERGED NODE
  %%     {enqueued_at, 1234}  %% UNIXTIME
  %%   ]}
  {[{type, Type},
    {id, ID},
    {database, DatabaseName},
    {item, Item}, %% DEEP MERGED NODE
    {enqueued_at, unix_time()}
  ]}.

-spec publish({[{_, _}, ...]}, binary()) -> ok.
publish(Data, RoutingKey) ->
  %% Calls stack until return values are docco'd:
  %% bunnyc:publish
  %%    gen_server:call(name, {publish ..})
  %%      bunnyc:internal_publish(fun amqp_channel:call/3, ...)
  %%        amqp_channel:call(name, {...}) -> ok|blocked|closing
  %% blocked or closing count as errors to us, and letting errors bubble up
  %% seems fine.
  ok = bunnyc:publish(?SERVER, RoutingKey, jiffy:encode(Data)).

-spec routing_key(uuid_binary()) -> binary().
routing_key(ObjectID) ->
  VnodeID = object_id_to_i(ObjectID) rem 1024,
  iolist_to_binary(["vnode-", io_lib:fwrite("~.10B", [VnodeID])]).

-spec object_id_to_i(uuid_binary()) -> non_neg_integer().
%% Strip hypens (if any) from the UUID and convert to an integer.
object_id_to_i(UUID) ->
    <<Key:160/unsigned-integer>> = crypto:sha(UUID),
    Key.

unix_time() ->
  {MS, S, _US} = os:timestamp(),
  (1000000 * MS) + S.
