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

-module(chef_index_queue).

-export([
         delete/4,
         delete/5,
         set/5,
         set/6,
         create_management_pool/3,
         ping/1,
         message_queue_len/1
        ]).

-ifdef(TEST).
-compile([export_all]).
-endif.

-include("chef_index.hrl").

-type uuid_binary() :: <<_:288>> | <<_:256>>. %% with|without hypens are both allowed
-type chef_db_name() :: binary().
-type ejson() :: {maybe_improper_list()}. %% Is an ejson object, but the type defn for that is recursive.
-type solr_url() :: [byte()] | binary() | undefined.
-type vhost() :: binary().

-define(POOLNAME, rabbitmq_index_management_service).

%%%%
%% Public API
%%%%

-spec set(vhost(), chef_indexable_type(), uuid_binary(), chef_db_name(), ejson()) -> ok.
%% @doc Insert or update an object in Solr.
set(VHost, Type, ID, DatabaseName, Item) ->
    set(VHost, Type, ID, DatabaseName, Item, undefined).

-spec set(vhost(), chef_indexable_type(), uuid_binary(),
          chef_db_name(), ejson(), solr_url()) -> ok.
%% @doc Insert or update an object in Solr. If `SolrUrl' is provided,
%% it will be embedded in the payload under key `"solr_url"'. The
%% expander can then make use of that for routing the request. If
%% `SolrUrl' is `undefined', then the `"solr_url"' payload key is
%% omitted. In this case, we expect expander to use its configured
%% solr url value.
set(VHost, Type, ID, DatabaseName, Item, SolrUrl) ->
  PackagedData = package_for_set(Type, ID, DatabaseName, Item, SolrUrl),
  publish(VHost, PackagedData, routing_key(ID)).

-spec delete(vhost(), chef_indexable_type(), uuid_binary(), chef_db_name()) -> ok.
%% @doc Delete an object from Solr.
%% @end
%%
%% Note that the guard for this function recapitulates the chef_indexable_type()
%% custom type.  This is done out of an abundance of caution and
%% paranoia :)
delete(VHost, Type, ID, DatabaseName) ->
    delete(VHost, Type, ID, DatabaseName, undefined).

-spec delete(vhost(), chef_indexable_type(), uuid_binary(),
             chef_db_name(), solr_url()) -> ok.
%% @doc Delete an object from Solr.
%% @end
%%
%% Note that the guard for this function recapitulates the chef_indexable_type()
%% custom type.  This is done out of an abundance of caution and
%% paranoia :)
delete(VHost, Type, ID, DatabaseName, SolrUrl) when Type =:= 'client';
                                                    Type =:= 'data_bag';
                                                    Type =:= 'data_bag_item';
                                                    Type =:= 'environment';
                                                    Type =:= 'node';
                                                    Type =:= 'role' ->
    PackagedData = package_for_delete(Type, ID, DatabaseName, SolrUrl),
    publish(VHost, PackagedData, routing_key(ID)).

%%%%
%% Public for eunit
%%%%

-spec package_for_set(chef_indexable_type(), uuid_binary(), chef_db_name(), ejson(), solr_url()) ->
                             {[{'action','add'} | {'payload',{_}},...]}.
%% @doc wraps a chef object in the necessary envelopes for expander to index it.
package_for_set(Type, ID, DatabaseName, Item, SolrUrl) ->
  InnerEnvelope = inner_envelope(Type, ID, DatabaseName, Item, SolrUrl),
  {[{action, add},
    {payload, InnerEnvelope}]}.

-spec package_for_delete(chef_indexable_type(), uuid_binary(), chef_db_name(), solr_url()) ->
                                {[{'action','delete'} | {'payload',{_}},...]}.
%% @doc wraps a chef object in the necessary envelopes for expander to de-index it.
package_for_delete(Type, ID, DatabaseName, SolrUrl) ->
  InnerEnvelope = inner_envelope(Type, ID, DatabaseName, {[]}, SolrUrl),
  {[{action, delete}, {payload, InnerEnvelope}]}.


create_management_pool(Username, Password, Config) ->
    chef_wm_rabbitmq_management:create_pool(?POOLNAME, add_basic_auth(Username, Password, Config)).

-spec ping(binary()) -> pong | pang.
ping(VHost) ->
    % TODO(jaym) 2017-08-02: chef_wm_rabbitmq_management should be moved to a shared app.
    % The reason for this is because referencing chef_wm_rabbitmq_management from here
    % creates a 2 way dependency between chef_index and oc_chef_wm.
    case chef_wm_rabbitmq_management:check_aliveness(
           ?POOLNAME, binary_to_list(VHost)) of
        true -> pong;
        _ -> pang
    end.

-spec message_queue_len(binary()) -> integer() | undefined.
message_queue_len(VHost) ->
    Name = chef_index_sup:server_for_vhost(VHost),
    case whereis(Name) of
        undefined -> undefined;
        Pid ->
            {message_queue_len, Len} = erlang:process_info(Pid, message_queue_len),
            Len
    end.

%%%%
%% Internal
%%%%

add_basic_auth(Username, Password, Config) ->
    IbrowseOptions = proplists:get_value(ibrowse_options, Config),
    Config1 = proplists:delete(ibrowse_options, Config),
    IbrowseOptions1 = [{basic_auth, {Username, erlang:binary_to_list(Password)}} | IbrowseOptions],
    [{ibrowse_options, IbrowseOptions1} | Config1].

-spec inner_envelope(chef_indexable_type(), uuid_binary(), chef_db_name(), ejson(), solr_url()) -> ejson().
inner_envelope(Type, ID, DatabaseName, Item, SolrUrl) ->
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
   ] ++ maybe_solr_url(SolrUrl)}.

maybe_solr_url(undefined) ->
    [];
maybe_solr_url(Url) ->
    [{solr_url, erlang:iolist_to_binary(Url)}].

-spec publish(vhost(), {[{_, _}, ...]}, binary()) -> ok.
publish(VHost, Data, RoutingKey) ->
  %% Calls stack until return values are docco'd:
  %% bunnyc:publish
  %%    gen_server:call(name, {publish ..})
  %%      bunnyc:internal_publish(fun amqp_channel:call/3, ...)
  %%        amqp_channel:call(name, {...}) -> ok|blocked|closing
  %% blocked or closing count as errors to us, and letting errors bubble up
  %% seems fine.

    %% Jiffy may return an iolist, but publish only takes binaries.
    Bin = erlang:iolist_to_binary(jiffy:encode(Data)),
    Server = chef_index_sup:server_for_vhost(VHost),
    ok = bunnyc:publish(Server, RoutingKey, Bin).

-spec routing_key(uuid_binary()) -> binary().
routing_key(ObjectID) ->
  VnodeID = object_id_to_i(ObjectID) rem 1024,
  iolist_to_binary(["vnode-", io_lib:fwrite("~.10B", [VnodeID])]).

-spec object_id_to_i(uuid_binary()) -> non_neg_integer().
%% Strip hypens (if any) from the UUID and convert to an integer.
object_id_to_i(UUID) ->
    <<Key:160/unsigned-integer>> = crypto:hash(sha, UUID),
    Key.

unix_time() ->
  {MS, S, _US} = os:timestamp(),
  (1000000 * MS) + S.
