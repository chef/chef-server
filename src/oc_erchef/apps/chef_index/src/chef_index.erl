%% Copyright 2015 Chef Software, Inc
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

-module(chef_index).

-export([search/1,
         update/1,
         update/2,
         query_from_params/4,
         add_org_guid_to_query/2,
         delete_search_db/1,
         delete_search_db_by_type/2,
         transform_data/1,
         transform_data/2,
         delete/4,
         add/5,
         add_batch/1,
         search_provider/0,
         ping/0
        ]).

-include("chef_solr.hrl").

search_provider() ->
    envy:get(chef_index, search_provider, solr, envy:one_of([solr, elasticsearch])).

-spec search(#chef_solr_query{}) ->
                    {ok, non_neg_integer(), non_neg_integer(), [binary()]} |
                    {error, {solr_400, string()}} |
                    {error, {solr_500, string()}}.
search(Query = #chef_solr_query{search_provider=elasticsearch}) ->
    chef_elasticsearch:search(Query);
search(Query = #chef_solr_query{search_provider=solr}) ->
    chef_solr:search(Query).

-spec update(iolist() | binary()) -> ok | {error, term()}.
update(Body) ->
    update(search_provider(), Body).

update(elasticsearch, Body) ->
    chef_elasticsearch:update(Body);
update(solr, Body) ->
    chef_solr:update(Body).

-spec delete_search_db(OrgId :: binary()) -> ok.
delete_search_db(OrgId) ->
    case search_provider() of
        elasticsearch -> chef_elasticsearch:delete_search_db(OrgId);
        solr -> chef_solr:delete_search_db(OrgId)
    end.

-spec delete_search_db_by_type(OrgId :: binary(), Type :: atom()) -> ok.
delete_search_db_by_type(OrgId, Type) ->
    case search_provider() of
        elasticsearch -> chef_elasticsearch:delete_search_db_by_type(OrgId, Type);
        solr -> chef_solr:delete_search_db_by_type(OrgId, Type)
    end.

-spec query_from_params(binary()|string(),
                        string() | binary() | undefined,
                        string(),
                        string()) -> #chef_solr_query{}.
query_from_params(ObjType, QueryString, Start, Rows) ->
    chef_index_query:from_params(search_provider(), ObjType, QueryString, Start, Rows).

-spec add_org_guid_to_query(#chef_solr_query{}, binary()) -> #chef_solr_query{}.
add_org_guid_to_query(Query, OrgGuid) ->
    chef_index_query:add_org_guid_to_query(Query, OrgGuid).

transform_data(Data) ->
    transform_data(search_provider(), Data).

transform_data(elasticsearch, Data) ->
    chef_elasticsearch:transform_data(Data);
transform_data(solr, Data) ->
    chef_solr:transform_data(Data).

add(TypeName, Id, DbName, IndexEjson, ReqId) ->
    QueueMode = queue_mode(),
    case QueueMode of
        rabbitmq ->
            ok = chef_index_queue:set(envy:get(chef_index, rabbitmq_vhost, binary), TypeName, Id, DbName, IndexEjson);
        _ -> %% else batch or inline, create doc
            TypeName2 = case TypeName of
                            data_bag_item ->
                                ej:get({<<"data_bag">>}, IndexEjson);
                            T ->
                                T
                        end,
            Doc = chef_index_expand:doc_for_index(TypeName2, Id, DbName, IndexEjson),
            send_to_solr(QueueMode, Doc, ReqId)
    end.

-spec add_batch(list()) -> ok | {error, list()}.
add_batch(Batch) ->
    {ok, BatchWorker} = chef_wait_group:start_link(fun add_batch_item_with_retries/1, []),
    [chef_wait_group:add(BatchWorker, {TypeName, Id, DbName}, Item) || {TypeName, Id, DbName, _} = Item <- Batch],
    case chef_wait_group:wait(BatchWorker) of
        {ok, Results}  ->
            case not_ok(Results) of
                [] -> ok;
                R -> {error, R}
            end;
        {error, Results, FailedJobs} ->
            {error, [not_ok(Results)|FailedJobs]}
    end.

add_batch_item_with_retries(Item) ->
    MaxRetries = envy:get(chef_index, reindex_item_retries, 3, non_neg_integer),
    %% We don't need secure random numbers, we just want to make sure
    %% that we get some variance across our workers. If we don't seed
    %% here all items end up getting the same random numbers.
    random:seed(os:timestamp()),
    add_batch_item_with_retries(Item, 0, MaxRetries).

add_batch_item_with_retries(Item, Failures, Max) ->
    {TypeName, Id, DbName, IndexEjson} = Item,
    case chef_index:add(TypeName, Id, DbName, IndexEjson, none) of
        ok ->
            ok;
        Error when Failures >= Max ->
            Error;
        Error ->
            Retries = Failures + 1,
            lager:warning("chef_index:add failed for ~s[~s]: ~p retrying (~p/~p)", [TypeName, Id, Error, Retries, Max]),
            wait_before_retry(),
            add_batch_item_with_retries(Item, Retries, Max)
    end.

wait_before_retry() ->
    Min = envy:get(chef_index, reindex_sleep_min_ms, 500, non_neg_integer),
    Max = envy:get(chef_index, reindex_sleep_max_ms, 2000, non_neg_integer),
    wait_before_retry(Min, Max).

wait_before_retry(0, 0) ->
    ok;
wait_before_retry(Min, Min) ->
    lager:info("chef_index: waiting ~B ms before retry", [Min]),
    timer:sleep(Min);
wait_before_retry(Min, Max) when Min > Max ->
    lager:error("chef_index: reindex_sleep_max_ms less than reindex_sleep_min_ms. Sleeping ~B", [Max]),
    timer:sleep(Max);
wait_before_retry(Min, Max) ->
    RandMinMax = Min + random:uniform(Max - Min),
    lager:info("chef_index: waiting ~B ms before retry", [RandMinMax]),
    timer:sleep(RandMinMax).

not_ok(Results) ->
    NotOk = fun({_, Res}) -> Res =/= ok end,
    lists:filter(NotOk, Results).

delete(TypeName, Id, DbName, ReqId) ->
    case queue_mode() of
        rabbitmq ->
            ok = chef_index_queue:delete(envy:get(chef_index, rabbitmq_vhost, binary), TypeName, Id, DbName);
        _ -> %% batch mode not implemented for delete, always use inline if not rabbitmq
            stats_hero:ctime(ReqId, {chef_solr, delete},
                             fun() ->
                                     Doc = chef_index_expand:doc_for_delete(TypeName, Id, DbName),
                                     chef_index_expand:send_delete(Doc)
                             end)
    end.

queue_mode() ->
    envy:get(chef_index, search_queue_mode, rabbitmq, envy:one_of([rabbitmq, batch, inline])).

send_to_solr(QueueMode, Doc, none) ->
    send_to_solr(QueueMode, Doc);
send_to_solr(QueueMode, Doc, ReqId) ->
    stats_hero:ctime(ReqId, {chef_solr, update}, fun() ->
                                                         send_to_solr(QueueMode, Doc)
                                                 end).
send_to_solr(batch, Doc) ->
    chef_index_batch:add_item(Doc);
send_to_solr(inline, Doc) ->
    chef_index_expand:send_item(Doc).

ping() ->
    case queue_mode() of
        rabbitmq ->
            Config = envy:get(chef_index, rabbitmq_index_management_service, [], any),
            Enabled = proplists:get_value(enabled, Config),
            case Enabled of
                true ->
                    chef_index_queue:ping(envy:get(chef_index, rabbitmq_vhost, binary));
                _ ->
                    pong
            end;
        _ ->
            pong
    end.
