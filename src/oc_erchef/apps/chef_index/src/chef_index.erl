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
         add_async/4,
         add_async/5,
         search_provider/0
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

%% The difference between add and add_async is that add_async
%% will not block on the data being posted to our search store
%% and will instantly return `ok` (regardless of queue mode);
%% whereas, add will block the request until the queue has been
%% flushed to our search store in batch queue mode or directly
%% posted in inline queue mode.
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

add_async(TypeName, Id, DbName, IndexEjson, _ReqId) ->
    %% No solr_time logging for add_async as this doesn't happen as
    %% part of a request, But we provided a add_async/5 to make it
    %% easier to call in oc_chef_object_db.
    add_async(TypeName, Id, DbName, IndexEjson).

add_async(TypeName, Id, DbName, IndexEjson) ->
    spawn(chef_index, add, [TypeName, Id, DbName, IndexEjson, none]),
    ok.

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
