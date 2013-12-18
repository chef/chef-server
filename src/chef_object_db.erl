%% -*- erlang-indent-level: 4;indent-tabs-mode: nil; fill-column: 92-*-
%% ex: ts=4 sw=4 et
%% @author Christopher Maier <cm@opscode.com>
%% @author Seth Falcon <seth@opscode.com>
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


%% @doc Helper functions that tie together operations across chef_db and chef_index
-module(chef_object_db).

-export([
         add_to_solr/3,
         bulk_delete_from_solr/4,
         delete/4,
         delete_from_solr/2]).

-include_lib("chef_objects/include/chef_types.hrl").

-type darklaunch() :: any().

-spec add_to_solr(tuple(), ejson_term() | {ejson_term(), _}, darklaunch()) -> ok.
add_to_solr(ObjectRec, ObjectEjson, Darklaunch) ->
    case chef_object:is_indexed(ObjectRec) of
        true ->
            IndexEjson = chef_object:ejson_for_indexing(ObjectRec, ObjectEjson),
            DbName = chef_otto:dbname(chef_object:org_id(ObjectRec)),
            Id = chef_object:id(ObjectRec),
            TypeName = chef_object:type_name(ObjectRec),
            index_queue_add(TypeName, Id, DbName, IndexEjson, Darklaunch);
        false ->
            ok
    end.

%% @doc Helper function to easily delete an object from Solr, instead
%% of calling chef_index_queue directly.
-spec delete_from_solr(tuple(), darklaunch()) -> ok.
delete_from_solr(ObjectRec, Darklaunch) ->
    case chef_object:is_indexed(ObjectRec) of
        true ->
            Id = chef_object:id(ObjectRec),
            DbName = chef_otto:dbname(chef_object:org_id(ObjectRec)),
            TypeName = chef_object:type_name(ObjectRec),
            index_queue_delete(TypeName, Id, DbName, Darklaunch);
        false ->
            ok
    end.

%% @doc Deletes an object from the database and queues a delete of the object's data in the
%% search index (Solr). Throws an error if the database delete operation fails. Crashing on
%% db error and not performing the solr deletes is the right thing because: if the data is
%% still in the db, then the entire operation can be retried. The object will still be
%% searchable and accessible; if the data was actually deleted despite the error return,
%% then the orphaned solr data are of little consequence, data returned from searches is
%% pulled out of the db so it will be as if the data was correctly deleted. If we deleted
%% the solr data when a db error was encountered, we could have data in the db that could
%% not be findable via search.
-spec delete(chef_db:db_context(), tuple(), object_id(), darklaunch()) -> ok.
delete(DbContext, #chef_data_bag{org_id = OrgId,
                                 name = DataBagName}=DataBag,
       _RequestorId, Darklaunch) ->
    %% This is a special case, because of the hierarchical relationship between Data Bag
    %% Items and Data Bags.  We need to get the ids of all the data bag's items so that we
    %% can remove them from Solr as well; a cascade-on-delete foreign key takes care of the
    %% data bag item deletion from the database
    DataBagItemIds = case chef_db:fetch_data_bag_item_ids(DbContext, {id, OrgId}, DataBagName) of
                         Items when is_list(Items)->
                             Items;
                         _Other ->
                             []
                     end,

    %% Remove data bag from database; no need to remove from Solr, since they're not indexed
    %% anyway (what's there to index, after all?)
    delete_from_db(DbContext, DataBag, Darklaunch), % throws on error
    %% Remove data bag items from Solr now; directly calling chef_index_queue:delete since
    %% we've just got ids, and not proper data bag item records required for
    %% chef_object_db:delete_from_solr
    bulk_delete_from_solr(data_bag_item, DataBagItemIds, OrgId, Darklaunch),
    ok;
delete(DbContext, ObjectRec, _RequestorId, Darklaunch) ->
    %% All other object deletion is relatively sane :)
    %% Note that this will throw if an error is encountered
    delete_from_db(DbContext, ObjectRec, Darklaunch),
    %% This is fire and forget as well. If we're here, we've already deleted the db record
    %% and won't be able to get back here for a retry.
    delete_from_solr(ObjectRec, Darklaunch),
    ok.

%% @doc Given an object type and a list of ids, delete the corresponding search index data
%% from solr. The delete is achieved by putting a delete message on the indexing queue
%% (rabbitmq). The delete happens asynchronously (best effort) and this function returns
%% immediately.
-spec bulk_delete_from_solr(atom(), [binary()], binary(), any()) -> ok.
bulk_delete_from_solr(Type, Ids, OrgId, Darklaunch) ->
    [ index_queue_delete(Type, Id, OrgId, Darklaunch) || Id <- Ids ],
    ok.

-spec delete_from_db(chef_db:db_context(), tuple(), darklaunch()) -> ok.
%% @doc Delete an object from the database.  Provides pattern-matching sugar over chef_db
%% delete functions, making the `delete` function in this module very simple. Throws if the
%% database call returns an error, otherwise returns `ok' ignoring specific return value
%% from the chef_db module.
delete_from_db(DbContext, ObjectRec, _Darklaunch) ->
    handle_delete_from_db(chef_db:delete(ObjectRec, DbContext)).

handle_delete_from_db({error, _}=Error) ->
    throw({delete_from_db, Error});
handle_delete_from_db(_Result) ->
    ok.

index_queue_add(TypeName, Id, DbName, IndexEjson, Darklaunch) ->
    {DefaultVHost, AuxVHost} = default_and_aux(),
    default_index_queue_add(DefaultVHost, TypeName, Id, DbName, IndexEjson, Darklaunch),
    aux_index_queue_add(AuxVHost, TypeName, Id, DbName, IndexEjson, Darklaunch),
    ok.

default_index_queue_add(VHost, TypeName, Id, DbName, IndexEjson, Darklaunch) ->
    ok = chef_index_queue:set(VHost, TypeName, Id, DbName, IndexEjson),
    ok.

aux_index_queue_add(VHost, TypeName, Id, DbName, IndexEjson, Darklaunch) ->
    case chef_wm_darklaunch:is_enabled(<<"rabbit_aux_vhost">>, Darklaunch) of
        true ->
            ok = chef_index_queue:set(VHost, TypeName, Id, DbName, IndexEjson);
        false ->
            ok
    end,
    ok.

index_queue_delete(TypeName, Id, DbName, Darklaunch) ->
    {DefaultVHost, AuxVHost} = default_and_aux(),
    default_index_queue_delete(DefaultVHost, TypeName, Id, DbName, Darklaunch),
    aux_index_queue_delete(AuxVHost, TypeName, Id, DbName, Darklaunch),
    ok.

default_index_queue_delete(VHost, TypeName, Id, DbName, Darklaunch) ->
    ok = chef_index_queue:delete(VHost, TypeName, Id, DbName),
    ok.

aux_index_queue_delete(VHost, TypeName, Id, DbName, Darklaunch) ->
    case chef_wm_darklaunch:is_enabled(<<"rabbit_aux_vhost">>, Darklaunch) of
        true ->
            ok = chef_index_queue:delete(VHost, TypeName, Id, DbName);
        false ->
            ok
    end,
    ok.

default_and_aux() ->
    %% rabbitmq_vhosts is a proplist with keys 'default' and 'aux' mapping to the
    %% appropriate rabbitmq vhosts.
    VHosts = envy:get(chef_wm, rabbitmq_vhosts, [{default, envy:get(chef_index, rabbitmq_vhost, binary)}], list),
    DefaultVHost = proplists:get_value(default, VHosts),
    AuxVHost = proplists:get_value(aux, VHosts),
    {DefaultVHost, AuxVHost}.

