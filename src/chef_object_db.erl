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

-export([add_to_solr/4,
         delete/3,
         delete_from_solr/1]).

-include_lib("chef_objects/include/chef_types.hrl").

-spec add_to_solr(chef_type(), object_id(), object_id(), ejson_term()) -> ok.
%% Add a Chef object to solr
add_to_solr(TypeName, _Id, _OrgId, _Ejson) when TypeName =:= data_bag;
                                              TypeName =:= user;
                                              TypeName =:= coookbook_version ->
            ok; %%These types are not indexed
add_to_solr(TypeName, Id, OrgId, Ejson) ->
    chef_index_queue:set(TypeName, Id, chef_otto:dbname(OrgId), Ejson).

%% @doc Helper function to easily delete an object from Solr, instead
%% of calling chef_index_queue directly.
-spec delete_from_solr(chef_indexable_object()
                        | #chef_cookbook_version{}
                        | #chef_user{}) -> ok.
delete_from_solr(Object) when is_record(Object, chef_cookbook_version);
                              is_record(Object, chef_data_bag_item);
                              is_record(Object, chef_user) ->
                 ok; %%These types are not indexed, so don't need to issue delete
delete_from_solr(Object) ->
    {Id, OrgId} = get_id_and_org_id(Object),
    chef_index_queue:delete(chef_object:type_name(Object), Id, chef_otto:dbname(OrgId)).

%% @doc Deletes an object from the database and queues a delete of the object's data in the
%% search index (Solr). Throws an error if the database delete operation fails. Crashing on
%% db error and not performing the solr deletes is the right thing because: if the data is
%% still in the db, then the entire operation can be retried. The object will still be
%% searchable and accessible; if the data was actually deleted despite the error return,
%% then the orphaned solr data are of little consequence, data returned from searches is
%% pulled out of the db so it will be as if the data was correctly deleted. If we deleted
%% the solr data when a db error was encountered, we could have data in the db that could
%% not be findable via search.
-spec delete(chef_db:db_context(),
             chef_object() |
             #chef_cookbook_version{checksums::'undefined' |
                                               [binary()]} |
             #chef_user{},
             object_id() ) -> ok.
delete(DbContext,#chef_data_bag{org_id = OrgId,
                                name = DataBagName}=DataBag,
                                _RequestorId) ->
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
    delete_from_db(DbContext, DataBag), % throws on error
    %% Remove data bag items from Solr now; directly calling chef_index_queue:delete since
    %% we've just got ids, and not proper data bag item records required for
    %% chef_object_db:delete_from_solr
    [ chef_index_queue:delete(data_bag_item, Id, OrgId) || Id <- DataBagItemIds ],
    ok;
delete(DbContext, Object, _RequestorId) ->
    %% All other object deletion is relatively sane :)
    %% Note that this will throw if an error is encountered
    delete_from_db(DbContext, Object),
    %% This is fire and forget as well. If we're here, we've already deleted the db record
    %% and won't be able to get back here for a retry.
    delete_from_solr(Object),
    ok.

-spec delete_from_db(chef_db:db_context(),
                     chef_object() | #chef_user{} | #chef_cookbook_version{}) -> ok.
%% @doc Delete an object from the database.  Provides pattern-matching sugar over chef_db
%% delete functions, making the `delete` function in this module very simple. Throws if the
%% database call returns an error, otherwise returns `ok' ignoring specific return value
%% from the chef_db module.
delete_from_db(DbContext, #chef_client{}=Client) ->
    handle_delete_from_db(chef_db:delete_client(DbContext, Client));
delete_from_db(DbContext, #chef_node{}=Node) ->
    handle_delete_from_db(chef_db:delete_node(DbContext, Node));
delete_from_db(DbContext, #chef_user{}=User) ->
    handle_delete_from_db(chef_db:delete_user(DbContext, User));
delete_from_db(DbContext, #chef_role{}=Role) ->
    handle_delete_from_db(chef_db:delete_role(DbContext, Role));
delete_from_db(DbContext, #chef_environment{}=Environment) ->
    handle_delete_from_db(chef_db:delete_environment(DbContext, Environment));
delete_from_db(DbContext, #chef_data_bag{}=DataBag) ->
    handle_delete_from_db(chef_db:delete_data_bag(DbContext, DataBag));
delete_from_db(DbContext, #chef_data_bag_item{}=DataBagItem) ->
    handle_delete_from_db(chef_db:delete_data_bag_item(DbContext, DataBagItem));
delete_from_db(DbContext, #chef_cookbook_version{}=CBVersion) ->
    handle_delete_from_db(chef_db:delete_cookbook_version(DbContext, CBVersion)).

handle_delete_from_db({error, _}=Error) ->
    throw({delete_from_db, Error});
handle_delete_from_db(_Result) ->
    ok.

-spec get_id_and_org_id(chef_object()) -> {binary(), binary()}.
%% @doc Return the `id' and `org_id' fields from a `chef_object()' record type as a tuple of
%% `{Id, OrgId}'.
get_id_and_org_id(#chef_node{id = Id, org_id = OrgId}) ->
    {Id, OrgId};
get_id_and_org_id(#chef_role{id = Id, org_id = OrgId}) ->
    {Id, OrgId};
get_id_and_org_id(#chef_environment{id = Id, org_id = OrgId}) ->
    {Id, OrgId};
get_id_and_org_id(#chef_client{id = Id, org_id = OrgId}) ->
    {Id, OrgId};
get_id_and_org_id(#chef_data_bag{id = Id, org_id = OrgId}) ->
    {Id, OrgId};
get_id_and_org_id(#chef_data_bag_item{id = Id, org_id = OrgId}) ->
    {Id, OrgId};
get_id_and_org_id(#chef_cookbook_version{id = Id, org_id = OrgId}) ->
    {Id, OrgId}.

