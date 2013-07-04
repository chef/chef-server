%% -*- erlang-indent-level: 4;indent-tabs-mode: nil; fill-column: 92-*-
%% ex: ts=4 sw=4 et
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

-module(chef_reindex).
-include_lib("chef_objects/include/chef_types.hrl").
-include_lib("ej/include/ej.hrl").

%% The only place we need an org name in all this code is in the
%% chef_db:bulk_get/4 call, but that argument is completely ignored
%% when accessing data in a SQL database.  Since this code will only
%% ever be used in an all-SQL environment, we can define a dummy value
%% here and avoid carrying around the real org name.  When chef_db no
%% longer needs to interact with CouchDB, bulk_get/4 will become
%% bulk_get/3, and we can dispense with this dummy placeholder
%% entirely.
-define(ORG_NAME, <<"does_not_matter">>).

%% A binary() index is taken to be a data bag name.
-type index() :: client | environment | node | role | binary().

-export([
         reindex/2
        ]).

%% @doc Sends all indexed items (clients, data bag items,
%% environments, nodes, and roles) from an organization to Solr for
%% reindexing.  Does not drop existing index entries beforehand; this
%% is explicitly a separate step.
-spec reindex(Ctx :: chef_db:db_context(),
              OrgId :: object_id()) -> ok.
reindex(Ctx, OrgId) ->
    BuiltInIndexes = [node, role, environment, client],
    DataBags = chef_db:data_bag_names(Ctx, OrgId),
    AllIndexes = BuiltInIndexes ++ DataBags,
    [ reindex(Ctx, OrgId, Index) || Index <- AllIndexes ],
    ok.

%% A note about our approach to reindexing:
%%
%% In order to make maximum reuse of existing code, we need to perform
%% a few gymnastics in order to get reindexing working properly.  On a
%% high level, reindexing involves iterating through every indexable
%% object in the database and placing each one on the indexing queue.
%%
%% The object placed on the queue is in most cases identical to the
%% JSON that is submitted to the Chef API (and stored in the
%% 'serialized_object' column in the appropriate database table).
%% Some object types (namely nodes and data bag items) must be
%% processed further, however, so we must take that into account.
%%
%% Additionally, Solr currently only stores the database ID of an
%% object; search results actually return just the IDs of objects that
%% match the search; this information is then used to retrieve the
%% actual data from the database to return to the user.  This ID is
%% thus required when putting an object on the indexing queue.
%% Unfortunately, current code for "bulk retrieval" of the
%% 'serialized_object' column from relevant tables in the database
%% does not return this database ID.  Until Sqerl is able to perform
%% ad-hoc SQL queries (a feature that should arrive soon!), it is
%% impractical to add additional "bulk" queries that return, say, the
%% serialized_object AND id column, or perhaps an entire record
%% representation.
%%
%% Thus, the current approach is to perform one additional database
%% query that fetches both the database ID and the unique name of an
%% item; the latter is available in the serialized_object data (once
%% it is decompressed and decoded into an EJson data structure).  By
%% processing this information into a dict mapping the name to the ID,
%% and carrying this dict through the reindexing processing, we can
%% quickly find the information we need.
%%
%% Thus, this name-to-id dict can be thought of as supplemental
%% information that enables the correct reconstruction of index data
%% for a given Chef object.  This data is NOT required for the
%% reindexing process per se, but is strictly a feature of the current
%% implementation, whose priority is the maximum reuse of existing
%% code.  Once Sqerl can perform ad-hoc queries (which will enable us
%% to get away from our current "bulk_get" approach more generally), a
%% more focused implementation may be substituted.

%% @doc Gathers supplemental information required for proper index
%% data reconstruction and begins the batch reindexing of all items of
%% the given indexable type.
-spec reindex(DbContext :: chef_db:db_context(),
              OrgId :: object_id(),
              Index :: index()) -> ok.
reindex(Ctx, OrgId, Index) ->
    NameIdDict = chef_db:create_name_id_dict(Ctx, Index, OrgId),
    %% Grab all the database IDs to do batch retrieval on
    AllIds = dict:fold(fun(_K, V, Acc) -> [V|Acc] end,
                       [],
                       NameIdDict), %% All dict values will be unique anyway
    {ok, BatchSize} = application:get_env(chef_wm, bulk_fetch_batch_size),
    batch_reindex(Ctx, AllIds, BatchSize, OrgId, Index, NameIdDict).

%% @doc Recursively batch-process a list of database IDs by retrieving
%% the serialized_object data from the database, processing them as
%% appropriate, and placing them on the indexing queue.
-spec batch_reindex(chef_db:db_context(),
                    Ids :: [object_id()] |                   %% Initial entry
                           {[object_id()], [object_id()]},   %% Recursive calls; current batch & remaining IDs
                    BatchSize :: non_neg_integer(),
                    OrgId :: object_id(),
                    Index :: index(),
                    NameIdDict :: dict()) -> ok.
batch_reindex(_, [], _, _, _, _) ->
    %% Nothing to process!
    ok;
batch_reindex(Ctx, Ids, BatchSize, OrgId, Index, NameIdDict) when is_list(Ids) ->
    batch_reindex(Ctx, safe_split(BatchSize, Ids), BatchSize, OrgId, Index, NameIdDict);
batch_reindex(Ctx, {LastBatch, []}, _BatchSize, OrgId, Index, NameIdDict) ->
    ok = index_a_batch(Ctx, LastBatch, OrgId, Index, NameIdDict),
    ok;
batch_reindex(Ctx, {CurrentBatch, Rest}, BatchSize, OrgId, Index, NameIdDict) ->
    ok = index_a_batch(Ctx, CurrentBatch, OrgId, Index, NameIdDict),
    batch_reindex(Ctx, safe_split(BatchSize, Rest), BatchSize, OrgId, Index, NameIdDict).

%% @doc Helper function to retrieve and index objects for a single
%% batch of database IDs.
-spec index_a_batch(chef_db:db_context(),
                    BatchOfIds :: [object_id()],
                    OrgId :: object_id(),
                    Index :: index(),
                    NameIdDict :: dict()) -> ok.
index_a_batch(Ctx, BatchOfIds, OrgId, Index, NameIdDict) ->
    SerializedObjects = chef_db:bulk_get(Ctx, ?ORG_NAME, chef_object_type(Index), BatchOfIds),
    ok = send_to_index_queue(OrgId, Index, SerializedObjects, NameIdDict),
    ok.

%% @doc Resolve an index to the name of the corresponding Chef object type.
chef_object_type(Index) when is_binary(Index) -> data_bag_item;
chef_object_type(Index)                       -> Index.

%% @doc Converts a list of serialized_objects into proper index data,
%% taking into account the kind of objects they are, and places each
%% item on the index queue.
-spec send_to_index_queue(OrgId :: object_id(),
                          Index :: index(),
                          SerializedObjects :: [binary()] | [ej:json_object()],
                          NameIdDict :: dict()) -> ok.
send_to_index_queue(_, _, [], _) ->
    ok;
send_to_index_queue(OrgId, Index, [SO|Rest], NameIdDict) ->
    %% All object types are returned from chef_db:bulk_get/4 as
    %% binaries (compressed or not) EXCEPT for clients, which are
    %% returned as EJson directly, because none of their
    %% information is actually stored as a JSON "blob" in the
    %% database.
    PreliminaryEJson = case is_binary(SO) of
                           true ->
                               chef_db_compression:decompress_and_decode(SO);
                           false ->
                                    SO
                       end,
    NameKey = name_key(chef_object_type(Index)),
    ItemName = ej:get({NameKey}, PreliminaryEJson),
    {ok, ObjectId} = dict:find(ItemName, NameIdDict),
    EJson = ejson_for_indexing(Index, PreliminaryEJson),
    ok = chef_object_db:add_to_solr(chef_object_type(Index), ObjectId, OrgId, EJson),
    send_to_index_queue(OrgId, Index, Rest, NameIdDict).

%% @doc Determine the proper key to use to retrieve the unique name of
%% an object from its EJson representation.
name_key(data_bag_item) -> <<"id">>;
name_key(_Type)         -> <<"name">>.

%% @doc For a given EJSON object, generate the appropriate index data.
-spec ejson_for_indexing(Index :: index(), EJson :: ej:json_object()) -> ej:json_object().
ejson_for_indexing(role, EJson) ->
    chef_object:ejson_for_indexing(#chef_role{}, EJson);
ejson_for_indexing(client, EJson) ->
    chef_object:ejson_for_indexing(#chef_client{}, EJson);
ejson_for_indexing(environment, EJson) ->
    chef_object:ejson_for_indexing(#chef_environment{}, EJson);
ejson_for_indexing(node, EJson) ->
    Name = ej:get({<<"name">>}, EJson),
    Environment = ej:get({<<"chef_environment">>}, EJson),
    chef_object:ejson_for_indexing(#chef_node{name=Name, environment=Environment}, EJson);
ejson_for_indexing(DataBagName, EJson) when is_binary(DataBagName) ->
    ItemName = ej:get({<<"id">>}, EJson),
    chef_object:ejson_for_indexing(#chef_data_bag_item{data_bag_name=DataBagName, item_name=ItemName}, EJson).

% TODO: Create a chef_common module for this stuff
safe_split(N, L) ->
    try
        lists:split(N, L)
    catch
        error:badarg ->
            {L, []}
    end.
