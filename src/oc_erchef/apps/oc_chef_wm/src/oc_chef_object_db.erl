%% -*- erlang-indent-level: 4;indent-tabs-mode: nil; fill-column: 92-*-
%% ex: ts=4 sw=4 et
%% @author Christopher Maier <cm@chef.io>
%% @author Seth Falcon <seth@chef.io>
%% @copyright 2012-2014 Chef Software, Inc.

%% @doc Helper functions that tie together operations across chef_db and chef_index
-module(oc_chef_object_db).

-export([safe_delete/3,
         delete/3,
         add_to_solr/3,
         add_batch_to_solr/1,
         bulk_delete_from_solr/4,
         delete_from_solr/2]).

-include("chef_types.hrl").
-include("oc_chef_types.hrl").

-record(context, {server_api_version,
                  reqid :: binary(),
                  darklaunch = undefined}).

-type delete_type() ::chef_object() |
                    #oc_chef_container{} |
                    #oc_chef_group{} |
                    #chef_cookbook_version{} |
                    #oc_chef_org_user_association{} |
                    #oc_chef_org_user_invite{} |
                    #oc_chef_cookbook_artifact_version{} |
                    #oc_chef_organization{}.

% @doc safely deletes an object from database and solr if appropriate, but
% returns not_found if the object doesn't exist; and returns an error tuple
% if an error occurs, instead of throwing on failure.
safe_delete(_DbContext, #chef_data_bag{}, _RequestorId) ->
    % If you need to use safe_delete with a data bag,
    % ensure that you add add proper recursive/bulk delete support
    throw({error, unsupported});
safe_delete(DbContext, Object, RequestorId) ->
    try
        oc_chef_object_db:delete_from_solr(Object, DbContext#context.reqid),
        % Report back the actual result of not_found or ok:
        delete_from_db(DbContext, RequestorId, Object)
    catch
        _Mod:Error ->
            {error, Error}
    end.

%% @doc Deletes an object from the database, removes the object's authz record (if needed),
%% and queues a delete of the object's data in the search index (Solr). Throws an error if
%% the database delete operation fails. Crashing on db error and not performing the authz
%% and solr deletes is the right thing because: if the data is still in the db, then the
%% entire operation can be retried. The object will still be searchable and accessible; if
%% the data was actually deleted despite the error return, then the orphaned authz and solr
%% data are of little consequence, data returned from searches is pulled out of the db so it
%% will be as if the data was correctly deleted. If we delted the authz and solr data when a
%% db error was encountered, we could have data in the db that could not be accessed nor be
%% findable via search.
-spec delete(chef_db:db_context(),
             delete_type(),
             object_id()) -> ok | not_found.
delete(DbContext, #chef_data_bag{org_id = OrgId,
                                 name = DataBagName}=DataBag, RequestorId) ->
    %% This is a special case, because of the hierarchical relationship between Data Bag
    %% Items and Data Bags.  We need to get the ids of all the data bag's items so that we
    %% can remove them from Solr as well; a cascade-on-delete foreign key takes care of the
    %% data bag item deletion from the database
    DataBagItemIds = case chef_db:fetch_data_bag_item_ids(DbContext, OrgId, DataBagName) of
                         Items when is_list(Items)->
                             Items;
                         _Other ->
                             []
                     end,

    %% Remove data bag from database; no need to remove from Solr, since they're not indexed
    %% anyway (what's there to index, after all?)
    delete_from_db(DbContext, RequestorId, DataBag), % throws on error
    %% Remove data bag items from the search index now; directly calling chef_index:delete
    %% since we've just got ids, and not proper data bag item records required for
    %% delete_from_solr
    bulk_delete_from_solr(data_bag_item, DataBagItemIds, OrgId, DbContext#context.reqid),
    ok;
delete(DbContext = #context{reqid= ReqId}, Object, RequestorId) ->
    %% All other object deletion is relatively sane :)
    %% Note that this will throw if an error is encountered
    delete_from_db(DbContext, RequestorId, Object),
    %% This is fire and forget as well. If we're here, we've already deleted the db record
    %% and won't be able to get back here for a retry.
    delete_from_solr(Object, ReqId),
    ok.

-spec delete_from_db(chef_db:db_context(),
                     RequestorId :: object_id(),
                     delete_type()) -> ok | not_found.
%% @doc Delete an object from the database.  Provides pattern-matching sugar over chef_db
%% delete functions, making the `delete` function in this module very simple. Also deletes
%% the corresponding authz object (if there is one and it should be deleted). That is, it
%% does nothing extra for data_bag_items, it deletes the authz id for the cookbook if the
%% last cookbook_version of a given name is being deleted, and otherwise deletes the authz
%% object as desired. If an error is returned by the `chef_db:delete_*` call, this function
%% throws a `{delete_from_db, {error, Why}}' tuple.
delete_from_db(DbContext, RequestorId, ObjectRec) ->
    maybe_delete_authz_id_or_error(chef_db:delete(ObjectRec, DbContext),
                                   ObjectRec, RequestorId).

-spec maybe_delete_authz_id_or_error(Status, Object, RequestorId) -> ok | not_found when
      Status :: {ok, 1 | 2} | not_found | {error, _},
      Object :: delete_type(),
      RequestorId :: object_id().
%% Some Chef objects do not have an authz id (data_bag_items) and some (cookbook_versions)
%% share an authz id (cookbook). This code determins when the parent authz id should be
%% deleted. For data_bag_items, never as data_bags are deleted directly by the API. For
%% cookbook_version objects, we delete the associated cookbook authz id when the the
%% cookbook_version is the last of the named cookbook.
maybe_delete_authz_id_or_error(Error = {error, _}, _Object, _RequestorId) ->
    %% db error, no further action, but throw!
    throw({delete_from_db, Error});
maybe_delete_authz_id_or_error(not_found, _Object, _RequestorId) ->
    %% if the object wasn't found, we take no further action
    not_found;
maybe_delete_authz_id_or_error(1, Object, _RequestorId)  when is_record(Object, chef_data_bag_item);
                                                              is_record(Object, oc_chef_org_user_association);
                                                              is_record(Object, oc_chef_org_user_invite) ->
    %% These objects have no authz id
    ok;
maybe_delete_authz_id_or_error({ok, 1}, #oc_chef_cookbook_artifact_version{authz_id = AuthzId}, RequestorId) ->
    case oc_chef_cookbook_artifact:exists_by_authz_id(AuthzId) of
        true -> ok;
        false -> oc_chef_authz:delete_resource(RequestorId, object, AuthzId);
        {error, _Why} = Error -> Error
    end;
maybe_delete_authz_id_or_error({ok, 1}, #chef_cookbook_version{}, _RequestorId) ->
    %% With status {ok, 1} we've deleted _only_ the cbv, not the cb so we leave the authz_id
    ok;
maybe_delete_authz_id_or_error({ok, 2}, #chef_cookbook_version{} = CBV, RequestorId) ->
    %% With status {ok, 2} we've deleted the cbv _and_ the cb so we delete the authz_id
    oc_chef_authz:delete_resource(RequestorId, object, chef_object:authz_id(CBV)),
    ok;
maybe_delete_authz_id_or_error(1, #chef_client{} = Object, RequestorId) ->
    oc_chef_authz:delete_resource(RequestorId, actor, chef_object:authz_id(Object)),
    ok;
maybe_delete_authz_id_or_error(1, #oc_chef_group{}, _RequestorId) ->
    % oc_chef_group cleans up after itself on delete. Don't attempt to delete
    ok;
maybe_delete_authz_id_or_error(1, Object, RequestorId) ->
    %% for all other object types, successful delete of 1 record means we should delete authz id
    oc_chef_authz:delete_resource(RequestorId, object, chef_object:authz_id(Object)),
    ok.

-spec add_to_solr(tuple(), ejson_term() | {ejson_term(), _}, binary()) -> ok.
add_to_solr(ObjectRec, ObjectEjson, ReqId) ->
    case chef_object:is_indexed(ObjectRec) of
        true ->
            {TypeName, Id, DbName, IndexEjson} = object_rec_to_index_args(ObjectRec, ObjectEjson),
            chef_index:add(TypeName, Id, DbName, IndexEjson, ReqId);
        false ->
            ok
    end.

-spec add_batch_to_solr([{tuple(), ejson_term()} | {ejson_term(), _}]) -> ok | {error, list()}.
add_batch_to_solr(Batch) ->
    BatchForIndex = [ object_rec_to_index_args(ObjectRec, ObjectEjson)
                      || {ObjectRec, ObjectEjson} <- Batch, chef_object:is_indexed(ObjectRec) =:= true ],
    chef_index:add_batch(BatchForIndex).

object_rec_to_index_args(ObjectRec, ObjectEjson) ->
    IndexEjson = chef_object:ejson_for_indexing(ObjectRec, ObjectEjson),
    Id = chef_object:id(ObjectRec),
    DbName = dbname(chef_object:org_id(ObjectRec)),
    TypeName = chef_object:type_name(ObjectRec),
    {TypeName, Id, DbName, IndexEjson}.

%% @doc Helper function to easily delete an object from the search index
-spec delete_from_solr(tuple(), binary()) -> ok.
delete_from_solr(ObjectRec, ReqId) ->
    case chef_object:is_indexed(ObjectRec) of
        true ->
            Id = chef_object:id(ObjectRec),
            DbName = dbname(chef_object:org_id(ObjectRec)),
            TypeName = chef_object:type_name(ObjectRec),
            chef_index:delete(TypeName, Id, DbName, ReqId);
        false ->
            ok
    end.

%% @doc Given an object type and a list of ids, delete the corresponding search index data.
-spec bulk_delete_from_solr(atom(), [binary()], binary(), binary()) -> ok.
bulk_delete_from_solr(Type, Ids, OrgId, ReqId) ->
    [ chef_index:delete(Type, Id, OrgId, ReqId) || Id <- Ids ],
    ok.

-spec dbname(binary()) -> <<_:40,_:_*8>>.
dbname(OrgId) ->
    <<"chef_", OrgId/binary>>.
