%% -*- erlang-indent-level: 4;indent-tabs-mode: nil; fill-column: 92 -*-
%% ex: ts=4 sw=4 et
%% @author Seth Falcon <seth@chef.io>
%% @author Mark Anderson <mark@chef.io>
%% @author Christopher Maier <cm@chef.io>
%% @copyright 2011-2012 Opscode Inc.
%% @end
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

-module(chef_otto).

-export([
         fetch_org/2,
         fetch_org_metadata/2,
         fetch_orgs/1,
         fetch_assigned_orgs/1,
         %% chef object fetching
         fetch_client/3,
         %% fetch_cookbook/3,
         fetch_data_bag/3,
         %% fetch_data_bag_item/3,
         fetch_environment/3,
         fetch_node/3,
         fetch_role/3,
         %% chef object listing
         fetch_nodes/2,
         fetch_nodes/3,
         fetch_nodes_with_ids/2,
         fetch_roles/2,
         fetch_roles_with_ids/2,

         fetch_data_bags/2,
         fetch_data_bags_with_ids/2,

         %% fetch_data_bag_items/2,
         %% fetch_data_bag_items_with_ids/2,

         %% fetch_cookbooks/2,

         fetch_by_name/4,
%         fetch_auth_join/2,
         fetch_auth_join_id/3,
         fetch_orgs_for_user_id/2,
         is_user_in_org/3,
         connect/0,
         connect/2,
         bulk_get/3,
         data_bag_exists/3,
         data_bag_names/2,
         environment_exists/3,
         dbname/1,
         convert_couch_json_to_node_record/4,
         couch_json_to_record/5,
         ping/0
         ]).

-include_lib("eunit/include/eunit.hrl").
-include_lib("couchbeam/include/couchbeam.hrl").
-include("chef_types.hrl").
-include("chef_otto.hrl").

-type couch_server() :: server().               % from couchbeam.hrl

-type authz_type() :: 'authz_client' |
                      'authz_container' |
                      'authz_cookbook' |
                      'authz_data_bag' |
                      'authz_environment' |
                      'authz_group' |
                      'authz_node' |
                      'authz_role'.

-type http_port() :: non_neg_integer().
-type db_key() :: binary() | string().

-define(gv(Key, PList), proplists:get_value(Key, PList)).

-spec connect() -> couch_server().
connect() ->
    Host = envy:get(chef_db, couchdb_host, string),
    Port = envy:get(chef_db, couchdb_port, non_neg_integer),
    connect(Host, Port).

-spec connect(string(), http_port()) -> couch_server().
connect(Host, Port) ->
    couchbeam:server_connection(Host, Port, "", []).

ping() ->
    try
        Server = connect(),
        case couchbeam:server_info(Server) of
            {ok, _} -> pong;
            _Else -> pang
        end
    catch
        How:Why ->
            error_logger:error_report({chef_otto, ping, How, Why}),
            pang
    end.

-spec is_user_in_org(couch_server(), object_id(), binary()) -> boolean().
%% @doc Return true if `User' is in `OrgName' and false otherwise.
is_user_in_org(Server, UserId, OrgName) when is_binary(OrgName) ->
    lists:member(OrgName, fetch_orgs_for_user_id(Server, UserId)).

-spec fetch_orgs_for_user_id(couch_server(), object_id()) -> [binary()].
%% @doc Return the list of organization names that username `User' is associated with
%%
fetch_orgs_for_user_id(Server, UserId) ->
    {ok, Db} = couchbeam:open_db(Server, ?user_db, []),
    {ok, View} = couchbeam:view(Db, {?organization_user_design,
                                     "by_organizations_for_user"},
                                [{key, UserId}, {include_docs, true}]),
    %% FIXME: are these the org guids? If so, we could alter the interface to check be via
    %% org_id and save a call.
    {ok, Res} = couchbeam_view:fetch(View),
    Rows = ej:get({<<"rows">>}, Res),
    GetOrgDocId = fun(Row) ->
                          OrgDocId = ej:get({<<"doc">>, <<"organization">>}, Row),
                          %% ensure binary
                          <<_/binary>> = OrgDocId,
                          OrgDocId
                  end,
    OrgAccountIds = [ GetOrgDocId(Row) || Row <- Rows ],
    Orgs = bulk_get(Server, ?user_db, OrgAccountIds),
    [ ej:get({<<"name">>}, Org) || Org <- Orgs ].

-spec fetch_orgs(couch_server()) -> [{binary(), binary()}].
%% @doc Return a list of {OrgName, OrgId} tuples for all orgs in the opscode-account
%% database.
%%
fetch_orgs(Server) ->
    {ok, Db} = couchbeam:open_db(Server, ?user_db, []),
    {ok, View} = couchbeam:view(Db, {?mixlib_auth_org_design, "by_name"},
                                [{include_docs, true}]),
    couchbeam_view:fold(View,
                        fun(Row, Acc) ->
                                OrgId = ej:get({<<"doc">>, <<"guid">>}, Row),
                                OrgName = ej:get({<<"key">>}, Row),
                                [{OrgName, OrgId}|Acc]
                        end).

-spec fetch_assigned_orgs(couch_server()) -> [{binary(), binary()}].

%% @doc Return a list of {OrgName, OrgId} tuples for all assigned orgs in the
%% opscode-account database.
%%
%% If an has full_name of "Pre-created" and name consisting of 20 characters, then it is
%% unassigned.
%%
fetch_assigned_orgs(Server) ->
    {ok, Db} = couchbeam:open_db(Server, ?user_db, []),
    {ok, View} = couchbeam:view(Db, {?mixlib_auth_org_design, "by_name"},
                                [{include_docs, true}]),
    couchbeam_view:fold(View,
                        fun(Row, Acc) ->
                                case is_unassigned(Row) of
                                    true -> Acc;
                                    false ->
                                        OrgId = ej:get({<<"doc">>, <<"guid">>}, Row),
                                        OrgName = ej:get({<<"key">>}, Row),
                                        [{OrgName, OrgId}|Acc]
                                end
                        end).

is_unassigned(Row) ->
    FullName = ej:get({<<"doc">>, <<"full_name">>}, Row),
    Name = ej:get({<<"doc">>, <<"name">>}, Row),
    FullName =:= <<"Pre-created">> andalso byte_size(Name) =:= 20.

-spec fetch_org_metadata(couch_server(), binary()) -> {binary(), binary()} | not_found.
%% @doc Return the org GUID and authz_id for a given organization name.
fetch_org_metadata(Server, OrgName) when is_binary(OrgName) ->
    case fetch_org(Server, OrgName) of
        {org_not_found, _} -> not_found;
        Org when is_list(Org) ->
            Guid = ?gv(<<"guid">>, Org),
            DocId = ?gv(<<"_id">>, Org),
            AuthzId = fetch_auth_join_id(Server, DocId, user_to_auth),
            {Guid, AuthzId}
    end.

-spec fetch_org(couch_server(), binary()) ->
    [tuple()]
        | {org_not_found, not_in_view}
        | {org_not_found, {no_doc, binary()}}.
%% @doc Return the organization document for a given organization name.
fetch_org(Server, OrgName) when is_binary(OrgName) ->
    {ok, Db} = couchbeam:open_db(Server, ?user_db, []),
    {ok, View} = couchbeam:view(Db, {?mixlib_auth_org_design, "by_name"},
                                [{key, OrgName}]),
    case couchbeam_view:first(View) of
        {ok, {Row}} ->
            OrgDocId = ?gv(<<"id">>, Row),
            case couchbeam:open_doc(Db, OrgDocId) of
                {error, not_found} -> {org_not_found, {no_doc, OrgDocId}};
                {ok, {OrgDoc}} -> OrgDoc
            end;
        {ok, []} ->
            {org_not_found, not_in_view}
    end;
fetch_org(Server, OrgName) when is_list(OrgName) ->
    fetch_org(Server, list_to_binary(OrgName)).

-spec fetch_by_name(couch_server(),
                    binary() | 'not_found',
                    binary() | string(),
                    authz_type() | chef_object_name()) ->
                           {ok, [{binary(), _}]} | {'not_found', atom() | 'org'}.
%% @doc Fetch from the mixlib-authz records in couchdb
%%
fetch_by_name(_Server, not_found, _Name, _Type) ->
    {not_found, org};
fetch_by_name(Server, OrgId, Name, Type) when is_list(Name), is_binary(OrgId) ->
    fetch_by_name(Server, OrgId, list_to_binary(Name), Type);
fetch_by_name(Server, OrgId, Name, Type) when is_binary(Name), is_binary(OrgId) ->
    {Design, ViewName} = design_and_view_for_type(Type),
    ChefDb = dbname(OrgId),
    {ok, Db} = couchbeam:open_db(Server, ChefDb, []),
    {ok, View} = couchbeam:view(Db, {Design, ViewName}, [{key, Name}]),
    case couchbeam_view:first(View) of
        {ok, {Row}} ->
            Id = ?gv(<<"id">>, Row),
            case couchbeam:open_doc(Db, Id) of
                {error, not_found} -> {not_found, Type};
                %% FIXME: why are we unpacking the ejson format?
                {ok, {Doc}} -> {ok, Doc}
            end;
        {ok, []} -> {not_found, Type};
        {error, not_found} -> {not_found, Type}
    end.

%%%
%%% Clients are only mixlib-side entities?
%%%
-spec fetch_client(couch_server(), binary() | not_found, binary() | string()) ->
                          #chef_client{}
                              | {not_found, client}
                              | {not_found, org}.
fetch_client(Server, OrgId, ClientName) ->
    case fetch_by_name(Server, OrgId, ClientName, authz_type(chef_client)) of
        {ok, Client} ->
            Id = ej:get({<<"_id">>}, Client),
            Cert = ej:get({<<"certificate">>}, Client),
            AuthzId = fetch_auth_join_id(Server, Id, user_to_auth),
            Updated =  ej:get({<<"requester_id">>}, Client),
            #chef_client{id = Id,
                         authz_id = AuthzId,
                         org_id = OrgId,
                         name = ClientName,
                         public_key =  Cert,
                         last_updated_by = Updated
                        };
        Error -> Error
    end.

%% @doc Enforce clean node data, remove couch cruft, add any missing keys
%%
%% Couch records contain fields _id, _rev that we don't need, so remove them. Make sure that
%% the expected top-level keys are present. If a top-level key is not found, add it with an
%% appropriate default value.
%%
cleanup_couch_node_record(R) when is_list(R) ->
    cleanup_couch_node_record({R});
cleanup_couch_node_record({_}=R) ->
    N1 = ej:delete({<<"_rev">>}, ej:delete({<<"_id">>}, R)),
    FixedKeys = [{<<"chef_environment">>, <<"_default">>},
                 {<<"default">>, ?EMPTY_EJSON_HASH},
                 {<<"normal">>, ?EMPTY_EJSON_HASH},
                 {<<"override">>, ?EMPTY_EJSON_HASH},
                 {<<"automatic">>, ?EMPTY_EJSON_HASH},
                 {<<"run_list">>, []}],
    lists:foldl(fun({Key, Default}, Node) ->
                        add_if_not_found(Node, Key, Default)
                end, N1, FixedKeys).

cleanup_couch_role_record(R) when is_list(R) ->
    cleanup_couch_role_record({R});
cleanup_couch_role_record({_}=R) ->
    R1 = ej:delete({<<"_rev">>}, ej:delete({<<"_id">>}, R)),
    %% FIXME 1: what about chef_type and json_class?
    %% FIXME 2: refactor to DRY up role/node code
    FixedKeys = [{<<"description">>, <<"">>},
                 {<<"default_attributes">>, ?EMPTY_EJSON_HASH},
                 {<<"override_attributes">>, ?EMPTY_EJSON_HASH},
                 {<<"run_list">>, []},
                 {<<"env_run_list">>, ?EMPTY_EJSON_HASH}],
    lists:foldl(fun({Key, Default}, Node) ->
                        add_if_not_found(Node, Key, Default)
                end, R1, FixedKeys).

cleanup_couch_environment_record(R) when is_list(R) ->
    cleanup_couch_environment_record({R});
cleanup_couch_environment_record({_}=R) ->
    R1 = ej:delete({<<"_rev">>}, ej:delete({<<"_id">>}, R)),
    %% FIXME 1: what about chef_type and json_class?
    %% FIXME 2: refactor to DRY up environment/node code
    FixedKeys = [{<<"description">>, <<"">>},
                 {<<"default_attributes">>, ?EMPTY_EJSON_HASH},
                 {<<"override_attributes">>, ?EMPTY_EJSON_HASH},
                 {<<"cookbook_versions">>, ?EMPTY_EJSON_HASH}],
    lists:foldl(fun({Key, Default}, Node) ->
                        add_if_not_found(Node, Key, Default)
                end, R1, FixedKeys).

cleanup_couch_data_bag_item_record(R) when is_list(R) ->
    cleanup_couch_data_bag_item_record({R});
cleanup_couch_data_bag_item_record({_}=R) ->
    R1 = ej:delete({<<"_rev">>}, ej:delete({<<"_id">>}, R)),
    %% FIXME 1: what about chef_type and json_class?
    %% FIXME 2: refactor to DRY up data_bag_item/node code
    R1.


-spec add_if_not_found({maybe_improper_list()}, binary(), binary()) ->
                              {maybe_improper_list()}.
add_if_not_found(Node, Key, Default) ->
    case ej:get({Key}, Node) of
        undefined -> ej:set({Key}, Node, Default);
        _Value -> Node
    end.

convert_couch_json_to_node_record(OrgId, AuthzId, RequestorId, NodeBlob) ->
    CleanBlob = cleanup_couch_node_record(NodeBlob),
    Name = ej:get({<<"name">>}, CleanBlob),
    Date = sql_date(now),
    #chef_node{id = chef_object_base:make_org_prefix_id(OrgId, Name),
               authz_id = AuthzId,
               org_id = OrgId,
               name = Name,
               environment = ej:get({<<"chef_environment">>}, CleanBlob),
               last_updated_by = RequestorId,
               created_at = Date,
               updated_at = Date,
               serialized_object = serialize_node(CleanBlob)}.

%%%
%%% Nodes have a mixlib-auth record and a regular record
%%%
-spec fetch_node(couch_server(), OrgId :: binary(),
                 NodeName :: binary() | string()) ->
                        #chef_node{} |
                        {'error', {{'not_found', atom()} |
                                   {'ok', _}, {'not_found', atom()}}}.
fetch_node(Server, OrgId, Name) ->
    fetch_object(Server, OrgId, Name, chef_node).

fetch_role(Server, OrgId, Name) ->
    fetch_object(Server, OrgId, Name, chef_role).

fetch_data_bag(Server, OrgId, Name) ->
    fetch_object(Server, OrgId, Name, chef_data_bag).

%% FIXME: data_bag_item needs a separate code path with a modified fetch object and friends
%% because you need to specify the data_bag name and the item name. Only the data_bag has
%% authz data.
%%
%%  fetch_data_bag_item(Server, OrgId, Name) -> fetch_object(Server, OrgId, Name,
%% chef_data_bag_item).

%% FIXME: this doesn't do the right thing yet with authz I think the issue is that we have a
%% top-level cookbook object which is authz'd, e.g. 'apache2', and then an individual
%% cookbook is 'apache2-0.2.3'.  Seems like we need some new term to distinguish cookbook
%% top-level from versioned instance. Maybe cookbook_version since that mirrors the Ruby
%% code.
%% fetch_cookbook(Server, OrgId, Name) ->
%%     fetch_object(Server, OrgId, Name, cookbook).

fetch_environment(Server, OrgId, Name) ->
    fetch_object(Server, OrgId, Name, chef_environment).

%% listing

%% FIXME: not quite right. This returns a list of cookbook_versions, but not the right data
%% for the API yet.
%% -spec fetch_cookbooks(couch_server(), binary()) -> [binary()].
%% fetch_cookbooks(Server, OrgId) -> fetch_objects(Server, OrgId, cookbook).

-spec fetch_data_bags(couch_server(), binary()) -> [binary()].
fetch_data_bags(Server, OrgId) ->
    fetch_objects(Server, OrgId, chef_data_bag).

%% -spec fetch_data_bag_items(couch_server(), binary()) -> [binary()].
%% fetch_data_bag_items(Server, OrgId) ->
%%     fetch_objects(Server, OrgId, data_bag_items).

-spec fetch_nodes(couch_server(), binary()) -> [binary()].
fetch_nodes(Server, OrgId) ->
    fetch_objects(Server, OrgId, chef_node).

-spec fetch_roles(couch_server(), binary()) -> [binary()].
fetch_roles(Server, OrgId) ->
    fetch_objects(Server, OrgId, chef_role).

%% @doc Return a list of {Name, Id} tuples for all data_bags in the specified org.
%%
%% This is used for couchdb => mysql migration
%%
fetch_data_bags_with_ids(Server, OrgId) ->
    fetch_objects_with_ids(Server, OrgId, chef_data_bag).

%% %% @doc Return a list of {Name, Id} tuples for all data_bag_items in the specified org.
%% %%
%% %% This is used for couchdb => mysql migration
%% %%
%% fetch_data_bag_items_with_ids(Server, OrgId) ->
%%     fetch_objects_with_ids(Server, OrgId, chef_data_bag_item).

%% @doc Return a list of {Name, Id} tuples for all nodes in the specified org.
%%
%% This is used for couchdb => mysql migration
%%
fetch_nodes_with_ids(Server, OrgId) ->
    fetch_objects_with_ids(Server, OrgId, chef_node).

%% @doc Return a list of node IDs that exist in the environment given by `EnvName'.
fetch_nodes(Server, OrgId, EnvName) ->
    ChefDb = dbname(OrgId),
    {ok, Db} = couchbeam:open_db(Server, ChefDb, []),
    {ok, View} = couchbeam:view(Db, {?node_design, "by_environment"},
                                [{key, EnvName}, {include_docs, true}]),
    Nodes = couchbeam_view:fold(View,
                                fun(Row, Acc) ->
                                        [ej:get({<<"key">>}, Row) | Acc]
                                end),
    lists:sort(Nodes).

%% @doc Return a list of {Name, Id} tuples for all roles in the specified org.
%%
%% This is used for couchdb => mysql migration
%%
fetch_roles_with_ids(Server, OrgId) ->
    fetch_objects_with_ids(Server, OrgId, chef_role).

-spec bulk_get(couch_server(), string() | binary(), [binary()]) -> [tuple()].
bulk_get(Server, DbName, Ids) ->
    {ok, Db} = couchbeam:open_db(Server, DbName, []),
    {ok, View} = couchbeam:all_docs(Db, [{keys, Ids}, {include_docs, true}]),
    DocCollector = fun(Row, Acc) ->
                      [ ej:get({<<"doc">>}, Row) | Acc ]
                   end,
    %% There is a apparent bug in CouchDB where documents that have been deleted can still
    %% be returned as 'null'.  The rest of the code should not have to deal with this, so
    %% we'll filter out any non-tuple items in our results here.
    Results = couchbeam_view:fold(View, DocCollector),
    lists:reverse([Doc || Doc <- Results, is_tuple(Doc)]).

-spec fetch_auth_join_id(couch_server(), db_key(), auth_to_user|user_to_auth) -> id() | {not_found, term()}.
fetch_auth_join_id(Server, Id, Direction) when is_list(Id) ->
    fetch_auth_join_id(Server, list_to_binary(Id), Direction);
fetch_auth_join_id(Server, Id, Direction) when is_binary(Id) ->
    {FieldName, ViewName} =
        case Direction of
            auth_to_user -> { <<"user_object_id">>, "by_auth_object_id"};
            user_to_auth -> { <<"auth_object_id">>, "by_user_object_id"}
        end,
    {ok, Db} = couchbeam:open_db(Server, ?auth_join_db, []),
    {ok, View} = couchbeam:view(Db, {?mixlib_auth_join_design, ViewName},
                                [{key, Id}, {include_docs, true}]),
    case couchbeam_view:first(View) of
        {ok, []} -> {not_found, missing};
        {ok, Row} -> ej:get({<<"doc">>, FieldName}, Row);
        Why -> {not_found, Why}
    end.

-spec data_bag_exists(couch_server(), binary(), binary() | string()) -> boolean().
%% @doc Return true if there is a data bag named `DataBag' in the
%% specified org.
%%
%%
data_bag_exists(Server, OrgId, DataBag)
  when is_binary(DataBag), is_binary(OrgId) ->
    object_exists(Server, OrgId, DataBag, chef_data_bag);
data_bag_exists(Server, OrgId, DataBag) when is_list(DataBag), is_binary(OrgId) ->
    data_bag_exists(Server, OrgId, list_to_binary(DataBag)).

-spec environment_exists(couch_server(), binary(), binary() | string()) -> boolean().
%% @doc Return true if there is an environment named `EnvName' in the
%% specified org.
%%
environment_exists(Server, OrgId, EnvName) when is_binary(EnvName), is_binary(OrgId) ->
    object_exists(Server, OrgId, EnvName, chef_environment);
environment_exists(Server, OrgId, EnvName) when is_list(EnvName), is_binary(OrgId) ->
    environment_exists(Server, OrgId, list_to_binary(EnvName)).


%% @doc Return the names of all an organization's data bags
-spec data_bag_names(couch_server(), OrgId::binary()) -> list(binary()).
data_bag_names(Server, OrgId) when is_binary(OrgId) ->
    {ok, Db} = couchbeam:open_db(Server, dbname(OrgId), []),
    {ok, View} = couchbeam:view(Db, {"data_bags", "all_id"}, []),

    couchbeam_view:fold(View,
                        fun(Row, Acc) ->
                                Name = ej:get({<<"key">>}, Row),
                                [Name | Acc]
                        end).

-spec dbname(binary()) -> <<_:40,_:_*8>>.
dbname(OrgId) ->
    <<"chef_", OrgId/binary>>.

serialize_node(Node) when is_binary(Node) ->
    zlib:gzip(Node);
serialize_node({_}=Node) ->
    serialize_node(chef_json:encode(Node)).

-spec fetch_object(couch_server(), OrgId :: binary(),
                   Name :: binary() | string(),
                   Type :: chef_object_name()) ->
                          chef_object() | #chef_client{} |
                          {'error', {{'not_found', atom()} | {'ok', _}, {'not_found', atom()}}}.
fetch_object(Server, OrgId, Name, Type) ->
    MixlibObjectRes = fetch_by_name(Server, OrgId, Name, authz_type(Type)),
    ObjectRes = fetch_by_name(Server, OrgId, Name, Type),
    case {MixlibObjectRes, ObjectRes} of
        {{ok, MixlibObject}, {ok, ObjectBlob}} ->
            <<MixlibId/binary>> = ej:get({<<"_id">>}, MixlibObject),
            <<AuthzId/binary>> = fetch_auth_join_id(Server, MixlibId, user_to_auth),
            <<RequesterId/binary>> = ej:get({<<"requester_id">>}, MixlibObject),
            %% assert_chef_object(couch_json_to_record(Type, OrgId, AuthzId, RequesterId, ObjectBlob), Type);
            couch_json_to_record(Type, OrgId, AuthzId, RequesterId, ObjectBlob);
        _Error ->
            {error, {MixlibObjectRes, ObjectRes}}
    end.

%% chef_otto.erl:512: Invalid type specification for function chef_otto:fetch_objects/3. The
%% success typing is (_,binary(),'cookbook' | 'data_bag' | 'data_bag_items' | 'node' |
%% 'role') -> [any()]

-spec fetch_objects(couch_server(), binary(), 'chef_data_bag' | 'chef_node' | 'chef_role') -> [binary()].
%% @doc fetch list of object names
fetch_objects(Server, OrgId, Type) ->
    ChefDb = dbname(OrgId),
    {ok, Db} = couchbeam:open_db(Server, ChefDb, []),
    {DesignName, ViewName} = design_and_view_for_type(Type),
    {ok, View} = couchbeam:view(Db, {DesignName, ViewName}, []),
    lists:sort(couchbeam_view:fold(View,
                                   fun(Row, Acc) ->
                                           <<Key/binary>> = ej:get({<<"key">>}, Row),
                                           [Key | Acc]
                                   end)).

-spec fetch_objects_with_ids(couch_server(), binary(),
                             chef_object_name()) -> [{binary(), binary()}].
%% @doc fetch list of object `{name, ID}' tuples.
fetch_objects_with_ids(Server, OrgId, Type) ->
    ChefDb = dbname(OrgId),
    {ok, Db} = couchbeam:open_db(Server, ChefDb, []),
    {DesignName, ViewName} = design_and_view_for_type(Type),
    {ok, View} = couchbeam:view(Db, {DesignName, ViewName}, []),
    lists:sort(couchbeam_view:fold(View,
                                   fun(Row, Acc) ->
                                        <<Name/binary>> = ej:get({<<"key">>}, Row),
                                        <<Id/binary>> = ej:get({<<"id">>}, Row),
                                        [ {Name, Id} | Acc]
                                   end)).

-spec object_exists(couch_server(), binary(), binary(), chef_environment | chef_data_bag) -> boolean().
%% @doc Return true if we find an object named `Named' of the specified `Type'.
%%
object_exists(Server, OrgId, Name, Type) when is_binary(Name), is_binary(OrgId) ->
    ChefDb = dbname(OrgId),
    {ok, Db} = couchbeam:open_db(Server, ChefDb, []),
    {Design, ViewName} = design_and_view_for_type(Type),
    {ok, View} = couchbeam:view(Db, {Design, ViewName}, [{key, Name}]),
    case couchbeam_view:first(View) of
        {ok, {_Row}} -> true;
        {ok, []} -> false;
        {error, not_found} -> false
    end.

-spec design_and_view_for_type(chef_object_name() | authz_type()) -> {[1..255,...], [1..255,...]}.
%% @doc Return the CouchDB design doc and view name for `Type'
%%
design_and_view_for_type(Type) ->
    %% this could also be done all in fun-head matching, but that felt like more typing than
    %% one case statement.
    case Type of
        authz_client -> {?mixlib_auth_client_design, "by_clientname"};
        authz_container -> {?mixlib_auth_container_design, "by_containername"};
        authz_cookbook -> {?mixlib_auth_container_design, "by_display_name"};
        authz_data_bag -> {?mixlib_auth_data_bag_design, "by_name"};
        authz_environment -> {?mixlib_auth_environment_design, "all_id"};
        authz_group -> {?mixlib_auth_group_design, "by_groupname"};
        authz_node -> {?mixlib_auth_node_design, "by_name"};
        authz_role -> {?mixlib_auth_role_design, "by_name"};

        %% FIXME: for cookbooks, we are probably also interested in views: all_with_version
        %% for listing.
        %% chef_cookbook -> {?cookbook_design, "all_id"};
        chef_client -> {?client_design, "all_id"};
        chef_data_bag -> {?data_bag_design, "all_id"};
        chef_data_bag_item -> {?data_bag_item_design, "all_id"};
        chef_environment -> {?environment_design, "all_id"};
        chef_node -> {?node_design, "all_id"};
        chef_role -> {?role_design, "all_id"}
    end.


%% @doc Convert fetchable object types to the type of their corresponding authz record for
%% mixlib authz mapping object lookup.
authz_type(chef_client) ->
    authz_client;
%% authz_type(chef_cookbook) ->
%%     authz_cookbook;
authz_type(chef_data_bag) ->
    authz_data_bag;
%% authz_type(chef_data_bag_item) ->
%%     authz_data_bag;
authz_type(chef_environment) ->
    authz_environment;
authz_type(chef_node) ->
    authz_node;
authz_type(chef_role) ->
    authz_role.

-spec couch_json_to_record(chef_object_name(), binary(), binary(),
                           binary() | undefined, term()) -> chef_object() | #chef_client{}.
%% @doc Convert parsed JSON retrieved from CouchDB into a Chef record.  FIXME: the id field
%% may have dashes and will be unsuitable for use as an ID in our RDBMS schema.
%%
couch_json_to_record(chef_node, OrgId, AuthzId, RequesterId, Object) ->
    <<Name/binary>> = ej:get({<<"name">>}, Object),
    <<Environment/binary>> = ej:get({<<"chef_environment">>}, Object),
    Date = sql_date(now),
    <<NodeJs/binary>> = chef_json:encode(cleanup_couch_node_record(Object)),
    #chef_node{id = ej:get({<<"_id">>}, Object),
               authz_id = AuthzId,
               org_id = OrgId,
               name = Name,
               environment = Environment,
               last_updated_by = RequesterId,
               created_at = Date,
               updated_at = Date,
               serialized_object = NodeJs
              };
couch_json_to_record(chef_role, OrgId, AuthzId, RequesterId, Object) ->
    Name = ej:get({<<"name">>}, Object),
    Date = sql_date(now),
    RoleJs = chef_json:encode(cleanup_couch_role_record(Object)),
    #chef_role{id = ej:get({<<"_id">>}, Object),
               authz_id = AuthzId,
               org_id = OrgId,
               name = Name,
               last_updated_by = RequesterId,
               created_at = Date,
               updated_at = Date,
               serialized_object = RoleJs
              };
couch_json_to_record(data_bag, OrgId, AuthzId, RequesterId, Object) ->
    Name = ej:get({<<"name">>}, Object),
    Date = sql_date(now),
    #chef_data_bag{id = ej:get({<<"_id">>}, Object),
                   authz_id = AuthzId,
                   org_id = OrgId,
                   name = Name,
                   last_updated_by = RequesterId,
                   created_at = Date,
                   updated_at = Date
                  };
couch_json_to_record(chef_client, OrgId, AuthzId, RequesterId, Object) ->
    Name = ej:get({<<"name">>}, Object),
    Date = sql_date(now),
    #chef_client{id = ej:get({<<"_id">>}, Object),
                   authz_id = AuthzId,
                   org_id = OrgId,
                   name = Name,
                   last_updated_by = RequesterId,
                   created_at = Date,
                   updated_at = Date
                  };
couch_json_to_record(chef_environment, OrgId, AuthzId, RequesterId, Object) ->
    Name = ej:get({<<"name">>}, Object),
    Date = sql_date(now),
    EnvironmentJs = chef_json:encode(cleanup_couch_environment_record(Object)),
    #chef_environment{id = ej:get({<<"_id">>}, Object),
               authz_id = AuthzId,
               org_id = OrgId,
               name = Name,
               last_updated_by = RequesterId,
               created_at = Date,
               updated_at = Date,
               serialized_object = EnvironmentJs
              };
couch_json_to_record(chef_data_bag_item, OrgId, _AuthzId, RequesterId, Object) ->
    Name = ej:get({<<"name">>}, Object),
    Date = sql_date(now),
    Data_Bag_ItemJs = chef_json:encode(cleanup_couch_data_bag_item_record(Object)),
    #chef_data_bag_item{id = ej:get({<<"_id">>}, Object),
                        org_id = OrgId,
                        item_name = Name,
                        data_bag_name = ej:get({<<"data_bag">>}, Object),
                        last_updated_by = RequesterId,
                        created_at = Date,
                        updated_at = Date,
                        serialized_object = Data_Bag_ItemJs
                       };
couch_json_to_record(Type, _, _, _, _) ->
    %% FIXME: add more here
    erlang:error({error, {unimplemented, Type}}).

-type time_stamp() :: {non_neg_integer(), non_neg_integer(), non_neg_integer()}.

-spec sql_date(time_stamp() | 'now') -> binary().
%%
%% Emit in DATETIME friendly format
%% TODO: Modify to generate datetime pseudo record as used by emysql?
sql_date(now) ->
    sql_date(os:timestamp());
sql_date({_,_,_} = TS) ->
    {{Year,Month,Day},{Hour,Minute,Second}} = calendar:now_to_universal_time(TS),
    iolist_to_binary(io_lib:format("~4w-~2..0w-~2..0w ~2..0w:~2..0w:~2..0w",
                  [Year, Month, Day, Hour, Minute, Second])).

%% -define(ASSERT_RECORD(Object, RecName),
%%         case Object of
%%             #RecName{} -> Object;
%%             _ -> error({unexpected_type, {expected, #RecName{}}, {found, Object}})
%%         end).

%% assert_chef_object(Object, chef_data_bag) ->
%%     ?ASSERT_RECORD(Object, chef_data_bag);

%% %% not used, so left out for now to avoid dialyzer warning
%% %% assert_chef_object(Object, chef_data_bag_item) ->
%% %%     ?ASSERT_RECORD(Object, chef_data_bag_item);

%% assert_chef_object(Object, chef_environment) ->
%%     ?ASSERT_RECORD(Object, chef_environment);

%% assert_chef_object(Object, chef_node) ->
%%     ?ASSERT_RECORD(Object, chef_node);

%% assert_chef_object(Object, chef_role) ->
%%     ?ASSERT_RECORD(Object, chef_role).
