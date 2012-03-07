%% -*- erlang-indent-level: 4;indent-tabs-mode: nil;fill-column: 92 -*-
%% ex: ts=4 sw=4 et
%% @author Seth Falcon <seth@opscode.com>
%% @copyright 2012 Opscode, Inc.

-module(mover_node_worker).
-behaviour(mover_gen_worker).

-include("mover.hrl").
-include_lib("chef_common/include/chef_sql.hrl").

-export([authz_type/0,
         convert_couch_json_to_object_record/4,
         create_object/3,
         delete_object_from_solr/2,
         fetch_objects/2,
         fetch_objects_from_sql/1,
         object_id/1,
         object_name/0,
         send_object_to_solr/2]).

authz_type() ->
    authz_node.

convert_couch_json_to_object_record(OrgId, AuthzId, RequestorId, NodeData) ->
    chef_otto:convert_couch_json_to_node_record(OrgId, AuthzId, RequestorId, NodeData).

create_object(Cn, Node, RequestorId) ->
    chef_db:create_node(Cn, Node, RequestorId).

delete_object_from_solr(NodeId, OrgId) ->
    ok = chef_index_queue:delete(node, NodeId, chef_otto:dbname(OrgId), {[]}),
    ok.

fetch_objects(S, OrgId) ->
    chef_otto:fetch_nodes_with_ids(S, OrgId).

fetch_objects_from_sql(OrgId) ->
    chef_sql:fetch_nodes(OrgId).

object_id(#chef_node{id = Id}) ->
    Id.

object_name() ->
    node.

send_object_to_solr(#chef_node{id = Id, org_id = OrgId}, NodeJson) ->
    ok = chef_index_queue:set(node, Id,
                              chef_otto:dbname(OrgId),
                              chef_node:ejson_for_indexing(NodeJson)),
    ok.


