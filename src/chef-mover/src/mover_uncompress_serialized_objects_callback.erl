%% -*- erlang-indent-level: 4;indent-tabs-mode: nil; fill-column: 92 -*-
%% ex: ts=4 sw=4 et
%% @author Jason Reed <jreed@chef.io>
%% @copyright 2015 Chef, Inc.
-module(mover_uncompress_serialized_objects_callback).

% This is a migration that uncompresses (specifically, ungzips) all serialized_object
% columns in the table nodes. It is meant to follow a change to chef-server which stops
% compression of new inbound database items. A useful consequence of not manually
% uncompressing all the json blobs is the exposure of the underlying data postgres json
% operators and indexing.

% The hope is that postgres's own TOAST mechanism for compression and external storage of
% large column values will do as well as manual gzipping. A preliminary experiment consisting
% of doing
% > ALTER TABLE nodes ALTER serialized_object SET STORAGE EXTENDED;
% (since the storage mode of serialized_object was EXTERNAL before, which inhibits
% compression, see http://www.postgresql.org/docs/9.4/static/storage-toast.html) exhibits
% about a 2x increase in pg_total_relation_size('nodes') on a chef-server populated by many
% copies of the file lib/fauxhai/platforms/windows/2003R2.json from the repository
% https://github.com/customink/fauxhai but it may be hoped that the use of jsonb or
% other postgres tuning might bring this down to parity with gzip.

-export([
	 migration_init/0,
	 migration_type/0,
	 supervisor/0,
	 migration_start_worker_args/2,
	 error_halts_migration/0,
	 reconfigure_object/2,
	 migration_action/2,
	 next_object/0
	]).

-record(node_record, {
        'id',
        'serialized_object'
       }).

-include("mover.hrl").

migration_init() ->
    mover_transient_migration_queue:initialize_queue(?MODULE,
                                                     all_unconverted_nodes()),
    ok.

migration_start_worker_args(Object, AcctInfo) ->
    [Object, AcctInfo].

migration_action(Row, AcctInfo) ->
    Id = Row#node_record.id,
    SerializedObject = Row#node_record.serialized_object,
    NewObject = chef_db_compression:decompress(SerializedObject),
    {ok, _Num} = sqerl:execute(node_update_sql(), [Id, NewObject]),
    ok.

next_object() ->
    mover_transient_migration_queue:next(?MODULE).

migration_type() ->
    <<"uncompress_serialized_objects">>.

supervisor() ->
    mover_transient_worker_sup.

error_halts_migration() ->
    true.

reconfigure_object(_ObjectId, _AcctInfo) ->
    no_op.

all_unconverted_nodes() ->
    {ok, Rows} = sqerl:execute(all_unconverted_nodes_sql(), []),
    XF = sqerl_transformers:rows_as_records(node_record, record_info(fields, node_record)),
    {ok, Data} = XF(Rows),
    Data.

all_unconverted_nodes_sql() ->
    <<"SELECT id, serialized_object FROM nodes">>.

node_update_sql() ->
    <<"UPDATE nodes
          SET serialized_object = $2
        WHERE id = $1">>.
