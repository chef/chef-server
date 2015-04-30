-module(mover_global_containers_migration_callback).

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

-include("mover.hrl").
-include_lib("moser/include/moser.hrl").

migration_init() ->
    AcctInfo = moser_acct_processor:open_account(),
    mover_transient_migration_queue:initialize_queue(?MODULE, moser_acct_processor:get_global_containers_list(AcctInfo)).

migration_start_worker_args(Object, AcctInfo) ->
    [Object, AcctInfo].

next_object() ->
    mover_transient_migration_queue:next(?MODULE).

migration_action(Object, _AcctInfo) ->
    {Guid, AuthzId, RequesterId, Data} = Object,
    moser_global_object_converter:insert_container(Guid, AuthzId, RequesterId, Data).

migration_type() ->
    <<"global_container_migration">>.

supervisor() ->
    mover_transient_worker_sup.

error_halts_migration() ->
    true.

reconfigure_object(_ObjectId, _AcctInfo) ->
    no_op.
