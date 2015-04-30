-module(mover_user_hash_converter_callback).

-export([
         migration_init/0,
         migration_start_worker_args/2,
         migration_action/1,
         next_object/0,
         migration_type/0,
         supervisor/0,
         error_halts_migration/0,
         reconfigure_object/2
         ]).

migration_init() ->
    mover_transient_migration_queue:initialize_queue(?MODULE, mover_user_hash_converter:remaining_user_ids()),
    mover_user_hash_converter:start_bcrypt_pool().


migration_start_worker_args(Object, _AcctInfo) ->
    [Object].

migration_action(ObjectId) ->
    mover_user_hash_converter:convert_user(ObjectId).

next_object() ->
    mover_transient_migration_queue:next(?MODULE).

supervisor() ->
    mover_transient_worker_sup.

migration_type() ->
    <<"user_hash_converter">>.

error_halts_migration() ->
    true.

reconfigure_object(_, _) ->
    no_op.
