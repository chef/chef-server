-module(mover_user_hash_converter_callback).

-export([
         migration_init/0,
         migration_start_worker_args/2,
         migration_action/1,
         next_object/0,
         migration_type/0,
         supervisor/0,
         error_halts_migration/0
         ]).

migration_init() ->
    status_check(),
    mover_transient_migration_queue:initialize_queue(mover_user_hash_converter:remaining_user_ids()),
    mover_user_hash_converter:start_bcrypt_pool().


migration_start_worker_args(Object, _AcctInfo) ->
    [Object].

migration_action(Object) ->
    mover_user_has_converter:convert_user(Object).

status_check() ->
    {ok, Status} = mover_manager:status(),
    ready = proplists:get_value(state, Status),
    mover_transient_migration_queue:initialize_queue([]).

next_object() ->
    mover_transient_migration_queue:next().

supervisor() ->
    mover_transient_worker_sup.

migration_type() ->
    <<"user_hash_converter">>.

error_halts_migration() ->
    true.
