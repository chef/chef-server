-module(mover_phase_1_migrator_callback).

-export([
         next_object/0,
         migration_start_worker_args/3,
         migration_action/2,
         migration_type/0,
         supervisor/0,
         error_halts_migration/0
         ]).

next_object() ->
    moser_state_tracker:next_ready_org().

%Return any initial state not related to the object id
migration_start_worker_args(Object, AcctInfo, _ProcessorFun) ->
    [Object, AcctInfo].

migration_action(OrgName, AcctInfo) ->
    moser_converter:convert_org(OrgName, AcctInfo).

migration_type() ->
    <<"phase_1">>.

supervisor() ->
    mover_org_migrator_sup.

error_halts_migration() ->
    true.
