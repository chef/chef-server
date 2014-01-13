-module(mover_phase_1_migrator_callback).

-export([
         migration_start_worker_args/2,
         migration_action/2,
         migration_type/0,
         supervisor/0,
         error_halts_migration/0
         ]).

%Return any initial state not related to the object id
migration_start_worker_args(Object, AcctInfo) ->
    [Object, AcctInfo].

migration_action(OrgName, AcctInfo) ->
    moser_converter:convert_org(OrgName, AcctInfo).

migration_type() ->
    <<"phase_1_migration">>.

supervisor() ->
    mover_org_migrator_sup.

error_halts_migration() ->
    true.
