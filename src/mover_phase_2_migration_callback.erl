-module(mover_phase_2_migration_callback).

-export([
         migration_start_worker_args/2,
         migration_action/2,
         migration_type/0,
         supervisor/0,
         error_halts_migration/0,
         reconfigure_object/2
         ]).

-include("mover.hrl").

migration_start_worker_args(Object, AcctInfo) ->
    [Object, AcctInfo].

migration_action(OrgName, _AcctInfo) ->
    io:fwrite("I have pretended to migrate stuff for ~p~n", [OrgName]),
    [{ok, done}].

migration_type() ->
    <<"phase_2_migration">>.

supervisor() ->
    mover_org_migrator_sup.

error_halts_migration() ->
    true.

reconfigure_object(ObjectId, _AcctInfo) ->
    mover_org_darklaunch:org_to_sql(ObjectId, ?PHASE_2_MIGRATION_COMPONENTS).
