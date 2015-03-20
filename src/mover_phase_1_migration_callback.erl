-module(mover_phase_1_migration_callback).

-export([
         migration_start_worker_args/2,
         migration_action/2,
         migration_type/0,
         needs_account_dets/0,
         supervisor/0,
         error_halts_migration/0,
         reconfigure_object/2
         ]).

-include("mover.hrl").

%Return any initial state not related to the object id
migration_start_worker_args(Object, AcctInfo) ->
    [Object, AcctInfo].

migration_action(OrgName, AcctInfo) ->
    moser_converter:convert_org(OrgName, AcctInfo).

migration_type() ->
    <<"phase_1_migration">>.

needs_account_dets() ->
    true.

supervisor() ->
    mover_org_migrator_sup.

error_halts_migration() ->
    true.

reconfigure_object(ObjectId, _AcctInfo) ->
    mover_org_darklaunch:org_to_sql(ObjectId, ?PHASE_1_MIGRATION_COMPONENTS).
