-module(mover_reindex_dry_run_revert_migration_callback).

-export([
         migration_init/0,
         migration_complete/0,
         migration_started/2,
         migration_successful/2,
         migration_failed/3,
         migration_start_worker_args/2,
         migration_action/1,
         migration_type/0,
         supervisor/0,
         error_halts_migration/0,
         reconfigure_object/1,
         next_object/0,
         disable_object/1
         ]).

-include_lib("moser/include/moser.hrl").

migration_init() ->
    mover_transient_migration_queue:initialize_queue(?MODULE, moser_state_tracker:migrated_orgs(mover_reindex_dry_run_migration_callback:migration_type())).

migration_complete() ->
    ok.

migration_started(_, _) ->
    ok.

migration_successful(_, _) ->
    ok.

migration_failed(_, _, _) ->
    ok.

next_object() ->
    mover_transient_migration_queue:next(?MODULE).

%Return any initial state not related to the object id
migration_start_worker_args(Object, _AcctInfo) ->
    [Object].

migration_action(OrgName) ->
    moser_state_tracker:force_org_to_state(OrgName, mover_reindex_dry_run_migration_callback:migration_type(), <<"ready">>),
    [{ok, reindex_revert_successful}].

migration_type() ->
    <<"solr4_migration_dry_run">>.

supervisor() ->
    mover_org_migrator_sup.

error_halts_migration() ->
    true.

reconfigure_object(OrgName) ->
    mover_org_darklaunch:enable_solr1(OrgName).

disable_object(_) ->
    ok.
