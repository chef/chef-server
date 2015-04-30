-module(mover_phase_2_migration_callback).

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
-include_lib("moser/include/moser.hrl").

migration_start_worker_args(Object, AcctInfo) ->
    [Object, AcctInfo].

migration_action(OrgName, AcctInfo) ->
    OrgInfo = moser_acct_processor:expand_org_info(#org_info{org_name = OrgName, account_info = AcctInfo}),
    {ok, _} = moser_utils:load_process_org(OrgInfo,
                     fun moser_group_converter:insert/1,
                     fun moser_chef_processor:cleanup_org_info/1,
                     "READ"),
    [{ok, done}].

migration_type() ->
    <<"phase_2_migration">>.

needs_account_dets() ->
    true.

supervisor() ->
    mover_org_migrator_sup.

error_halts_migration() ->
    true.

reconfigure_object(ObjectId, _AcctInfo) ->
    mover_org_darklaunch:org_to_sql(ObjectId, ?PHASE_2_MIGRATION_COMPONENTS).
