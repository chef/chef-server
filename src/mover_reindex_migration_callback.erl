-module(mover_reindex_migration_callback).

-export([
         next_object/0,
         migration_start_worker_args/3,
         migration_action/2,
         migration_type/0,
         supervisor/0,
         error_halts_migration/0
         ]).

-include_lib("moser/include/moser.hrl").

next_object() ->
    moser_state_tracker:next_ready_org(<<"reindex">>).

%Return any initial state not related to the object id
migration_start_worker_args(Object, AcctInfo, _ProcessorFun) ->
    Org = moser_acct_processor:expand_org_info(#org_info{org_name = Object, account_info = AcctInfo}),
    [Org].

migration_action(OrgName, AcctInfo) ->
    moser_converter:convert_org(OrgName, AcctInfo).

migration_type() ->
    <<"reindex">>.

supervisor() ->
    mover_org_migrator_sup.

error_halts_migration() ->
    true.
