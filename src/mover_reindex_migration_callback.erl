-module(mover_reindex_migration_callback).

-export([
         migration_start_worker_args/2,
         migration_action/1,
         migration_type/0,
         supervisor/0,
         error_halts_migration/0,
         reconfigure_object/1
         ]).

-include_lib("moser/include/moser.hrl").

%Return any initial state not related to the object id
migration_start_worker_args(Object, AcctInfo) ->
    Org = moser_acct_processor:expand_org_info(#org_info{org_name = Object, account_info = AcctInfo}),
    [Org].

migration_action(#org_info{org_id = OrgId, org_name = OrgName}) ->
    IndexStateResults = chef_ez_reindex_direct:reindex(OrgName, OrgId, "http://localhost:8983/solr/update"),
    [ok = Result || Result <- IndexStateResults],
    [{ok, reindex_successful}].

migration_type() ->
    <<"reindex">>.

supervisor() ->
    mover_org_migrator_sup.

error_halts_migration() ->
    true.

reconfigure_object(_OrgInfo) ->
    ok.
