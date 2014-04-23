-module(mover_reindex_dry_run_migration_callback).

-export([
         migration_start_worker_args/2,
         migration_action/1,
         migration_type/0,
         supervisor/0,
         error_halts_migration/0,
         reconfigure_object/1,
         disable_object/1
         ]).

-include_lib("moser/include/moser.hrl").

%Return any initial state not related to the object id
migration_start_worker_args(Object, AcctInfo) ->
    Org = moser_acct_processor:expand_org_info(#org_info{org_name = Object, account_info = AcctInfo}),
    [Org].

migration_action(#org_info{org_id = OrgId, org_name = OrgName}) ->
    delete_existing_db(OrgId),
    IndexStateResults = chef_ez_reindex_direct:reindex(OrgName, OrgId),
    [ok = Result || Result <- IndexStateResults],
    [{ok, reindex_successful}].

migration_type() ->
    <<"solr4_migration_dry_run">>.

supervisor() ->
    mover_org_migrator_sup.

error_halts_migration() ->
    true.

reconfigure_object(#org_info{org_name = OrgName}) ->
    mover_org_darklaunch:enable_both_solrs(OrgName).

delete_existing_db(OrgId) ->
    Doc = [<<"<?xml version='1.0' encoding='UTF-8'?><delete><query>">>,
           <<"X_CHEF_database_CHEF_X:chef_">>,
           OrgId,
           <<"</query></delete>">>],
    DocBin = iolist_to_binary(Doc),
    {ok, Code, _Head, Body} = chef_reindex_http:request("update", post, DocBin),
    case Code of
        "2" ++ _Rest ->
            ok;
        _  ->
            {error, {Code, Body}}
    end.

disable_object(_) ->
    ok.
