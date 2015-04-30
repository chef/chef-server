-module(mover_reindex_migration_callback).

-export([
         migration_start_worker_args/2,
         migration_action/1,
         migration_type/0,
         supervisor/0,
         error_halts_migration/0,
         reconfigure_object/1,
         needs_account_dets/0
         ]).

-include_lib("moser/include/moser.hrl").

needs_account_dets() ->
    true.

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
    <<"solr4_migration">>.

supervisor() ->
    mover_org_migrator_sup.

error_halts_migration() ->
    true.

reconfigure_object(#org_info{org_name = OrgName}) ->
    mover_org_darklaunch:enable_solr4(OrgName).

delete_existing_db(OrgId) ->
    Doc = [<<"<?xml version='1.0' encoding='UTF-8'?><delete><query>">>,
           <<"X_CHEF_database_CHEF_X:chef_">>,
           OrgId,
           <<"</query></delete>">>],
    DocBin = iolist_to_binary(Doc),
    {ok, Code, _Head, Body} = chef_reindex_http:request("update",post, DocBin),
    case Code of
        "2" ++ _Rest ->
            ok;
        _  ->
            {error, {Code, Body}}
    end.
