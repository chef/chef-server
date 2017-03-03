%% -*- erlang-indent-level: 4;indent-tabs-mode: nil; fill-column: 92 -*-
%% ex: ts=4 sw=4 et
%% @author Marc Paradise <marc@chef.io>
%% @copyright 2017 Chef Software, Inc.
%%
%% This module exists to validate chef-mover connectivity to
%% the components it needs (postgres, redis, bifrost)
%%
%% In order to use it first create some orgs:
%%    chef-server-ctl org-create org1 org1
%%    chef-server-ctl org-create org2 org2
%%
%% mover_manager:migrate(all, 1, mover_test_migration).
%%
%% If this is a live box, remember to delete the orgs:
%%
%%    chef-server-ctl org-delete org1
%%    chef-server-ctl org-delete org2
%%
-module(mover_test_migration).

-export([
         migration_init/0,
         migration_complete/0,
         migration_type/0,
         supervisor/0,
         migration_start_worker_args/2,
         migration_action/2,
         next_object/0,
         error_halts_migration/0,
         reconfigure_object/2,
         needs_account_dets/0
        ]).

-include("mover.hrl").

-record(org, {name, id, authz_id}).

migration_init() ->
    mv_oc_chef_authz_http:create_pool(),
    mover_transient_migration_queue:initialize_queue(?MODULE, all_orgs()).

migration_action(#org{name = Name} = Org, _) ->
    io:fwrite("Processing org ~p, just so you know the superuser authz ID is ~p and mover_org_darklaunch:disable_org replied ~p~n",
              [Name, mv_oc_chef_authz:superuser_id(), mover_org_darklaunch:disable_org(Name)]),
    ok = mover_org_darklaunch:enable_org(Name).

migration_complete() ->
    mv_oc_chef_authz_http:delete_pool().



all_orgs() ->
    %% TODO: Will this be Bad(TM) in Hosted?
    {ok, Orgs} = sqerl:select(<<"SELECT name, id, authz_id FROM orgs">>,[], rows_as_records, [org, record_info(fields, org)]),
    Orgs.

%%
%% Generic mover callback functions for
%% a transient queue migration
%%
needs_account_dets() ->
    false.

migration_start_worker_args(Object, AcctInfo) ->
    [Object, AcctInfo].

next_object() ->
    mover_transient_migration_queue:next(?MODULE).

migration_type() ->
    <<"test_migration">>.

supervisor() ->
    mover_transient_worker_sup.

error_halts_migration() ->
    false.

reconfigure_object(_ObjectId, _AcctInfo) ->
    ok.
