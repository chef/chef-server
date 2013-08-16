%% -*- erlang-indent-level: 4;indent-tabs-mode: nil;fill-column: 92 -*-
%% ex: ts=4 sw=4 et
%% @author Marc A. Paradise <marc@opscode.com>
%% @copyright 2013 Opscode, Inc.
%%

-module(mover_batch_migrator).

-export([ping/0,
         migrate_all/0]).

-define(POLL_SLEEP_MS, 500).

% Stolen from moser/src/moser.hrl
-record(org_info,
        { org_name = undefined,
          org_id = undefined,
          db_name = undefined,
          is_precreated = false,
          chef_ets,
          auth_ets,
          account_info = undefined,
          start_time}).

-compile([{parse_transform, lager_transform}]).

ping() ->
    pong.

migrate_all() ->
    % Disable sleep  - for OPC migrations, we are guaranteed no in-flight traffic.
    application:set_env(mover, sleep_time, 0),

    % Build dets tables
    mover_manager:create_account_dets(),

    % Migrate
    R = proceed_migration(),

    % Reply with results
    process_results(R).

proceed_migration() ->
    proceed_migration(capture_org_state()).

proceed_migration({ok, _}) ->
    migrate();
proceed_migration({error, Reason}) ->
    {error, Reason, []}.

migrate() ->
    Orgs = org_list(),
    migrate_next(Orgs, []).

process_results({error, Reason, CompletedOrgs}) ->
    [{status, {aborted, Reason}} | partition_results(CompletedOrgs)];
process_results(CompletedOrgs) when is_list(CompletedOrgs) ->
    [{status, complete} | partition_results(CompletedOrgs)].

partition_results(Results) ->
    {RF, MF, S} = lists:foldl(fun partition/2, {[],[],[]}, Results),
    [{successful_orgs, S},
     {failed_orgs, MF},
     {reset_failed, RF}].

partition({Org, migration_success}, {RF, MF, S}) ->
    {RF, MF, [binary_to_list(Org) | S]};
partition({Org, migration_failure}, {RF, MF, S}) ->
    {RF, [binary_to_list(Org) | MF], S};
partition({Org, {error, org_reset_failed}}, {RF, MF, S}) ->
    {[binary_to_list(Org) | RF], MF, S}.

org_list() ->
    org_list(moser_state_tracker:unmigrated_orgs()).

org_list(no_orgs_in_state) ->
    [];
org_list(OrgList) ->
    OrgList.

%% We'll migrate each org  individually so that we are
%% able to reply with details for each org.
migrate_next([], Acc) ->
    Acc;
migrate_next([Org|Rest], Acc) ->
    lager:info([{org_name, Org}], "Requesting migration: ~p~n"),
    case reset_org(Org) of
        ok ->
            do_migrate(Org, Rest, Acc);
        Reason ->
            migrate_next(Rest, [{Org, Reason} | Acc])
    end.

%% An org reset can fail for a number of valid reasons. When we are following the flow
%% of interactively resetting multiple orgs then continuing, it makes sense to abort up front.
%% However, in an unattended migration, we'll capture any error and just note that we can't reset the org,
%% so that we can move on to the next org.
reset_org(OrgName) ->
    try
        mover_util:reset_org(OrgName),
        ok
    catch
        % Note that details are already captured in the error log - we'll
        % continue processing so we can accumulate additional errors for
        % analysis.
        _Mod:_Reason ->
            {error, org_reset_failed}
    end.

%% Kick off an org migration, and wait for it to finish
do_migrate(Org, Remaining, Acc) ->
    % Crash if mover manager fsm isn't in the expected state
    {ok, burning_couches} = mover_manager:migrate_next(),
    Status = wait_for_status(),
    case proplists:get_value(fatal_stop, Status) of
        true ->
            % as with all errors, we will have captured error details in the log
            {error, {migrations_halted, fatal_error}, Acc};
        false ->
            % A specific org may still fail, but that's not fatal - check for that results here.
            Result = case lists:keysearch(orgs_failed, 1, Status) of
                {value, {orgs_failed, 1}} ->
                    {Org, migration_failure};
                {value, {orgs_failed, 0}} ->
                    {Org, migration_success}
            end,
            % In any case, capture the result and keep going.
            migrate_next(Remaining, [Result | Acc])
    end.



%% poll mover_manager:status until it indicates that it's completed.
wait_for_status() ->
	{ok, Status} = mover_manager:status(),
    case proplists:get_value(state, Status) of
        ready ->
            Status;
        _ ->
            timer:sleep(?POLL_SLEEP_MS),
            wait_for_status()
    end.

%% Populate the org state table.  If it encounters an org that is
%% already in the table, it will ignore it - any other error will cause it to abort
%% insert operations
%%
%% This functionality used to be in moser_state_tracker but was removed
%% in order to avoid any inadvertent possibility of rebuilding org state
%% table after org creation was cut over to sql in production.
capture_org_state() ->
    Info = mover_manager:get_account_dets(),
    AllOrgs = moser_acct_processor:all_orgs(Info),
    case insert_orgs(AllOrgs, []) of
       {error, Reason} ->
           {error, Reason};
       Result when is_list(Result) ->
           {ok, Result}
    end.

insert_orgs([], Acc) ->
    Acc;
insert_orgs([#org_info{org_name = OrgName, org_id = OrgId} = Org | Rest], Acc) ->
    case moser_state_tracker:insert_one_org(Org) of
        ok ->
            insert_orgs(Rest, [{OrgName, ok} | Acc]);
        {error, {error, error, <<"23505">>, _Msg, _Detail}} ->
            lager:warning([{org_name,OrgName}, {org_id, OrgId}], "org already exists, ignoring"),
            insert_orgs(Rest, [{OrgName, duplicate} | Acc]);
        {error, Reason} ->
            lager:error([{org_name,OrgName}, {org_id, OrgId}],
                        "stopping - failed to capture state because: ~p", [Reason]),
            {error, {org_state_load_failed, Reason}}
    end.

