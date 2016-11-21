%% -*- erlang-indent-level: 4;indent-tabs-mode: nil;fill-column: 92 -*-
%% ex: ts=4 sw=4 et
%% @author Marc A. Paradise <marc@chef.io>
%% @author Tyler Cloke <tyler@chef.io>
%% @copyright 2013-2014 Chef, Inc.
%%

-module(mover_batch_migrator).

-export([
         ping/0,
         migrate_all/0,
         migrate_all/1
        ]).

-include_lib("moser/include/moser.hrl").

ping() ->
    pong.

migrate_all() ->
    migrate_all(mover_phase_1_migration_callback).

migrate_all(CallbackMod) ->
    % Disable sleep  - for OPC migrations, we are guaranteed no in-flight traffic.
    application:set_env(mover, sleep_time, 0),

    % Build dets tables
    mover_manager:create_account_dets(CallbackMod),

    % Migrate
    R = proceed_migration(CallbackMod),

    % Reply with results
    process_results(R).

proceed_migration(CallbackMod) ->
    MigrationType = CallbackMod:migration_type(),
    proceed_migration(capture_org_state(MigrationType), CallbackMod).

proceed_migration({ok, _}, CallbackMod) ->
    migrate(CallbackMod);
proceed_migration({error, Reason}, _) ->
    {error, Reason, []}.

migrate(CallbackMod) ->
    Orgs = org_list(CallbackMod),
    migrate_next(Orgs, [], CallbackMod).

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

org_list(CallbackMod) ->
    moser_state_tracker:unmigrated_orgs(CallbackMod:migration_type()).

%% We'll migrate each org  individually so that we are
%% able to reply with details for each org.
migrate_next([], Acc, _) ->
    Acc;
migrate_next([Org|Rest], Acc, CallbackMod) ->
    lager:info([{org_name, Org}], "Requesting migration: ~p~n"),
    case reset_org(Org, CallbackMod) of
        ok ->
            do_migrate(Org, Rest, Acc, CallbackMod);
        Reason ->
            migrate_next(Rest, [{Org, Reason} | Acc], CallbackMod)
    end.

%% An org reset can fail for a number of valid reasons. When we are following the flow
%% of interactively resetting multiple orgs then continuing, it makes sense to abort up front.
%% However, in an unattended migration, we'll capture any error and just note that we can't reset the org,
%% so that we can move on to the next org.
reset_org(OrgName, CallbackMod) ->
    try
        mover_util:reset_org(OrgName, CallbackMod:migration_type()),
        ok
    catch
        % Note that details are already captured in the error log - we'll
        % continue processing so we can accumulate additional errors for
        % analysis.
        _Mod:_Reason ->
            {error, org_reset_failed}
    end.

%% Kick off an org migration, and wait for it to finish
do_migrate(Org, Remaining, Acc, CallbackMod) ->
    % Crash if mover manager fsm isn't in the expected state
    {ok, burning_couches} = mover_manager:migrate_next(CallbackMod),
    Status = mover_util:wait_for_status(),
    case proplists:get_value(fatal_stop, Status) of
        true ->
            % as with all errors, we will have captured error details in the log
            {error, {migrations_halted, fatal_error}, Acc};
        false ->
            % A specific org may still fail, but that's not fatal - check for that results here.
            Result = case lists:keysearch(objects_failed, 1, Status) of
                {value, {objects_failed, 1}} ->
                    {Org, migration_failure};
                {value, {objects_failed, 0}} ->
                    {Org, migration_success}
            end,
            % In any case, capture the result and keep going.
            migrate_next(Remaining, [Result | Acc], CallbackMod)
    end.

%% Populate the org state table.  If it encounters an org that is
%% already in the table, it will ignore it - any other error will cause it to abort
%% insert operations
%%
%% This functionality used to be in moser_state_tracker but was removed
%% in order to avoid any inadvertent possibility of rebuilding org state
%% table after org creation was cut over to sql in production.
capture_org_state(MigrationType) ->
    Info = mover_manager:get_account_dets(),
    moser_state_tracker:capture_full_org_state_list(Info, MigrationType).
