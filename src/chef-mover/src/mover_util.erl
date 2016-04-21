%% -*- erlang-indent-level: 4;indent-tabs-mode: nil; fill-column: 92 -*-
%% ex: ts=4 sw=4 et
%% @author Marc A. Paradise <marc@chef.io>
%% @copyright (C) 2013, Opscode Inc.
%%
%% @doc
%% Some simple org migration utilities.
%% @end

-module(mover_util).

-export([populate_xdl_with_unmigrated_orgs/0,
         reset_org/2,
         reset_orgs/2,
         reset_orgs_from_file/2,
         call_if_exported/4,
         wait_for_status/0
        ]).

-include("mover.hrl").

-define(POLL_SLEEP_MS, 500).

%% @doc poll mover_manager:status until it indicates that it's completed.
%% In the case that things get stuck halting, it will try 240 more times
%% (two minutes at current sleep time).
wait_for_status() ->
    {ok, Status} = mover_manager:status(),
    case proplists:get_value(state, Status) of
        ready ->
            Status;
        _ ->
            timer:sleep(?POLL_SLEEP_MS),
            wait_for_status()
    end.

%% @doc Get a list of unmigrated orgs from migration_state_table
%% and set the xdarklaunch flags
populate_xdl_with_unmigrated_orgs() ->
    Orgnames = moser_state_tracker:unmigrated_orgs(mover_phase_1_migration_callback:migration_type()),
    [mover_org_darklaunch:init_org_to_couch(Orgname, ?PHASE_1_MIGRATION_COMPONENTS) || Orgname <- Orgnames].

%% @doc delete any SQL data for the named org and reset its state
%% to indicate it's ready to migrate.
%%
reset_org(OrgName, MigrationType) ->
    reset_org(OrgName, not_applicable, MigrationType).

reset_org(OrgName, Line, MigrationType) when is_list(OrgName) ->
    reset_org(iolist_to_binary(OrgName), Line, MigrationType);
reset_org(OrgName, Line, MigrationType) when is_binary(OrgName) ->
    case moser_state_tracker:ready_migration(OrgName, MigrationType) of
        ok ->
	    % Check if GUID can be found from OrgName and log an error if it doesn't.
	    % Vestigial code from when the GUID was actually needed but it doesn't hurt to
	    % check this very rare error case still.
            Acct = moser_acct_processor:open_account(),
            case moser_acct_processor:get_org_guid_by_name(OrgName, Acct) of
                not_found ->
                    org_reset_error(OrgName, Line, "org does not exist in account table", MigrationType);
                _GUID ->
                    ok
            end;
        Other ->
            org_reset_error(OrgName, Line, Other, MigrationType)
    end.

org_reset_error(OrgName, Line, Reason, MigrationType) ->
    log_org_reset_error(OrgName, Line, Reason),
    % Advance state if possible, so that we can mark as failed.
    ok = case moser_state_tracker:migration_started(OrgName, MigrationType) of
        ok -> ok;
        {error, not_in_expected_state, _} -> ok;
        Error -> Error
    end,
    ok = case moser_state_tracker:migration_failed(OrgName, reset_org, MigrationType) of
        ok -> ok;
        {error, not_in_expected_state, State} ->
            log_org_reset_error(OrgName, Line, "Org state could not be set to failed."),
            State;
         Error2 -> Error2
    end.

log_org_reset_error(OrgName, not_applicable, Error) ->
    lager:error([{org_name, OrgName}], "Error while resetting org: ~p", [Error]);
log_org_reset_error(OrgName, Line, Error) ->
    lager:error([{org_name, OrgName}], "Error on line ~p while resetting org: ~p", [Line, Error]).

%% @doc Reset each org in the provided list of orgs.
reset_orgs(Orgs, MigrationType) when is_list(Orgs) ->
    [ reset_org(X, MigrationType) || X <- Orgs ].

%% @doc Reset each org in the provided file, which must contain
%% one org per line.
reset_orgs_from_file(FileName, MigrationType) ->
    {ok, Dev} = file:open(FileName, [read]),
    process_lines(Dev, MigrationType, fun reset_org/2, 1).

process_lines(Dev, MigrationType, Processor, LineNo) ->
    case io:get_line(Dev, "") of
        eof ->
            file:close(Dev),
            ok;
        "\n" ->
            process_lines(Dev, MigrationType, Processor, LineNo + 1);
        Line ->
            Processor(string:strip(Line, right, $\n), LineNo, MigrationType),
            process_lines(Dev, MigrationType, Processor, LineNo + 1)
    end.

call_if_exported(undefined, _FunName, Args, DefaultFun) ->
    erlang:apply(DefaultFun, Args);
call_if_exported(Mod, FunName, Args, DefaultFun) ->
    %% Modules that haven't been called are lazily loaded in the node
    %% This results in function_exported returning false even if the
    %% function is exported with correct arity.  code:ensure_loaded
    %% will load the module and return a bad match if the module is
    %% missing.
    {module, _} = code:ensure_loaded(Mod),
    case erlang:function_exported(Mod, FunName, length(Args)) of
        true ->
            erlang:apply(Mod, FunName, Args);
        false  ->
            erlang:apply(DefaultFun, Args)
    end.
