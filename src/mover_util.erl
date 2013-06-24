%% -*- erlang-indent-level: 4;indent-tabs-mode: nil; fill-column: 92 -*-
%% ex: ts=4 sw=4 et
%% @author Marc A. Paradise <marc@opscode.com>
%% @copyright (C) 2013, Opscode Inc.
%%
%% @doc
%% Some simple org migration utilities.
%% @end

-module(mover_util).

-export([populate_xdl_with_unmigrated_orgs/0,
         reset_org/1,
         reset_orgs/1,
         reset_orgs_from_file/1]).

-include("mover.hrl").

-compile([{parse_transform, lager_transform}]).

%% @doc Get a list of unmigrated orgs from migration_state_table
%% and set the xdarklaunch flags
populate_xdl_with_unmigrated_orgs() ->
    Orgnames = moser_state_tracker:unmigrated_orgs(),
    [mover_org_darklaunch:init_org_to_couch(Orgname, ?PHASE_2_MIGRATION_COMPONENTS) || Orgname <- Orgnames].

%% @doc delete any SQL data for the named org and reset its state
%% to indicate it's ready to migrate.
%%

reset_org(OrgName) ->
    reset_org(OrgName, not_applicable).

reset_org(OrgName, Line) when is_list(OrgName) ->
    reset_org(iolist_to_binary(OrgName), Line);
reset_org(OrgName, Line) when is_binary(OrgName) ->
    case moser_state_tracker:ready_migration(OrgName) of
        ok ->
            Acct = moser_acct_processor:open_account(),
            case moser_acct_processor:get_org_guid_by_name(OrgName, Acct) of
                not_found ->
                    org_reset_error(OrgName, Line, "org does not exist in account table");
                GUID ->
                    moser_chef_converter:cleanup_orgid(GUID)
            end;
        Other ->
            org_reset_error(OrgName, Line, Other)
    end.

org_reset_error(OrgName, Line, Reason) ->
    log_org_reset_error(OrgName, Line, Reason),
    % Advance state if possible, so that we can mark as failed.
    ok = case moser_state_tracker:migration_started(OrgName) of
        ok -> ok;
        {error, not_in_expected_state, _} -> ok;
        Error -> Error
    end,
    ok = case moser_state_tracker:migration_failed(OrgName, reset_org) of
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
reset_orgs(Orgs) when is_list(Orgs) ->
    [ reset_org(X) || X <- Orgs ].

%% @doc Reset each org in the provided file, which must contain
%% one org per line.
reset_orgs_from_file(FileName) ->
    {ok, Dev} = file:open(FileName, [read]),
    process_lines(Dev, fun reset_org/2, 1).

process_lines(Dev, Processor, LineNo) ->
    case io:get_line(Dev, "") of
        eof ->
            file:close(Dev),
            ok;
        "\n" ->
            process_lines(Dev, Processor, LineNo + 1);
        Line ->
            Processor(string:strip(Line, right, $\n), LineNo),
            process_lines(Dev, Processor, LineNo + 1)
    end.
