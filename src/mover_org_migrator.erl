%% -*- erlang-indent-level: 4;indent-tabs-mode: nil;fill-column: 92 -*-
%% ex: ts=4 sw=4 et
%% @author Marc Paradise <marc@opscode.com>
%% @copyright 2013 Opscode, Inc.
%%
%% @doc a gen_fsm that migrates a single org from end to end.
%%

-module(mover_org_migrator).
-behaviour(gen_fsm).

%% Comment this out for VIM syntax check/compiles to work.
-compile([{parse_transform, lager_transform}]).

%% gen_fsm callbacks
-export([init/1,
         handle_event/3,
         handle_sync_event/4,
         handle_info/3,
         terminate/3,
         code_change/4]).

%% states
-export([disable_org_access/2,
         sleep/2,
         migrate_org/2,
         verify_org/2,
         set_org_to_sql/2,
         enable_org_access/2 ]).

-include("mover.hrl").

%% api
-export([start_link/1]).

-record(state, {
                 org_name :: string()     %% The org we are migrating
               }).


start_link(Config) ->
    gen_fsm:start_link(?MODULE, Config, []).

init(OrgName) ->
    State = #state{org_name = OrgName},
    case moser_state_tracker:migration_started(OrgName) of
        ok ->
            lager:info([{org_name, OrgName}], "Starting migration."),
            {ok, disable_org_access, State, 0};
        Error ->
            stop_with_failure(State, Error, init)
    end.

disable_org_access(timeout, #state{org_name = OrgName} = State) ->
    case mover_org_darklaunch:disable_org(OrgName) of
        ok ->
            {next_state, sleep, State, 0};
        {error, Error} ->
            stop_with_failure(State, Error, disable_org_access)
    end.

%% The sleep state is configured to add a wait period immediately
%% after placing an org in 503 (downtime) mode. This wait period
%% helps ensure that any in-flight writes to an organizaiton at the
%% start of a migration will be captured. The configurable value of
%% the sleep time will be calculated after analyzing the reponse
%% times of CUD operations to OHC organizations.
%%
sleep(timeout, #state{} = State) ->
    timer:sleep(envy:get(mover, sleep_time, integer)),
    {next_state, migrate_org, State, 0}.

migrate_org(timeout, #state{org_name = OrgName} = State) ->
    try moser_converter:convert_org(OrgName) of
        [{ok, _}] ->
            {next_state, verify_org, State, 0};
        Error ->
            stop_with_migration_error(State, Error)
    catch
        _ErrorType:Reason ->
            stop_with_migration_error(State, Reason)
    end.

verify_org(timeout, #state{org_name = _OrgName} = State) ->
    %% Placeholder: verification is currently external.
    {next_state, set_org_to_sql, State, 0}.

set_org_to_sql(timeout, #state{org_name = OrgName} = State) ->
    case mover_org_darklaunch:org_to_sql(OrgName, ?PHASE_2_MIGRATION_COMPONENTS) of
        ok ->
            {next_state, enable_org_access, State, 0};
        {error, Error} ->
            stop_with_failure(State, Error, set_org_to_sql)
    end.

enable_org_access(timeout, #state{org_name = OrgName} = State) ->
    case mover_org_darklaunch:enable_org(OrgName) of
        ok ->
            {stop, normal, State};
        {error, Error} ->
            stop_with_failure(State, Error, enable_org_access)
    end.

handle_event(_Event, StateName, State) ->
    {next_state, StateName, State}.

handle_sync_event(_Event, _From, StateName, State) ->
    {reply, ok, StateName, State}.

handle_info(_Info, StateName, State) ->
    {next_state, StateName, State}.

terminate(normal, _StateName, #state{org_name = OrgName}) ->
    lager:info([{org_name, OrgName}], "Terminating after successful migration"),
    moser_state_tracker:migration_successful(OrgName);
terminate(_Other, StateName, #state{org_name = OrgName}) ->
    lager:info([{org_name, OrgName}], "Terminating after failed migration"),
    moser_state_tracker:migration_failed(OrgName, StateName).

code_change(_OldVsn, StateName, State, _Extra) ->
    {ok, StateName, State}.

%%
%% Internal
%%
stop_with_failure(#state{org_name = OrgName} = State, Error, LastState) ->
    lager:error([{org_name, OrgName}], "Migration halted in ~p due to error: ~p", [LastState, Error]),
    {stop, {error, Error, LastState}, State}.

stop_with_migration_error(#state{org_name = OrgName} = State, Error) ->
    lager:error([{org_name, OrgName}], "Error in converting org data, skipping:  ~p", [Error]),
    {stop, {migration_error, Error}, State}.
