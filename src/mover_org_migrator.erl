%% -*- erlang-indent-level: 4;indent-tabs-mode: nil;fill-column: 92 -*-
%% ex: ts=4 sw=4 et
%% @author Marc Paradise <marc@opscode.com>
%% @copyright 2013 Opscode, Inc.
%%
%% @doc a gen_fsm that migrates a single org from end to end.
%%

-module(mover_org_migrator).
-behaviour(gen_fsm).

-compile([{parse_transform, lager_transform}]).

% gen_fsm callbacks
-export([init/1,
         handle_event/3,
         handle_sync_event/4,
         handle_info/3,
         terminate/3,
         code_change/4]).

% states
-export([disable_org_access/2,
         migrate_org/2,
         verify_org/2,
         set_org_to_sql/2,
         enable_org_access/2,
         complete_migration/2,
         abort_migration/2 ]).

% api
-export([start_link/1]).

-include_lib("moser/include/moser_state.hrl").

-define(PHASE_2_MIGRATION_COMPONENTS,
            [checksums, cookbooks, environments, roles, data]).

-record(state, { account_info :: term(),   % TODO #account_info for moser, provided via init config
                 ms :: #migration_state{}, % migration state persistance record
                 start_time :: term(),     % timestamp indicating start of migration operation
                 error :: term()           % the error that has placed us in failure state.
               }).

start_link(Config) ->
    gen_fsm:start_link(?MODULE, Config, []).

init(#migration_state{} = MS) ->
    State = #state{ms = MS, start_time = os:timestamp()},
    State1 = persist_state(State, inflight, init),
    {ok, disable_org_access, State1, 0}.

disable_org_access(timeout, State) ->
    State1 = persist_state(State, inflight, disable_org_access),
    case mover_org_darklaunch:disable_org(State1#state.ms) of
        {ok, MS} ->
            {next_state, migrate_org, State1#state{ms = MS}, 0};
        {{error, Error}, MS} ->
            State2 = persist_state(State1#state{ms = MS}, failure, disable_org_access),
            {stop, {error, Error}, State2}
    end.

migrate_org(timeout, State) ->
    State1 = persist_state(State, inflight, migrate_org),
    case moser_converter:convert_org(State1#state.ms) of
        [{ok, _}] ->
            {next_state, verify_org, State1, 0};
        {error, Error} ->
            State2 = persist_state(State1, inflight, migrate_org, Error),
            {next_state, abort_migration, State2, 0}
    end.

verify_org(timeout, State) ->
    State1 = persist_state(State, inflight, verify_org),
    % TODO do we have any validation we can run?
    % case moser_validator:validate_org(MS) of
    case ok of
        ok ->
            {next_state, set_org_to_sql, State1, 0};
        {error, Error} ->
            State2 = persist_state(State1, failed, verify_org, Error),
            {next_state, abort_migration, State2, 0}
    end.

set_org_to_sql(timeout, State) ->
    State1 = persist_state(State, inflight, set_org_to_sql),
    case mover_org_darklaunch:org_to_sql(State1#state.ms, ?PHASE_2_MIGRATION_COMPONENTS) of
        {ok, MS} ->
            {next_state, enable_org_access, State1#state{ms = MS}, 0};
        {{error, Error}, MS} ->
            State2 = persist_state(State1#state{ms = MS}, failed, set_org_to_sql, Error),
            {next_state, abort_migration, State2, 0}
    end.

enable_org_access(timeout, State) ->
    State1 = persist_state(State, inflight, enable_org_access),
    case mover_org_darklaunch:enable_org(State1#state.ms) of
        {ok, MS} ->
            {next_state, complete_migration, State1#state{ms = MS}, 0};
        {{error, Error}, MS} ->
            State2 = persist_state(State1#state{ms = MS}, failed, enable_org_access, Error),
            {stop, {error, Error}, State2}
    end.

complete_migration(timeout, State) ->
    State1 = persist_state(State, complete, complete_migration),
    {stop, normal, State1}.

abort_migration(timeout, #state{error = Error} = State) ->
    % Do NOT persist state here - we would overwrite the failure detail
    % previously captured.
    {stop, {shutdown, {error, Error}}, State}.

handle_event(_Event, StateName, State) ->
    io:fwrite("GOT handle_event ~p!", [StateName]),
    {next_state, StateName, State}.

handle_sync_event(_Event, _From, StateName, State) ->
    io:fwrite("GOT handle_sync_event ~p!", [StateName]),
    {reply, ok, StateName, State}.

handle_info(_Info, StateName, State) ->
    io:fwrite("GOT handle_info~p!", [StateName]),
    {next_state, StateName, State}.

terminate(_Reason, _StateName, _State) ->
    ok.

code_change(_OldVsn, StateName, State, _Extra) ->
    {ok, StateName, State}.


%% Internal
%%
persist_state(MS, OrgState, Stage) ->
    persist_state(MS, OrgState, Stage, undefined).

persist_state(#state{start_time = Start, ms = MS} = State, OrgState, Stage, Error) ->
    MS2 = MS#migration_state{
                state = OrgState,
                last_step = Stage,
                migration_duration = timer:now_diff(os:timestamp(), Start)},
    log_state_result(MS2, Error),
    {_, MS3} = moser_state_tracker:update_state(MS2),
    State#state{ms = MS3, error = Error}.

log_state_result(#migration_state{ state = failure, last_step = Stage } = MS, Error) ->
    lager:error(?MS_LOG_META(MS),
                "mover_org_migrator failed in state ~p with error ~p",
                [Stage, Error]);
log_state_result(#migration_state{ last_step = Stage } = MS, _Error ) ->
    lager:info(?MS_LOG_META(MS),
               "mover_org_migrator in progress, migration stage: ~p",
               [Stage]).

