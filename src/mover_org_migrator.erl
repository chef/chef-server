%% -*- erlang-indent-level: 4;indent-tabs-mode: nil;fill-column: 92 -*-
%% ex: ts=4 sw=4 et
%% @author Marc Paradise <marc@opscode.com>
%% @copyright 2013 Opscode, Inc.
%%
%% @doc a gen_fsm that migrates a single org from end to end.
%%


-module(mover_org_migrator).
-behaviour(gen_fsm).

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

-define(PHASE_2_MIGRATION_COMPONENTS,
            [checksums, clients, cookbooks, environments, roles, data]).

-define(ORG_META(OrgName), [{org_name, OrgName}]).

-record(state, { account_info :: term(), % TODO #account_info for moser, provided via init config
                 org_name :: string(),   % name of the org being migrated
                 start_time :: term(),   % timestamp indicating start of migration operation
                 error :: term()         % the error that has placed us in failure state.
               }).

start_link(Config) ->
    gen_fsm:start_link(?MODULE, Config, []).

init(Config) ->
    % TODO config is currently just org name, we may want to
    % factor up some of the account_info stuff out of moser.
    {ok, disable_org_access, #state{org_name = Config, start_time = os:timestamp()}, 0}.

disable_org_access(timeout, #state{org_name = OrgName} = State) ->
    case mover_org_darklaunch:disable_org(OrgName) of
        ok ->
            {next_state, migrate_org, State, 0};
        {error, Error} ->
            %lager:error(?ORG_META(OrgName), "Failed to place org into maintenance mode, skipping it: ~p", [Error]),
            {stop, {error, Error}, State}
    end.

migrate_org(timeout, #state{org_name = OrgName} = State) ->
    case moser_converter:convert_org(OrgName) of
        [{ok, _}] ->
            {next_state, verify_org, State, 0};
        {error, Error} ->
            %TODO lager:error(?ORG_META(OrgName), "Failed to migrate org: ~p", [Error]),
            io:fwrite("Failed to migrate org: ~p ~p~n", [OrgName, Error]),
            {next_state, abort_migration, #state{error = Error} = State, 0}
    end.

verify_org(timeout, #state{org_name = _OrgName} = State) ->
    % TODO do we have any validation we can run?
    % case moser_validator:validate_org(OrgName) of
    case ok of
        ok ->
            {next_state, set_org_to_sql, State, 0};
        {error, Error} ->
            %lager:error(?ORG_META(OrgName), "Failed to verify org: ~p", [Error]),
            {next_state, abort_migration, #state{error = Error} = State, 0}
    end.


set_org_to_sql(timeout, #state{org_name = OrgName} = State) ->
    case mover_org_darklaunch:org_to_sql(OrgName, ?PHASE_2_MIGRATION_COMPONENTS) of
        ok ->
            {next_state, enable_org_access, State, 0};
        {error, Error} ->
            %lager:error([{org_name, OrgName}],
            %    "Failed to update org darklaunch to sql: ~p", [Error]),
            {next_state, abort_migration, #state{error = Error} = State, 0}
    end.

enable_org_access(timeout, #state{org_name = OrgName} = State) ->
    case mover_org_darklaunch:enable_org(OrgName) of
        ok ->
            {next_state, complete_migration, State, 0};
        {error, Error} ->
            % Everything's done but we couldn't un-503 the org. Don't
            % undo the migration, it won't help. This needs
            % intervention...
            %lager:error([{org_name, OrgName}],
            %    "Failed to take org out of maint mode: ~p ~p", [Error]),
            {stop, {error, Error}, State}
    end.

complete_migration(timeout, #state{org_name = OrgName, start_time = Start} = State) ->
    % TODO update migration state
    Time = moser_utils:us_to_secs(timer:now_diff(os:timestamp(), Start)),
    % TODO lager misbehaving for now: lager:info(?ORG_META(OrgName), "Migration completed successfully in ~.3f seconds.", Time),
    %
    io:fwrite("Migration of ~p completed in ~.3f~n", [OrgName, Time]),
    {stop, normal, State}.

abort_migration(timeout, #state{org_name = OrgName, error = Error} = State) ->
    % TODO roll it back...
    %lager:warn(?ORG_META(OrgName), "Aborting migration."),
    io:fwrite("Failed to migrate org: ~p~n", [OrgName]),
    {stop, {shutdown, {error, Error}}, State}.

handle_event(_Event, StateName, State) ->
    {next_state, StateName, State}.

handle_sync_event(_Event, _From, StateName, State) ->
    {reply, ok, StateName, State}.

handle_info(_Info, StateName, State) ->
    {next_state, StateName, State}.

terminate(_Reason, _StateName, _State) ->
    ok.

code_change(_OldVsn, StateName, State, _Extra) ->
    {ok, StateName, State}.



