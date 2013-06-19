%% -*- erlang-indent-level: 4;indent-tabs-mode: nil;fill-column: 92 -*-
%% ex: ts=4 sw=4 et
%% @author Marc Paradise <marc@opscode.com>
%% @copyright 2013 Opscode, Inc.
%%
%% @doc a gen_fsm that runs validation of a single org's cookbook dependencies
%% from end to end.

-module(mover_org_dep_validator).

-record(state, {
                 depscan_results :: term(), %% first pass validation results
                 depwalk_results :: term(),
                 org_name :: string()     %% The org we are validating
               }).

%% Comment this out for VIM syntax check/compiles to work.
-compile([{parse_transform, lager_transform}]).
-export([init/1,
         handle_event/3,
         handle_sync_event/4,
         handle_info/3,
         terminate/3,
         code_change/4]).


%% gen_fsm callbacks


%% API
-export([start_link/1]).


%% states
-export([depscan/2,
         depwalk/2,
         complete_validation/2
        ]).

-include("mover.hrl").

start_link(Config) ->
    gen_fsm:start_link(?MODULE, Config, []).

init(OrgName) ->
    State = #state{org_name = OrgName },
    case moser_state_tracker:validation_started(OrgName) of
        ok ->
            lager:info([{org_name, OrgName}], "Starting dep validation."),
            {ok, depscan, State, 0};
        Error ->
            stop_with_failure(State, Error, init)
    end.

depscan(timeout, #state{org_name = OrgName} = State) ->
    % For now, discarding detail data. We can look into capturing this
    % or other usage.
    moser_depscanner:org_missing_deps(OrgName),
    R = moser_depscanner:unique_missing_deps_for_org(OrgName),
    {next_state, depwalk, State#state{depscan_results = R}, 0}.


depwalk(timeout, #state{org_name = OrgName} = State) ->
    % For now discarding detail data. We can look into capturing based
    % on results.
    moser_depwalker:eval_node_deps_for_org(OrgName),
    R = moser_depwalker:org_totals(OrgName),
    {next_state, complete_validation, State#state{depwalk_results = R}, 0}.

complete_validation(timeout, State) ->
    {stop, normal, State}.

handle_event(_Event, StateName, State) ->
    {next_state, StateName, State}.

handle_sync_event(_Event, _From, StateName, State) ->
    {reply, ok, StateName, State}.

handle_info(_Info, StateName, State) ->
    {next_state, StateName, State}.

terminate(normal, _StateName, #state{org_name = OrgName,
                                     depwalk_results = DWR,
                                     depscan_results = DSR }) ->
    lager:info([{org_name, OrgName}],
               "Terminating after successful validation ~p", [OrgName]),
    Results = [ {depwalk_results, DWR}, {depscan_results, DSR} ],
    moser_state_tracker:validation_completed(OrgName, Results);
terminate(_Other, _StateName, #state{org_name = OrgName}) ->
    lager:info([{org_name, OrgName}],
                "Unexpected error while attempting validate ~p", [OrgName]),
    moser_state_tracker:validation_failed(OrgName, _Other).

code_change(_OldVsn, StateName, State, _Extra) ->
    {ok, StateName, State}.

%%
%% Internal
%%
stop_with_failure(#state{org_name = OrgName} = State, Error, LastState) ->
    lager:error([{org_name, OrgName}], "Validation halted in ~p due to error: ~p", [LastState, Error]),
    {stop, {error, Error, LastState}, State}.
