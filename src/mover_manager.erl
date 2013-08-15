%% -*- erlang-indent-level: 4;indent-tabs-mode: nil;fill-column: 92 -*-
%% ex: ts=4 sw=4 et
%% @author Marc A. Paradise <marc@opscode.com>
%% @copyright 2013 Opscode, Inc.
%%
%% @doc
%%
%% <em>States and Transitions</em>
%%
%% <ul>
%% <li> `ready -> working' : when the 'start' message is received.</li>
%% <li> `working -> halting'</li>
%% <ul>
%%     <li> no more orgs available to be processed </li>
%%     <li> fatal error encountered within a worker </li>
%%     <li> all requested orgs are currently processing </li>
%%     <li> a worker could not be started </li>
%% </ul>
%%
%% <li> `halting -> ready' : all workers have stopped </li>
%% </ul>
%%
%% <em>Worker Startup and Replacement</em>
%% <p>
%% Workers are evaluated when current state is `working'. If there are
%% fewer `live_workers' than `max_worker_count' and there are
%% still orgs left to process, another worker is started.
%% </p>
%%
%% <ul>
%% <li> `working' state is signalled every time a worker
%%      terminates with either a `normal' exit or a
%%      `migration_error' exit.
%% </li>
%% <li> `working' state is signalled every time a worker
%%      is started, so that the FSM can determine if more
%%      workers are needed.
%% </li>
%% </ul>
%%
%% <em>Additional Notes</em>
%% <ul>
%% <li>An org is considered as 'processed' (for status reporting) once a worker
%%     for that org has been successfully launched. Regardless of whether or not
%%     the migration succeeds, the persisted state of the org will be set to remove
%%     it from the pool of orgs available for migration.</li>
%%
%% <li>If at any point a worker goes down for a reason we haven't trapped,
%%     or for a specific {error, X} reason that we have trapped,
%%     we will stop replacing workers and allow all current workers to die out
%%     regardless of if there are orgs remaining to process.</li>
%% </ul>
%% @end

-module(mover_manager).

%% Comment this out for VIM syntax check/compiles to work.
-compile([{parse_transform, lager_transform}]).

% API Exports
-export([ start_link/0,
          migrate/2,
          migrate_next/0,
          validate_deps/2,
          validate_deps_next/0,
          status/0,
          halt_actions/0 ]).

%% gen_fsm callbacks
-export([ init/1,
          handle_event/3,
          handle_sync_event/4,
          handle_info/3,
          terminate/3,
          code_change/4 ]).
%% states
-export([ ready/3,
          working/2,
          working/3,
          halting/2,
          halting/3 ]).

-define(SERVER, ?MODULE).

-record(state, { %% Supervisor module that knows how to start workers
                 supervisor :: atom(),
                 %% Org name generator in moser_state_tracker
                 next_org_generator :: atom(),
                 %% Number of active workers
                 live_worker_count = 0 :: non_neg_integer(),
                 %% Max requested workers:
                 max_worker_count = 0   :: non_neg_integer(),
                 %% Number of orgs left to attempt to migrate,
                 %% or a number less than zero if all orgs are being migrated.
                 orgs_remaining = 0     :: integer(),
                 %% Total number of orgs processed:
                 orgs_done = 0          :: non_neg_integer(),
                 %% number of orgs requested in this migration, or -1 for all orgs:
                 orgs_requested = 0     :: -1 | non_neg_integer(),
                 %% How many non-fatal errors occurred during org migrations:
                 error_count = 0        :: non_neg_integer(),
                 %% if true, a worker encountered a fatal error that forced
                 %% us to stop spawning new migration workers.
                 fatal_stop = false     :: boolean(),
          %% moser account info as returned by `moser_acct_processor:open_account/0'
          acct_info
             }).
%%
%% API
%%

start_link() ->
    gen_fsm:start_link({local, ?SERVER}, ?MODULE, [], []).

migrate_next() ->
    migrate(1, 1).
validate_deps_next() ->
    validate_deps(1, 1).

migrate(all, NumWorkers) ->
    migrate(-1, NumWorkers);
migrate(NumOrgs, NumWorkers) ->
    gen_fsm:sync_send_event(?SERVER, {start, NumOrgs, NumWorkers,
                                      mover_org_migrator_sup,
                                      next_ready_org}).

validate_deps(all, NumWorkers) ->
    validate_deps(-1, NumWorkers);
validate_deps(NumOrgs, NumWorkers) ->
    gen_fsm:sync_send_event(?SERVER, {start, NumOrgs, NumWorkers,
                                      mover_org_dep_validator_sup,
                                      next_validation_ready_org}).

status() ->
    gen_fsm:sync_send_all_state_event(?SERVER, status).

halt_actions() ->
    gen_fsm:sync_send_all_state_event(?SERVER, halt).

init([]) ->
    AcctInfo = moser_acct_processor:open_account_ro(),
    {ok, ready, #state{acct_info = AcctInfo}}.

%%
%% States
%%

%% @doc `ready' state can be reached in one of two ways. In both methods, there
%% is no timeout - so the ready state function will only be invoked when
%% an explicit call is made to start migrations via `migrate/2' or `migrate_next/0'
%%
%% <ul>
%% <li>`init/1' completion</li>
%% <li>A migration has been started and all workers have terminated. In this case
%%     we have preserved FSM state until the next explicit `{start, Orgs, Workers}'
%%     is issued, so that results of the last batch can be queried via `status/0'</li>
%% </ul>
ready({start, NumOrgs, NumWorkers, Supervisor, OrgGenerator}, _From, CurrentState) ->

    %% Do not carry forward existing state - create a new
    %% state record to reflect the new attempted migrations.
    State = CurrentState#state{max_worker_count = NumWorkers,
                   orgs_requested = NumOrgs,
                   orgs_remaining = NumOrgs,
                   next_org_generator = OrgGenerator,
                   supervisor = Supervisor},
    {reply, {ok, burning_couches}, working, State, 0}.

working(timeout, #state{orgs_remaining = 0} = State) ->
    %% All orgs requested have been started. Move to
    %% halting until all workers complete their tasks.
    {next_state, halting, State, 0};
working(timeout, #state {max_worker_count = Count,
                         live_worker_count = Count} = State) ->
    %% We have as many workers as we want. Our next action occurs when
    %% a worker finishes or user requests a halt.
    {next_state, working, State};
working(timeout, #state {max_worker_count = MW,
                         live_worker_count = LW,
                         next_org_generator = Next,
                         supervisor = SupMod} = State) when LW < MW ->
    %% We have fewer workers than requested, start another one
    %% if there are still orgs available for migrating.

    %% TODO alternatively, we could just capture the correct fun to use
    %% to generate the next name directly - I'm not sure that _sup is
    %% the right place for this function.
    case moser_state_tracker:Next() of
        {ok, no_more_orgs} ->
            %% Stop and wait for workers to end.
            {next_state, halting, State#state {orgs_remaining = 0}, 0};
        [OrgName] ->
            start_org_worker(SupMod, OrgName, State);
        OrgName ->
            start_org_worker(SupMod, OrgName, State)
    end.

working({start, _, _, _, _}, _From, State) ->
    {reply, {error, busy_now}, halting, State}.


start_org_worker(SupMod, OrgName, #state{live_worker_count = LW,
                                         orgs_remaining = OR,
                                         acct_info = AcctInfo} = State) ->
    case SupMod:start_worker({OrgName, AcctInfo}) of
        {ok, Pid} ->
            monitor(process, Pid),
            {next_state, working, State#state{live_worker_count = (LW + 1),
                                              orgs_remaining = (OR - 1)}, 0};
        _AnythingElse ->
            lager:error("Failed to start worker for org ~p - halting remaining workers!", [OrgName]),
            {next_state, halting, State#state {fatal_stop = true}, 0}
    end.

halting(timeout, #state{live_worker_count = 0} = State) ->
    %% All workers stopped - we're ready to accept a new request for
    %% migrations.  Hold onto State so that results can be queried via status/0
    {next_state, ready, State};
halting(timeout, #state{} = State) ->
    %% Workers remainin, we stay 'halting' until there are none.
    {next_state, halting, State}.

halting({start, _, _, _, _}, _From, State) ->
    {reply, {error, halting_operations}, halting, State}.

handle_info({'DOWN', _MRef, process, _Pid, Reason}, StateName,
            #state{live_worker_count = LW, orgs_done = OD} = State) ->
    worker_down(Reason, StateName, State#state{live_worker_count = LW - 1,
                                               orgs_done = OD + 1});
handle_info(_Info, StateName, State) ->
    {next_state, StateName, State}.

worker_down({migration_error, _Detail}, StateName, #state{error_count = EC} = State) ->
    {next_state, StateName, State#state{error_count = (EC + 1)}, 0};
worker_down(normal, StateName, State) ->
    {next_state, StateName, State, 0};
worker_down(Reason, StateName, #state{supervisor = mover_org_dep_validator_sup,
                                      error_count = EC} = State) ->
    %% Errors in validation are not fatal to running additional
    %% validations, so we won't halt operations
    lager:error("in ~p: Worker down with unexpected error while validating.  Continuing validations. ~p",
                [StateName, Reason]),
    {next_state, StateName, State#state{error_count = (EC + 1)}, 0};
worker_down(Reason, StateName, State) ->
    lager:error("in ~p: Worker down with unexpected error. Halting new workers. ~p", [StateName, Reason]),
    {next_state, halting, State#state{fatal_stop = true}, 0}.

handle_event(_Event, StateName, State) ->
    {next_state, StateName, State}.

handle_sync_event(halt, _From, working, State) ->
    {reply, {ok, halting}, halting, State, 0};
handle_sync_event(halt, _From, StateName, State) ->
    {reply, {error, not_now_dear}, StateName, State};
handle_sync_event(status, _From, StateName, #state{live_worker_count = LW,
                                                  orgs_done = OD,
                                                  orgs_remaining = OR,
                                                  orgs_requested = OReq,
                                                  fatal_stop = FS,
                                                  error_count = EC} = State) ->
    Summary = [{state, StateName},
               {orgs_completed, OD},
               {orgs_remaining, normalize_org_count(OR)},
               {orgs_requested, normalize_org_count(OReq)},
               {orgs_in_progress, LW},
               {orgs_failed, EC},
               {fatal_stop, FS}],
    {reply, {ok, Summary}, StateName, State};
handle_sync_event(_Event, _From, StateName, State) -> {next_state, StateName, State}.

normalize_org_count(Count) when Count < 0 ->
    all;
normalize_org_count(Count) ->
    Count.

terminate(_Reason, _StateName, _State) ->
    ok.

code_change(_OldVsn, StateName, State, _Extra) ->
    {ok, StateName, State}.

