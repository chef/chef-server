%% -*- erlang-indent-level: 4;indent-tabs-mode: nil;fill-column: 92 -*-
%% ex: ts=4 sw=4 et
%% @author Marc A. Paradise <marc@chef.io>
%% @copyright 2013-2018 Chef Software, Inc.
%%
%% @doc
%%
%% <em>States and Transitions</em>
%%
%% <ul>
%% <li> `ready -> working' : when the 'start' message is received.</li>
%% <li> `working -> halting'</li>
%% <ul>
%%     <li> no more objects available to be processed </li>
%%     <li> fatal error encountered within a worker </li>
%%     <li> a worker could not be started </li>
%% </ul>
%%
%% <li> `halting -> ready' : all workers have stopped </li>
%% </ul>
%%
%% <em>Worker Startup and Replacement</em>
%% <p>
%% fewer `live_workers' than `max_worker_count' and there are
%% Workers are evaluated when current state is `working'. If there are
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

% API Exports
-export([ ping/0,
          start_link/0,
          set_concurrency/1,
          migrate/3,
          migrate_next/1,
          migrate_user_password_storage/2,
          status/0,
          halt_actions/0,
          create_account_dets/1,
          get_account_dets/0]).

-ifdef(TEST).
-export([
         stop/0,
         set_state/1,
         get_state/0
         ]).
-endif.

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

-record(migration_worker, {
                            %% Callback module for a given migration
                            callback_module :: atom()}).

-record(state, { %% Number of active workers
                 live_worker_count = 0  :: non_neg_integer(),
                 %% Max requested workers:
                 max_worker_count = 0   :: non_neg_integer(),
                 %% Number of objects left to attempt to migrate,
                 %% or a number less than zero if all orgs are being migrated.
                 objects_remaining = 0  :: integer(),
                 %% Total number of objects processed:
                 objects_done = 0       :: non_neg_integer(),
                 %% number of orgs requested in this migration, or -1 for all orgs:
                 objects_requested = 0  :: -1 | non_neg_integer(),
                 %% How many non-fatal errors occurred during org migrations:
                 error_count = 0        :: non_neg_integer(),
                 %% if true, a worker encountered a fatal error that forced
                 %% us to stop spawning new migration workers.
                 fatal_stop = false     :: boolean(),
                 %% moser account info as returned by `moser_acct_processor:open_account/0'
                 acct_info              :: term(),
                 %% handler for thte migration we're attempting
                 worker                 :: #migration_worker{}

             }).
%%
%% API
%%

ping() ->
    pong.

start_link() ->
    gen_fsm:start_link({local, ?SERVER}, ?MODULE, [], []).
-ifdef(TEST).
stop() ->
    gen_fsm:sync_send_all_state_event(?SERVER, stop).
set_state(StateName) ->
    gen_fsm:sync_send_all_state_event(?SERVER, {set_state, StateName}).
get_state() ->
    gen_fsm:sync_send_all_state_event(?SERVER, get_state).
-endif.

migrate_next(CallbackModule) ->
    migrate(1, 1, CallbackModule).

migrate(all, NumWorkers, CallbackModule) ->
    migrate(-1, NumWorkers, CallbackModule);
migrate(NumOrgs, NumWorkers, CallbackModule) ->
    Worker = #migration_worker{callback_module = CallbackModule},
    % give 10 second timeout since loading migration queues can take awhile
    gen_fsm:sync_send_event(?SERVER, {start, NumOrgs, NumWorkers, Worker}, 120000).

%% Migrate user password hash from sha1 embedded in json to dedicated fields
%% using bcrypt.
migrate_user_password_storage(all, NumWorkers) ->
    migrate_user_password_storage(-1, NumWorkers);
migrate_user_password_storage(NumUsers, NumWorkers) ->
    Worker = #migration_worker{
                               callback_module = mover_user_hash_converter_callback},
    gen_fsm:sync_send_event(?SERVER, {start, NumUsers, NumWorkers, Worker}).

%% Ensure nothing is active - that indicates it's safe to reset
%% the transient queue, even if it has contents left from a previous run.

status() ->
    gen_fsm:sync_send_all_state_event(?SERVER, status, 30000).


set_concurrency(NewConcurrency) when is_integer(NewConcurrency) and (NewConcurrency > 0) ->
    gen_fsm:sync_send_all_state_event(?SERVER, {set_concurrency, NewConcurrency});
set_concurrency(BadValue) ->
    lager:error("Concurrency must be numeric and greater than 0."),
    {error, {badarg, BadValue}}.

halt_actions() ->
    gen_fsm:sync_send_all_state_event(?SERVER, halt).

create_account_dets(CallbackModule) ->
    gen_fsm:sync_send_event(?SERVER, {create_account_dets, CallbackModule}, infinity).

get_account_dets() ->
    gen_fsm:sync_send_all_state_event(?SERVER, get_account_dets).

init([]) ->
    AcctInfo = try moser_acct_processor:open_account() of
                   Result ->
                       Result
               catch
                   error:{badmatch, {error, {file_error, _, enoent}}} ->
                       undefined;

                   error:{badmatch, {error, {not_closed, _}}} ->
                       undefined
               end,
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
ready({start, NumObjects, NumWorkers, Worker}, _From, CurrentState) ->
    %% Do not carry forward existing state - create a new
    %% state record to reflect the new attempted migrations.
    State = CurrentState#state{max_worker_count = NumWorkers,
                   objects_requested = NumObjects,
                   objects_remaining = NumObjects,
                   worker = #migration_worker{callback_module = CallbackMod } = Worker},
    call_if_exported(CallbackMod, migration_init, [], fun no_op/0),
    {reply, {ok, burning_couches}, working, State, 0};
ready({create_account_dets, CallbackMod}, _From, State = #state{ acct_info = Acct } ) ->
    NewAccount = case call_if_exported(CallbackMod, needs_account_dets, [], fun always_false/0) of
       true -> create_dets_files(Acct);
       false -> account_dets_not_opened
    end,
    {reply, NewAccount, ready, State#state{acct_info = NewAccount}}.

working(timeout, #state{objects_remaining = 0} = State) ->
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
                         worker = #migration_worker{callback_module = Mod}}
                         = State) when LW < MW ->
    Next = fun() -> moser_state_tracker:next_ready_org(Mod:migration_type()) end,
    %% We have fewer workers than requested, start another one
    %% asl ong as there is work to do
    case call_if_exported(Mod, next_object, [], Next) of
        {ok, no_more_orgs} -> % no more orgs to migrate
            {next_state, halting, State#state {objects_remaining = 0}, 0};
        {ok, no_more} ->      % no more anything else to do
            %% Stop and wait for workers to end.
            {next_state, halting, State#state {objects_remaining = 0}, 0};
        [Object] ->
            start_worker(Object, State);
        Object ->
            start_worker(Object, State)
    end.

working({start, _, _, _}, _From, State) ->
    {reply, {error, busy_now}, halting, State}.


start_worker(Object, #state{live_worker_count = LW,
                                     objects_remaining = OR,
                                     acct_info = AcctInfo,
                                     worker = #migration_worker{callback_module = Mod}} = State) ->
    MigrationArgs = call_if_exported(Mod,migration_start_worker_args,[Object, AcctInfo],  fun default_worker_args/2),
    case apply(Mod:supervisor(), start_worker, [Mod, Object, MigrationArgs]) of
        {ok, Pid} ->
            monitor(process, Pid),
            {next_state, working, State#state{live_worker_count = (LW + 1),
                                              objects_remaining = (OR - 1)}, 0};
        Error ->
            lager:error("Failed to start worker for ~p - halting remaining workers! Error: ~p", [Object, Error]),
            {next_state, halting, State#state {fatal_stop = true}, 0}
    end.

default_worker_args(Object, AcctInfo) ->
    [Object, AcctInfo].

halting(timeout, #state{live_worker_count = 0,worker = #migration_worker { callback_module = Mod }} = State) ->
    %% All workers stopped - we're ready to accept a new request for
    %% migrations.  Hold onto State so that results can be queried via status/0
    mover_util:call_if_exported(Mod, migration_complete, [], fun no_op/0),
    {next_state, ready, State};
halting(timeout, #state{} = State) ->
    %% Workers remainin, we stay 'halting' until there are none.
    {next_state, halting, State}.

halting({start, _, _, _}, _From, State) ->
    {reply, {error, halting_operations}, halting, State}.

handle_info({'DOWN', _MRef, process, _Pid, Reason}, StateName,
            #state{live_worker_count = LW, objects_done = OD} = State) ->
    worker_down(Reason, StateName, State#state{live_worker_count = LW - 1,
                                               objects_done = OD + 1});
handle_info(_Info, StateName, State) ->
    {next_state, StateName, State}.

worker_down({migration_error, _Detail}, StateName, #state{error_count = EC} = State) ->
    {next_state, StateName, State#state{error_count = (EC + 1)}, 0};
worker_down(normal, StateName, State) ->
    {next_state, StateName, State, 0};
worker_down(Reason, StateName, #state{ worker = #migration_worker { callback_module = Mod },
                                       error_count = EC} = State) ->
    case Mod:error_halts_migration() of
        false ->
            lager:error("in ~p: Worker down with unexpected error while validating.  Continuing validations. ~p",
                        [StateName, Reason]),
            {next_state, StateName, State#state{error_count = (EC + 1)}, 0};
        true ->
            lager:error("in ~p: Worker down with unexpected error. Halting new workers. ~p", [StateName, Reason]),
            {next_state, halting, State#state{fatal_stop = true}, 0}
    end.

handle_event(_Event, StateName, State) ->
    {next_state, StateName, State}.

handle_sync_event(halt, _From, working, State) ->
    {reply, {ok, halting}, halting, State, 0};
handle_sync_event(halt, _From, StateName, State) ->
    {reply, {error, not_now_dear}, StateName, State};
handle_sync_event(stop, _From, _StateName, State) ->
    {stop, normal, ok, State};
handle_sync_event({set_state, NewState}, _From, _StateName, State) ->
    {reply, ok, NewState, State};
handle_sync_event(get_state, _From, StateName, State) ->
    {reply, StateName, StateName, State};
handle_sync_event(status, _From, StateName, #state{live_worker_count = LW,
                                                  objects_done = OD,
                                                  objects_remaining = OR,
                                                  objects_requested = OReq,
                                                  fatal_stop = FS,
                                                  error_count = EC} = State) ->
    Summary = [{state, StateName},
               {orgs_completed, OD},
               {objects_remaining, normalize_object_count(OR)},
               {objects_requested, normalize_object_count(OReq)},
               {objects_in_progress, LW},
               {objects_failed, EC},
               {fatal_stop, FS}],

    %% Fixed a race condition that prevents the gen_fsm from transitioning from the current
    %% state to the next state when looping over status.
    %%
    %% The zero timeout prevents a status call from clearing the gen_fsm timeout, and the
    %% state transition is dependant on a timeout to occur.
    %%
    %% Except the transition FROM `ready`. `ready` does not have a timeout handler, so a zero
    %% timeout would not have the desired effect

    case StateName of
        ready ->
            {reply, {ok, Summary}, StateName, State};
        _ ->
            {reply, {ok, Summary}, StateName, State, 0}
    end;

handle_sync_event({set_concurrency, Value}, _From, working, #state{max_worker_count = Value} = State) ->
    {reply, {ok, no_change}, working, State};
handle_sync_event({set_concurrency, Value}, _From, working, #state{max_worker_count = OldValue} = State) ->
    {reply, {ok, modified, OldValue}, working, State#state{max_worker_count = Value} };
handle_sync_event({set_concurrency, _}, _From, StateName, State) ->
    {reply, {error, bad_state}, StateName, State};
handle_sync_event(get_account_dets, _From, StateName, State = #state{ acct_info = AcctInfo }) ->
    {reply, AcctInfo, StateName, State};
handle_sync_event(_Event, _From, StateName, State) -> {next_state, StateName, State}.

normalize_object_count(Count) when Count < 0 ->
    all;
normalize_object_count(Count) ->
    Count.

terminate(_Reason, _StateName, _State) ->
    ok.

code_change(_OldVsn, StateName, State, _Extra) ->
    {ok, StateName, State}.

create_dets_files(undefined) ->
    moser_acct_processor:process_account_file(),
    moser_acct_processor:open_account();
create_dets_files(Acct) ->
    moser_acct_processor:close_account(Acct),
    create_dets_files(undefined).

call_if_exported(#state{worker = Worker}, FunName, Args, DefaultFun) ->
    call_if_exported(Worker, FunName, Args, DefaultFun);
call_if_exported(#migration_worker{callback_module = Mod}, FunName, Args, DefaultFun) ->
    call_if_exported(Mod, FunName, Args, DefaultFun);
call_if_exported(Mod, FunName, Args, DefaultFun) ->
    mover_util:call_if_exported(Mod, FunName, Args, DefaultFun).

no_op() ->
    no_op.

always_false() ->
    false.
