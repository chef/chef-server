-module(mover_reindex_dry_run_revert_migration_callback).

-behaviour(gen_server).

-export([
         migration_init/0,
         migration_complete/0,
         migration_started/2,
         migration_successful/2,
         migration_failed/3,
         migration_start_worker_args/2,
         migration_action/1,
         migration_type/0,
         supervisor/0,
         error_halts_migration/0,
         reconfigure_object/1,
         next_object/0,
         disable_object/1
         ]).

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-include_lib("moser/include/moser.hrl").

migration_init() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

migration_complete() ->
    gen_server:cast(?MODULE, stop).

migration_started(_, _) ->
    ok.

migration_successful(_, _) ->
    ok.

migration_failed(_, _, _) ->
    ok.

next_object() ->
    gen_server:call(?MODULE, next).
%Return any initial state not related to the object id
migration_start_worker_args(Object, _AcctInfo) ->
    [Object].

migration_action(OrgName) ->
    moser_state_tracker:force_org_to_state(OrgName, mover_reindex_dry_run_migration_callback:migration_type(), <<"ready">>),
    [{ok, reindex_revert_successful}].

migration_type() ->
    <<"solr4_migration_dry_run">>.

supervisor() ->
    mover_org_migrator_sup.

error_halts_migration() ->
    true.

reconfigure_object(OrgName) ->
    mover_org_darklaunch:enable_solr1(OrgName).

disable_object(_) ->
    ok.

init(_Args) ->
    {ok, moser_state_tracker:migrated_orgs(mover_reindex_dry_run_migration_callback:migration_type())}.
handle_call(next, _From, []) ->
    {reply, {ok, no_more_orgs}, []};
handle_call(next, _From, [Next | State]) ->
    {reply, Next, State};
handle_call(stop, _From, State) ->
    {stop, normal, State};
handle_call(_Message, _From, State) ->
    {reply, ok, State}.
handle_cast(stop, State) ->
    {stop, normal, State};
handle_cast(_Message, State) ->
    {noreply, State}.
handle_info(_Message, State) ->
    {noreply, State}.
terminate(_Reason, _State) ->
    ok.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
