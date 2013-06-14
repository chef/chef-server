-module(mover_chef_couch_removal_worker).

-behaviour(gen_fsm).

%% Comment this out for VIM syntax check/compiles to work.
-compile([{parse_transform, lager_transform}]).

%% API
-export([
    start_link/0,
    purge_orgs/0,
    purge_orgs/1,
    purge_org/1,
    pause/0,
    resume/0,
    cancel/0
  ]).

%% FSM states
-export([
    ready/2,
    purging/2,
    paused/2,
    purge_one/2
  ]).
%% gen_fsm callbacks
-export([init/1,
    handle_event/3,
    handle_sync_event/4,
    handle_info/3,
    terminate/3,
    code_change/4]).

-define(SERVER, ?MODULE).
-define(DEFAULT_ORG_MAXIMUM, 100000).

-record(state, {org_names_fun = [], counter = 1, total_count = 0}).

start_link() ->
    gen_fsm:start_link({local, ?SERVER}, ?MODULE, [], []).

purge_orgs() ->
	purge_orgs(?DEFAULT_ORG_MAXIMUM).

purge_orgs(Maximum) ->
	gen_fsm:send_event(?SERVER,
    {start_purge,
      fun() ->
          moser_state_tracker:next_purge_ready_org()
      end,
      Maximum}).

purge_org(Org) ->
  gen_fsm:send_event(?SERVER, {start_purge, Org}).

pause() ->
  gen_fsm:send_event(?SERVER, pause).

cancel() ->
  gen_fsm:send_event(?SERVER, cancel).

resume() ->
  gen_fsm:send_event(?SERVER, continue).

init([]) ->
    {ok, ready, #state{}}.
%%event
ready({start_purge, OrgNameFun, Maximum}, State) ->
    gen_fsm:send_event(self(), purge),
    {next_state, purging, State#state{org_names_fun = OrgNameFun, total_count = Maximum}};
ready({start_purge, Org}, State) ->
    gen_fsm:send_event(self(), {purge_one, Org}),
    {next_state, purge_one, State};
ready(_Event, State) ->
    {next_state, ready, State}.

purge_one({purge_one, Org}, State) ->
    ok = moser_state_tracker:purge_started(Org),
    {Time, _Result} = timer:tc(fun() -> moser_chef_couch_removal:delete_org(Org) end),
    ok = moser_state_tracker:purge_successful(Org),
    lager:info("[1/1] purged in ~p milliseconds ~p", [Time div 1000, Org]),
    lager:info("Finished purging 1 Org", []),
    {next_state, ready, State}.

purging(purge, #state{counter = TotalCount, total_count = TotalCount}) ->
    lager:info("Finished purging ~p", [TotalCount]),
    {next_state, ready, #state{}};

purging(purge, State = #state{org_names_fun = OrgNameFun, counter = Counter, total_count = TotalCount}) ->
		NewState = case OrgNameFun() of
			OrgName when is_list(OrgName) orelse is_binary(OrgName) ->
        lager:info("Starting purge of ~p", [OrgName]),
				do_purge(OrgName, Counter, TotalCount),
				State#state{counter = Counter + 1};
			_ ->
				State#state{counter = TotalCount}
		end,
    gen_fsm:send_event(self(), purge),
    {next_state, purging, NewState};
purging(cancel, #state{org_names_fun = Orgs, counter = _Counter}) ->
    lager:info("purging cancelled, remaining orgs ~p", [Orgs]),
    {next_state, ready, #state{}};
purging(pause, State) ->
    lager:info("purging paused", []),
    {next_state, paused, State}.

paused(cancel, _State) ->
    lager:info("purging cancelled", []),
    {next_state, ready, #state{}};
paused(continue, State) ->
    lager:info("purging continued", []),
    gen_fsm:send_event(self(), purge),
    {next_state, purging, State};
paused(_Event, State) ->
    {next_state, paused, State}.

handle_event(_Event, StateName, State) ->
    {next_state, StateName, State}.

handle_sync_event(_Event, _From, StateName, State) ->
    Reply = ok,
    {reply, Reply, StateName, State}.

handle_info(_Info, StateName, State) ->
    {next_state, StateName, State}.

terminate(_Reason, _StateName, _State) ->
    ok.

code_change(_OldVsn, StateName, State, _Extra) ->
    {ok, StateName, State}.

do_purge(Org, Counter, TotalCount) ->
		ok = moser_state_tracker:purge_started(Org),
    %% put org in purging state
		{Time, ok} = timer:tc(fun() -> moser_chef_couch_removal:delete_org(Org) end),
    lager:info("[~p/~p] purged in ~p milliseconds ~p", [Counter, TotalCount, Time div 1000, Org]),
    %% put org in deleted state
		ok = moser_state_tracker:purge_successful(Org).
