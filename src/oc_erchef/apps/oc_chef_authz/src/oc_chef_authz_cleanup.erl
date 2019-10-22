%% -*- erlang-indent-level: 4;indent-tabs-mode: nil; fill-column: 92 -*-
%% ex: ts=4 sw=4 et
%%%-------------------------------------------------------------------
%%% @author Oliver Ferrigni <>
%%% @doc gen_fsm responsible for cleaning up orphaned authz_ids.  These
%%% authz ids are detected in oc_chef_group and added to a set of either
%%% actor or group authz_ids.  On a timer, the authz_ids are deleted
%%% from authz.
%%%
%%% @end
%%% Created :  6 Nov 2013 by Oliver Ferrigni <>
%%%-------------------------------------------------------------------
%% Copyright 2013 Opscode, Inc. All Rights Reserved.
%%
%% This file is provided to you under the Apache License,
%% Version 2.0 (the "License"); you may not use this file
%% except in compliance with the License.  You may obtain
%% a copy of the License at
%%
%%   http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing,
%% software distributed under the License is distributed on an
%% "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
%% KIND, either express or implied.  See the License for the
%% specific language governing permissions and limitations
%% under the License.

-module(oc_chef_authz_cleanup).

-behaviour(gen_statem).

%% API
-export([
         start_link/0,
         add_authz_ids/2,
         get_authz_ids/0,
         start/0,
         stop/0,
         prune/0,
         %prune/3,

         addx/3,
         startx/3,
         stopx/3,
         prunex/3,
         get_authz_idsx/3
        ]).

%% gen_fsm callbacks
%% CHANGE FOR gen_statem:
%%  -export([init/1, callback_mode/0, terminate/3, code_change/4]).
%%  Add callback__mode/0
%%  Change arity of the state functions
%%  Remove handle_info/3
-export([
         init/1,
         callback_mode/0,
         handle_info/3,
%        handle_sync_event/4,
%        handle_event/4,
         terminate/3,
         code_change/4]).

%% FSM states
-export([
         stopped/2,
         started/3
        ]).

-define(SERVER, ?MODULE).

-define(DEFAULT_BATCH_SIZE, 2500).
-define(DEFAULT_INTERVAL, 1000).

-include("oc_chef_authz.hrl").
-include("oc_chef_authz_cleanup.hrl").


%%--------------------------------------------------------------------
%% @doc
%% Creates a gen_fsm process which calls Module:init/1 to
%% initialize. To ensure a synchronized start-up procedure, this
%% function does not return until Module:init/1 has returned.
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link() ->
    gen_statem:start_link({local, ?SERVER}, ?MODULE, [], []).

-spec add_authz_ids([oc_authz_id()], [oc_authz_id()]) -> ok.
add_authz_ids(Actors, Groups) ->
    gen_statem:cast(?MODULE, {addx, Actors, Groups}).

-spec get_authz_ids() -> {[oc_authz_id()], [oc_authz_id()]}.
get_authz_ids() ->
    gen_statem:call(?MODULE, get_authz_idsx, ?CLEANUP_TIMEOUT).

start() ->
    start_link().
%    gen_statem:cast(?MODULE, startx).

stop() ->
    gen_statem:cast(?MODULE, stopx).

prune() ->
    gen_statem:cast(?MODULE, prunex).

callback_mode() -> state_functions.

%%%===================================================================
%%% gen_fsm callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Whenever a gen_fsm is started using gen_fsm:start/[3,4] or
%% gen_fsm:start_link/[3,4], this function is called by the new
%% process to initialize.
%%
%% @spec init(Args) -> {ok, StateName, State} |
%%                     {ok, StateName, State, Timeout} |
%%                     ignore |
%%                     {stop, StopReason}
%% @end
%%--------------------------------------------------------------------
init([]) ->
    {ok, started, create_timer(#state{})}.

stopped(stop, State) ->
    {next_state, stopped, State};
stopped(start, State) ->
    {next_state, started, create_timer(State)};
stopped({timeout, _Ref, prunex}, State) ->
    {next_state, stopped, State};
stopped(prunex, State) ->
    {next_state, stopped, process_batch(State)};
stopped(_Message, State) ->
    {next_state, stopped, State}.

started(_, stop, State) ->
    {next_state, stopped, cancel_timer(State)};
started(_, start, State) ->
    {next_state, started, State};
started(_, {timeout, _Ref, prunex}, State) ->
    {next_state, started, process_batch(State)};
started(_, prunex, State) ->
    {next_state, started, process_batch(State)};
started(_, _Message, State) ->
    {next_state, started, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Whenever a gen_fsm receives an event sent using
%% gen_fsm:send_all_state_event/2 (now gen_statem:cast/2), this function
%% is called to handle the event.
%%
%% @spec handle_event(Event, StateName, State) ->
%%                   {next_state, NextStateName, NextState} |
%%                   {next_state, NextStateName, NextState, Timeout} |
%%                   {stop, Reason, NewState}
%% @end
%%--------------------------------------------------------------------
%handle_event({add, Actors, Groups}, ok, StateName, State) ->
%    {next_state, ok, StateName, update_state(Actors, Groups, State)}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Whenever a gen_fsm receives an event sent using
%% gen_fsm:sync_send_all_state_event/[2,3], this function is called
%% to handle the event.
%%
%% @spec handle_sync_event(Event, From, StateName, State) ->
%%                   {next_state, NextStateName, NextState} |
%%                   {next_state, NextStateName, NextState, Timeout} |
%%                   {reply, Reply, NextStateName, NextState} |
%%                   {reply, Reply, NextStateName, NextState, Timeout} |
%%                   {stop, Reason, NewState} |
%%                   {stop, Reason, Reply, NewState}
%% @end
%%--------------------------------------------------------------------
%handle_sync_event(get_authz_ids, _From, StateName, State) ->
%    {reply, State#state.authz_ids, StateName, State};
%handle_sync_event(_Event, _From, StateName, State) ->
%    Reply = ok,
%    {reply, Reply, StateName, State}.

%handle_event({call, _From}, get_authz_ids, StateName, State) ->
%    {reply, from, {State#state.authz_ids, StateName, State}};
%handle_event(cast, add, StateName, State) ->
%    {reply, from, StateName, State}.
%handle_event(cast, start, StateName, State) ->
%    {reply, from, StateName, State}.
%handle_event(cast, stop, StateName, State) ->
%    {reply, from, StateName, State}.
%handle_event(cast, prune, StateName, State) ->
%    {reply, from, StateName, State}.
%handle_event(cast, Msg, StateName, State) ->
%    {reply, from, StateName, State}.
get_authz_idsx({call, _From}, StateName, State) ->
    {reply, from, {State#state.authz_ids, StateName, State}}.
addx(cast, {addx, Actors, Groups}, {StateName, State}) ->
    {reply, from, {next_state, StateName, update_state(Actors, Groups, State)}}.
startx(cast, StateName, State) ->
    {reply, from, {StateName, State}}.
stopx(cast, StateName, State) ->
    {reply, from, {StateName, State}}.
prunex(cast, StateName, State) ->
    {reply, from, {StateName, State}}.
%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_fsm when it receives any
%% message other than a synchronous or asynchronous event
%% (or a system message).
%%
%% @spec handle_info(Info,StateName,State)->
%%                   {next_state, NextStateName, NextState} |
%%                   {next_state, NextStateName, NextState, Timeout} |
%%                   {stop, Reason, NewState}
%% @end
%%--------------------------------------------------------------------
handle_info(_Info, StateName, State) ->
    {next_state, StateName, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_fsm when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_fsm terminates with
%% Reason. The return value is ignored.
%%
%% @spec terminate(Reason, StateName, State) -> void()
%% @end
%%--------------------------------------------------------------------
terminate(_Reason, _StateName, _State) ->
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, StateName, State, Extra) ->
%%                   {ok, StateName, NewState}
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, StateName, State, _Extra) ->
    {ok, StateName, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================


process_batch(State = #state{authz_ids = {ActorSet, GroupSet}}) ->
    {ActorAuthzIdsToRemove, RemainingActors} = prune(sets:to_list(ActorSet)),
    {GroupAuthzIdsToRemove, RemainingGroups} = prune(sets:to_list(GroupSet)),
    case {
      length(ActorAuthzIdsToRemove),
      length(RemainingActors),
      length(GroupAuthzIdsToRemove),
      length(RemainingGroups)
     } of
        {0,_,0,_} ->
            ok;
        {LengthActors, LengthRemainingActors, LengthGroups, LengthRemainingGroups} ->
            error_logger:info_msg(
              "oc_chef_authz_cleanup:process_batch actors_removed ~p/~p groups_removed ~p/~p~n",
              [LengthActors,
               check_for_zero(LengthRemainingActors, LengthActors),
               LengthGroups,
               check_for_zero(LengthRemainingGroups, LengthGroups)])
    end,
    SuperUserAuthzId = oc_chef_authz:superuser_id(),
    delete_authz_ids(SuperUserAuthzId, actor, ActorAuthzIdsToRemove),
    delete_authz_ids(SuperUserAuthzId, group, GroupAuthzIdsToRemove),
    create_timer(State#state{authz_ids = {sets:from_list(RemainingActors), sets:from_list(RemainingGroups)}}).

prune(List) ->
    prune(envy:get(oc_chef_authz, cleanup_batch_size, ?DEFAULT_BATCH_SIZE, integer), List).

prune(Count, List) ->
    try
        lists:split(Count, List)
    catch
        error:badarg ->
            {List, []}
    end.

delete_authz_ids(_, _, []) ->
    ok;
delete_authz_ids(SuperUserAuthzId, Type, AuthzIdsToRemove) ->
    [oc_chef_authz:delete_resource(SuperUserAuthzId, Type, AuthzIdToRemove) || AuthzIdToRemove <- AuthzIdsToRemove].

check_for_zero(0, Default) ->
    Default;
check_for_zero(Val, _Default) ->
    Val.

update_state(Actors, Groups, #state{authz_ids = {ActorSet, GroupSet}} = State) ->
    State#state{authz_ids =
                    {sets:union(sets:from_list(Actors), ActorSet),
     sets:union(sets:from_list(Groups), GroupSet)}}.

create_timer(State) ->
    Timeout = envy:get(oc_chef_authz, cleanup_interval, ?DEFAULT_INTERVAL, integer),
    State#state{timer_ref = erlang:start_timer(Timeout, self(), prunex)}.

cancel_timer( State = #state{timer_ref = inactive}) ->
    State;
cancel_timer(State = #state{timer_ref = TimerRef}) ->
    erlang:cancel_timer(TimerRef),
    State#state{timer_ref = inactive}.
