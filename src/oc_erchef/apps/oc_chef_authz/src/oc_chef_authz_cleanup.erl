%% -*- erlang-indent-level: 4;indent-tabs-mode: nil; fill-column: 92 -*-
%% ex: ts=4 sw=4 et
%%%-------------------------------------------------------------------
%%% @author Oliver Ferrigni <>
%%% @doc gen_statem responsible for cleaning up orphaned authz_ids.  These
%%% authz ids are detected in oc_chef_group and added to a set of either
%%% actor or group authz_ids.  On a timer, the authz_ids are deleted
%%% from authz.
%%%
%%% @end
%%% Created :  6 Nov 2013 by Oliver Ferrigni <>
%%%-------------------------------------------------------------------
%% Copyright Chef Software, Inc. All Rights Reserved.
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
         prune/2
        ]).

%% gen_statem callbacks
-export([
         callback_mode/0,
         init/1,
         terminate/3,
         code_change/4]).

%% FSM states
-export([
         stopped/3,
         started/3
        ]).

-define(SERVER, ?MODULE).

-define(DEFAULT_BATCH_SIZE, 2500).
-define(DEFAULT_INTERVAL, 1000).

-include("oc_chef_authz.hrl").
-include("oc_chef_authz_cleanup.hrl").


%%--------------------------------------------------------------------
%% @doc
%% Creates a gen_statem process which calls Module:init/1 to
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
    gen_statem:cast(?MODULE, {add, Actors, Groups}).

-spec get_authz_ids() -> {[oc_authz_id()], [oc_authz_id()]}.
get_authz_ids() ->
    gen_statem:call(?MODULE, get_authz_ids, ?CLEANUP_TIMEOUT).

start() ->
    gen_statem:cast(?MODULE, start).

stop() ->
    gen_statem:cast(?MODULE, stop).

prune() ->
    gen_statem:cast(?MODULE, prune).
%%%===================================================================
%%% gen_statem callbacks
%%%===================================================================

callback_mode() ->
    state_functions.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Whenever a gen_statem is started using gen_statem:start/[3,4] or
%% gen_statem:start_link/[3,4], this function is called by the new
%% process to initialize.
%% @end
%%--------------------------------------------------------------------
init([]) ->
    {ok, started, create_timer(#state{})}.

stopped(cast, stop, State) ->
    {keep_state, State};
stopped(cast, start, State) ->
    {next_state, started, create_timer(State)};
stopped(info, {timeout, _Ref, prune}, State) ->
    {keep_state, State};
stopped(cast, prune, State) ->
    {keep_state, process_batch(State)};
stopped(EventType, Event, State) ->
    handle_common(EventType, Event, State).

started(cast, stop, State) ->
    {next_state, stopped, cancel_timer(State)};
started(cast, start, State) ->
    {keep_state, State};
started(info, {timeout, _Ref, prune}, State) ->
    {keep_state, process_batch(State)};
started(cast, prune, State) ->
    {keep_state, process_batch(State)};
started(EventType, Event, State) ->
    handle_common(EventType, Event, State).

%% Events handled identically in every state.
handle_common(cast, {add, Actors, Groups}, State) ->
    {keep_state, update_state(Actors, Groups, State)};
handle_common({call, From}, get_authz_ids, State) ->
    {keep_state, State, [{reply, From, State#state.authz_ids}]};
handle_common({call, From}, _Event, State) ->
    {keep_state, State, [{reply, From, ok}]};
handle_common(_EventType, _Event, State) ->
    {keep_state, State}.

terminate(_Reason, _StateName, _State) ->
    ok.

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
        {0, _, 0, _} ->
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
    State#state{timer_ref = erlang:start_timer(Timeout, self(), prune)}.

cancel_timer( State = #state{timer_ref = inactive}) ->
    State;
cancel_timer(State = #state{timer_ref = TimerRef}) ->
    _ = erlang:cancel_timer(TimerRef),
    State#state{timer_ref = inactive}.
