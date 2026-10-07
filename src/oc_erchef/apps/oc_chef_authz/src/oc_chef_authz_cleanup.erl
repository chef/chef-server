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
         init/1,
         callback_mode/0,
         terminate/3,
         code_change/4
        ]).

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

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Selects the state function callback mode, so that each state is
%% handled by a function of the same name (stopped/3, started/3).
%%
%% @spec callback_mode() -> state_functions
%% @end
%%--------------------------------------------------------------------
callback_mode() ->
    state_functions.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Whenever a gen_statem is started using gen_statem:start/[3,4] or
%% gen_statem:start_link/[3,4], this function is called by the new
%% process to initialize.
%%
%% @spec init(Args) -> {ok, State, Data} |
%%                     {ok, State, Data, Actions} |
%%                     ignore |
%%                     {stop, StopReason}
%% @end
%%--------------------------------------------------------------------
init([]) ->
    {ok, started, create_timer(#state{})}.

stopped(cast, stop, Data) ->
    {next_state, stopped, Data};
stopped(cast, start, Data) ->
    {next_state, started, create_timer(Data)};
stopped(cast, prune, Data) ->
    {next_state, stopped, process_batch(Data)};
stopped(info, {timeout, _Ref, prune}, Data) ->
    {next_state, stopped, Data};
stopped(EventType, Event, Data) ->
    handle_common(EventType, Event, Data).


started(cast, stop, Data) ->
    {next_state, stopped, cancel_timer(Data)};
started(cast, start, Data) ->
    {next_state, started, Data};
started(cast, prune, Data) ->
    {next_state, started, process_batch(Data)};
started(info, {timeout, _Ref, prune}, Data) ->
    {next_state, started, process_batch(Data)};
started(EventType, Event, Data) ->
    handle_common(EventType, Event, Data).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Events that are handled the same way regardless of the current
%% state. Under gen_fsm these were spread across handle_event/3
%% (all state events), handle_sync_event/4 (all state sync events)
%% and handle_info/3; gen_statem delivers them to the state function
%% for the current state, so each state function falls through here.
%%
%% @spec handle_common(EventType, Event, Data) ->
%%                   keep_state_and_data |
%%                   {keep_state, NewData} |
%%                   {keep_state_and_data, Actions}
%% @end
%%--------------------------------------------------------------------
handle_common(cast, {add, Actors, Groups}, Data) ->
    {keep_state, update_state(Actors, Groups, Data)};
handle_common({call, From}, get_authz_ids, Data) ->
    {keep_state_and_data, [{reply, From, Data#state.authz_ids}]};
handle_common({call, From}, _Event, _Data) ->
    {keep_state_and_data, [{reply, From, ok}]};
handle_common(_EventType, _Event, _Data) ->
    keep_state_and_data.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_statem when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_statem terminates with
%% Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State, Data) -> void()
%% @end
%%--------------------------------------------------------------------
terminate(_Reason, _StateName, _Data) ->
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Data, Extra) ->
%%                   {ok, State, Data}
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, StateName, Data, _Extra) ->
    {ok, StateName, Data}.

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
    erlang:cancel_timer(TimerRef),
    State#state{timer_ref = inactive}.
