%%%-------------------------------------------------------------------
%%% @copyright (C) 2017, Chef
%%% @doc
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(sky_client).

-behaviour(gen_fsm).

%% API
-export([start_link/4,
         open_connection/1,
         send_message/2]).

%% gen_fsm callbacks
-export([init/1,
         closed/2,
         closed/3,
         wait_for_open/2,
         wait_for_open/3,
         wait_for_upgrade/2,
         wait_for_upgrade/3,
         open/2,
         open/3,
         handle_event/3,
         handle_sync_event/4,
         handle_info/3,
         terminate/3,
         code_change/4]).

-include_lib("sky_client.hrl").

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the fsm
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link(Host, Port, Org, Name) ->
    gen_fsm:start_link(?MODULE, [Host, Port, Org, Name], []).

%%%===================================================================
%%%
%%%
open_connection(Pid) ->
  gen_fsm:send_event(Pid, open_request).

send_message(Pid, Message) ->
  gen_fsm:sync_send_event(Pid, {send_message, Message}).


%%%===================================================================
%%% gen_fsm callbacks
%%%===================================================================

%% State -- > Event (Command/Notification) Received --> Action --> New State

%-spec init([binary(), binary(), binary()]) -> {ok, closed, #state{} }.
init([Host, Port, Org, Name]) ->
    State = #state{
               host = Host,
               port = Port,
               org = Org,
               name = Name,
               websocket = undefined,
               heartbeat_cancel_ref = undefined},
    gen_fsm:send_event(self(), open_request),
    {ok, closed, State}.

%% closed --> "please open" --> gun:open --> wait_for_open
closed(open_request, State = #state{host = Host, port = Port} ) ->
  {ok, Pid} = gun:open(Host, Port, #{retry => 0}),
  State1 = State#state{websocket = Pid},
  {next_state, wait_for_open, State1}.

closed({send_message, _Message}, _From, State) ->
  {reply, {error, closed}, closed, State}.

%% wait_for_open   --> "ok, I'm up" --> gun:ws_upgrade --> wait_for_upgrade
wait_for_open(ready_for_upgrade, State = #state{websocket = Websocket}) ->
    Path = build_resource(State),
    lager:info("Opening path '~p'", [Path]),
    gun:ws_upgrade(Websocket, Path, [], #{compress => false}),
    {next_state, wait_for_upgrade, State}.

wait_for_open({send_message, _Message}, _From, State) ->
  {reply, {error, closed}, wait_for_open, State}.

%% wait_for_upgrade --> "I'm upgraded" --> n/a --> open
wait_for_upgrade(upgraded_to_websocket, State) ->
  State1 = set_heartbeat(State),
  {next_state, open, State1}.

wait_for_upgrade({send_message, _Message}, _From, State) ->
  {reply, {error, closed}, wait_for_upgrade, State}.

%% open --> "Message from server" --> route_message --> open
%% open --> "Pls send msg to server" --> send_message --> open
%% open --> connection drops --> cleanup --> closed
open({receive_message, Message}, State = #state{org = Org, name = Name}) ->
  lager:info("Received message (~p/~p): ~p", [Org, Name, Message]),
  {next_state, open, State};
open(connection_dropped, State = #state{websocket = Websocket}) ->
  gun:close(Websocket),
  gen_fsm:send_event(self(), open_request),
  State1 = cancel_heartbeat(State),
  {next_state, closed, State1#state{websocket = undefined}};
open(send_heartbeat, State = #state{websocket = Websocket}) ->
  ok = gun:ws_send(Websocket, {text, "CLIENT_HEARTBEAT"}),
  State1 = set_heartbeat(State),
  {next_state, open, State1}.

open({send_message, Message}, _From, State = #state{websocket=Websocket}) ->
  ok = gun:ws_send(Websocket, {text, Message}),
  {reply, ok, open, State}.


%%--------------------------------------------------------------------
%%% @private
%%% @doc
%%% This function is called by a gen_fsm when it receives any
%%% message other than a synchronous or asynchronous event
%%% (or a system message).
%%%
%%% @spec handle_info(Info,StateName,State)->
%%%                   {next_state, NextStateName, NextState} |
%%%                   {stop, Reason, NewState}
%%% @end
%%%--------------------------------------------------------------------
handle_info({gun_up, _ServerPid, _Protocol}, StateName, State) ->
    gen_fsm:send_event(self(), ready_for_upgrade),
    {next_state, StateName, State};
handle_info({gun_ws, _ConnPid, {text, Message}}, StateName, State) ->
    gen_fsm:send_event(self(), {receive_message, Message}),
    {next_state, StateName, State};
handle_info({gun_ws_upgrade, _ServerPid, ok, _Headers}, StateName, State) ->
    gen_fsm:send_event(self(), upgraded_to_websocket),
    {next_state, StateName, State};
handle_info({gun_down, _ConnPid, _Protocol, _Reason, _KilledStreams, _UnprocessedStreams}, StateName, State) ->
    gen_fsm:send_event(self(), connection_dropped),
    {next_state, StateName, State};
handle_info({'DOWN', _MRef, process, _ServerPid, _Reason}, StateName, State) ->
  %% DO SOMETHING HERE
    {next_state, StateName, State};
handle_info(_Info, StateName, State) ->
    {next_state, StateName, State}.

handle_event(_Event, StateName, State) ->
    {next_state, StateName, State}.

handle_sync_event(_Event, _From, StateName, State) ->
    {next_state, ok, StateName, State}.

%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
terminate(_Reason, _StateName, _State) ->
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, _StateName, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
build_resource(#state{org = Org, name = Name}) ->
    erlang:iolist_to_binary(["/organizations/", Org, "/websocket/", Name]).

set_heartbeat(State) ->
  Ref = gen_fsm:send_event_after(?HEARTBEAT, send_heartbeat),
  State#state{heartbeat_cancel_ref = Ref}.

cancel_heartbeat(State = #state{heartbeat_cancel_ref = HeartbeatCancelRef}) ->
  _ = gen_fsm:cancel_timer(HeartbeatCancelRef),
  State#state{heartbeat_cancel_ref = undefined}.
