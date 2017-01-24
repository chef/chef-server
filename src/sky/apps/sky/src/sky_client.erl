%%%-------------------------------------------------------------------
%%% @author Mark Anderson <mark@chef.io>
%%% @copyright (C) 2017, Chef
%%% @doc
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(sky_client).

-behaviour(gen_fsm).

%% API
-export([start_link/3,
         open_connection/1,
         send_message/2]).

%% gen_fsm callbacks
-export([init/1,
         closed/2,
         wait_for_open/2,
         wait_for_upgrade/2,
         open/2,
         handle_event/3,
         handle_sync_event/4,
         handle_info/3,
         terminate/3,
         code_change/4]).

-define(HEARTBEAT, 60*1000).
-define(HOST, "127.0.0.1").
-define(PORT, 8080).
-define(RESOURCE, "/organizations/testorg/websocket/someclient").

-record(state, {
          uri, % :: binary(),
          org, % :: binary(),
          name, % :: binary(),
          websocket % :: pid() | undefined
         }).

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
start_link(URI, Org, Name) ->
    gen_fsm:start_link(?MODULE, [URI, Org, Name], []).

%%%===================================================================
%%%
%%%
open_connection(Pid) ->
  gen_fsm:send_event(Pid, open_request).

send_message(Pid, Message) ->
  gen_fsm:send_event(Pid, {send_message, Message}).


%%%===================================================================
%%% gen_fsm callbacks
%%%===================================================================

%% State -- > Event (Command/Notification) Received --> Action --> New State

%-spec init([binary(), binary(), binary()]) -> {ok, closed, #state{} }.
init([URI, Org, Name]) ->
    State = #state{uri = URI, org = Org, name = Name, websocket = undefined},
    {ok, closed, State}.

%% closed --> "please open" --> gun:open --> wait_for_open
closed(open_request, State = #state{uri=URI} ) ->
  {Hostname, Port} = extract_host_and_port(URI),
  {ok, Pid} = gun:open(Hostname, Port, #{retry => 0}),
  State1 = State#state{websocket = Pid},
  {ok, wait_for_open, State1}.

%% wait_for_open   --> "ok, I'm up" --> gun:ws_upgrade --> wait_for_upgrade
wait_for_open(ready_for_upgrade, State = #state{uri=URI, websocket=Websocket}) ->
  Path = extract_resource(URI),
  gun:ws_upgrade(Websocket, Path, [], #{compress => false}),
  {ok, wait_for_upgrade, State}.

%% wait_for_upgrade --> "I'm upgraded" --> n/a --> open
wait_for_upgrade(upgraded_to_websocket, State) ->
  {ok, open, State}.

%% open --> "Message from server" --> route_message --> open
%% open --> "Pls send msg to server" --> send_message --> open
%% open --> connection drops --> cleanup --> closed
open({send_message, Message}, State = #state{websocket=Websocket}) ->
  ok = gun:ws_send(Websocket, {text, Message}),
  {ok, open, State}.



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
handle_info({gun_ws_upgrade, _ServerPid, ok, _Headers}, StateName, State) ->
    gen_fsm:send_event(self(), upgraded_to_websocket),
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
extract_host_and_port(_Uri) ->
  {?HOST, ?PORT}.

extract_resource(_Uri) ->
  ?RESOURCE.

