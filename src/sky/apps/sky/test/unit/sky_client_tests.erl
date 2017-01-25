-module(sky_client_tests).

-include_lib("eunit/include/eunit.hrl").
-include_lib("hoax/include/hoax.hrl").

-include_lib("../src/sky_client.hrl").

-compile([export_all]).

server_test_() ->
    [
     hoax:fixture(?MODULE, "init_"),
     hoax:fixture(?MODULE, "closed_"),
     hoax:fixture(?MODULE, "wait_for_open_"),
     hoax:fixture(?MODULE, "wait_for_upgrade_"),
     hoax:fixture(?MODULE, "open_")
    ].

init_returns_expected_state() ->
    {ok, StateName, State} = sky_client:init([dummyHost, dummyPort, dummyOrg, dummyName]),

    ?assertEqual(closed, StateName),
    ?assertEqual(#state{host = dummyHost, port = dummyPort, org = dummyOrg, name = dummyName, websocket = undefined}, State).

closed_recieves_open_request_and_opens() ->
    hoax:expect(receive
                gun:open(dummyHost, dummyPort, #{retry => 0}) ->
                        {ok, dummyPid}
                end),

    InputState = #state{host = dummyHost, port = dummyPort, websocket = undefined},

    {next_state, wait_for_open, State} = sky_client:closed(open_request, InputState),

    ?assertEqual(InputState#state{host = dummyHost, port = dummyPort, websocket = dummyPid}, State),

    ?verifyAll.

closed_receives_send_message_and_returns_error() ->
    InputState = #state{},

    {reply, {error, closed}, closed, State} = sky_client:closed({send_message, dummyMessage}, dummyCaller, InputState),
    ?assertEqual(InputState, State).


wait_for_open_receives_ready_for_upgrade_and_upgrades() ->
    hoax:expect(receive
                gun:ws_upgrade(dummyWebsocket, "/organizations/dummyOrg/websocket/dummyName", [], #{compress => false}) ->
                        ok
                end),

    InputState = #state{org = dummyOrg, name = dummyName, websocket = dummyWebsocket},

    {next_state, wait_for_upgrade, State} = sky_client:wait_for_open(ready_for_upgrade, InputState),

    ?assertEqual(InputState, State),

    ?verifyAll.

wait_for_open_receives_send_message_and_returns_error() ->
    InputState = #state{},

    {reply, {error, closed}, wait_for_open, State} = sky_client:wait_for_open({send_message, dummyMessage}, dummyCaller, InputState),

    ?assertEqual(InputState, State).

wait_for_upgrade_receives_upgraded_to_websocket_and_transitions_to_open() ->
    InputState = #state{},

    {next_state, open, State} = sky_client:wait_for_upgrade(upgraded_to_websocket, InputState),

    ?assertEqual(InputState, State).

wait_for_upgrade_receives_send_message_and_returns_error() ->
    InputState = #state{},

    {reply, {error, closed}, wait_for_upgrade, State} = sky_client:wait_for_upgrade({send_message, dummyMessage}, dummyCaller, InputState),

    ?assertEqual(InputState, State).

open_receives_send_message_and_sends_message() ->
    hoax:expect(receive
                gun:ws_send(dummyWebsocket, {text, dummyMessage}) ->
                        ok
                end),

    InputState = #state{websocket = dummyWebsocket},

    {reply, ok, open, State} = sky_client:open({send_message, dummyMessage}, dummyCaller, InputState),

    ?assertEqual(InputState, State),

    ?verifyAll.

open_receives_message_and_logs_message() ->
  InputState = #state{},

  {next_state, open, State} = sky_client:open({receive_message, testMessage}, InputState),

  ?assertEqual(InputState, State).
