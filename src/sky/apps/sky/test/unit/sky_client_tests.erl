-module(sky_client_tests).

-include_lib("eunit/include/eunit.hrl").
-include_lib("hoax/include/hoax.hrl").

-include_lib("../src/sky_client.hrl").

-compile([export_all]).

server_test_() ->
    [
     hoax:fixture(?MODULE, "init_"),
     hoax:fixture(?MODULE, "closed_")
    ].

init_returns_expected_state() ->
    {ok, StateName, State} = sky_client:init([dummyURI, dummyOrg, dummyName]),

    ?assertEqual(closed, StateName),
    ?assertEqual(#state{uri = dummyURI, org = dummyOrg, name = dummyName, websocket = undefined}, State).

closed_recieves_open_request_and_opens() ->
    hoax:expect(receive
                gun:open(?HOST, ?PORT, #{retry => 0}) ->
                        {ok, dummyPid}
                end),

    InputState = #state{websocket=undefined},

    {ok, wait_for_open, State} = sky_client:closed(open_request, InputState),

    ?assertEqual(InputState#state{websocket=dummyPid}, State),

    ?verifyAll.
