-module(server_connect_integration_tests).

-include_lib("eunit/include/eunit.hrl").
%-include_lib("hoax/include/hoax.hrl").

-compile([export_all]).

-define(HOST, "127.0.0.1").
-define(DEFAULT_PORT, 8080).

server_test_() ->
    [
     hoax:fixture(?MODULE, "server_")
    ].


server_starts_and_stops() ->
    {ok, Apps} = start_server(),
    %% try connecting
    %% cleanup
    stop_server(Apps).

server_can_be_connected_to() ->
    Path = "/organizations/testorg/websocket/someclient",
    {ok, Apps} = start_server(),

    application:ensure_all_started(gun),

    {ok, Pid} = gun:open(?HOST, ?DEFAULT_PORT, #{retry=>0}),

    {ok, http} = gun:await_up(Pid),
    Ref = monitor(process, Pid),
    gun:ws_upgrade(Pid, Path, [], #{compress => false}),
    receive
        {gun_ws_upgrade, Pid, ok, _} ->
            ok;
        Msg ->
            io:format("Unexpected message ~p", [Msg]),
            error(failed)
    end,
    close(Pid, Ref),
    stop_server(Apps).

server_says_something_after_connecting() ->
    {ok, Apps} = start_server(),

    application:ensure_all_started(gun),

    {Pid, Ref} = open_connection_to_server(),

    receive
        {gun_ws, Pid, Frame} ->
            ?assertEqual({text, <<"CONFIG:60:DEADBEEF">>}, Frame);
        _Msg ->
            ?debugFmt("Unexpected message ~p", [_Msg])
    end,
    close(Pid, Ref),
    stop_server(Apps).


open_connection_to_server() ->
    Path = "/organizations/testorg/websocket/someclient",
    {ok, Pid} = gun:open(?HOST, ?DEFAULT_PORT, #{retry=>0}),

    {ok, http} = gun:await_up(Pid),
    Ref = monitor(process, Pid),
    _StreamRef = gun:ws_upgrade(Pid, Path, [], #{compress => false}),
    receive
        {gun_ws_upgrade, Pid, ok, _} ->
            ok;
        Msg ->
            io:format("Unexpected message ~p", [Msg]),
            error(failed)
    end,
    {Pid, Ref}.

watch_messages(Text, Timeout) ->
    receive
        Msg ->
            ?debugFmt("~p saw message ~p", [Text, Msg]),
            watch_messages(Text, Timeout)
    after
        Timeout ->
            ?debugFmt("~p timed out", [Text]),
            ok
    end.

%% helpers to manage the server
start_server() ->
    case application:ensure_all_started(sky) of
        {ok, Apps} ->
            {ok, Apps};
        {error, Error} ->
            erlang:error({application_start_failed, Error})
    end.

stop_server(Apps) ->
    [application:stop(A) || A <- lists:flatten(Apps)],
    ok.


close(Pid, Ref) ->
        demonitor(Ref),
        gun:close(Pid),
        gun:flush(Pid).
