-module(payload).

-export([
    io/3]).

io(SessionID, Env, Input) ->
    io:format(user, "pid ~p", [whereis(telemetry_mock_consumer)]),
    telemetry_mock_consumer ! {http_request, self(), Input},
    io:format(user, "input parameters ~p, ~p, ~p", [SessionID, Env, Input]),
    mod_esi:deliver(SessionID,  "status:201 Created\r\nContent-Type:text/html\r\n\r\n"),  
    ok.