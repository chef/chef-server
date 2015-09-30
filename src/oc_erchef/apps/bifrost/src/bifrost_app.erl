-module(bifrost_app).

-behaviour(application).
-export([start/2,stop/1]).

start(_Type, _StartArgs) ->
    case os:getenv("DEVVM") of
        "1" ->
            application:start(sync);
        _ -> ok
    end,
    bifrost_sup:start_link().

stop(_State) ->
    ok.
