-module(bifrost_app).

-behaviour(application).
-export([start/2,stop/1]).

start(_Type, _StartArgs) ->
    %% erlang 19.3.x SIGTERM changes caused runit failures.
    %% this reverts to previous working SIGTERM behavior.
    os:set_signal(sigterm, default),

    { ok, AppList } =  application:get_key(bifrost, included_applications),
    [ application:ensure_all_started(App, permanent) || App <- AppList ],
    case os:getenv("DEVVM") of
        "1" ->
            application:start(sync);
        _ -> ok
    end,
    bifrost_sup:start_link().

stop(_State) ->
    ok.
