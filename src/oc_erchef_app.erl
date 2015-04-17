-module(oc_erchef_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    case os:getenv("DEVVM") of
        "1" ->
            application:start(sync);
        _ -> ok
    end,
    oc_erchef_sup:start_link().

stop(_State) ->
    ok.
