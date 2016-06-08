-module(oc_erchef_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).
-export([server_api_version/1]).
-include("include/server_api_version.hrl").

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

-spec server_api_version(min|max|deprecated) -> {ok, api_version()}.
server_api_version(min) ->
    {ok, ?API_MIN_VER};
server_api_version(max) ->
    {ok, ?API_MAX_VER};
server_api_version(deprecated) ->
    {ok, ?API_DEPRECATED_VER}.
