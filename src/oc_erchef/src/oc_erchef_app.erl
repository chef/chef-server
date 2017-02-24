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
    %% when start is invoked, any non-included apps are already started
    %% This means that chef_secrets is started - so it's safe to
    %% start pooler and any other included apps that needed to wait for pooler. See
    %% comment in app.src for details.
    { ok, AppList } =  application:get_key(oc_erchef, included_applications),
    [ application:ensure_all_started(App) || App <- AppList ],

    %% If we're in a dev vm environment, start the code sync & compile tools
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
