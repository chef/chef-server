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
    %% erlang 19.3.x SIGTERM changes caused runit failures.
    %% this reverts to previous working SIGTERM behavior.
    os:set_signal(sigterm, default),

    %% When start is invoked, any non-included apps are already started.
    %% This means that chef_secrets is started - so it's safe to start
    %% pooler and any other included apps that needed to wait for pooler.
    %% See comment in app.src for details.
    { ok, AppList } =  application:get_key(oc_erchef, included_applications),
    [ application:ensure_all_started(App, permanent) || App <- AppList ],

% not sure if erlcloud is included in list of applications (output of below = false)
io:format("~noc_erchef_app: erlcloud is included in list? ~p~n", [lists:member(erlcloud, AppList)]),
application:ensure_all_started(erlcloud, permanent),

AwsAccessKeyId     = envy:get(chef_objects, aws_access_key_id,     undefined, [atom, list]),
AwsSecretAccessKey = envy:get(chef_objects, aws_secret_access_key, undefined, [atom, list]),
AwsSessionToken    = envy:get(chef_objects, aws_session_token,     undefined, [atom, list]),
AwsDefaultRegion   = envy:get(chef_objects, aws_default_region,    undefined, [atom, list]),

% dunno if this should be before or after start_link()
application:set_env(erlcloud, aws_access_key_id,     AwsAccessKeyId),
application:set_env(erlcloud, aws_secret_access_key, AwsSecretAccessKey),
application:set_env(erlcloud, aws_security_token,    AwsSessionToken),
application:set_env(erlcloud, aws_region,            AwsDefaultRegion),

io:format("~n~noc_erchef_app: aws_access_key_id     - ~p", [application:get_env(erlcloud, aws_access_key_id)]),
io:format(  "~noc_erchef_app: aws_secret_access_key - ~p", [application:get_env(erlcloud, aws_secret_access_key)]),
io:format(  "~noc_erchef_app: aws_session_token     - ~p", [application:get_env(erlcloud, aws_security_token)]),
io:format(  "~noc_erchef_app: aws_default_region    - ~p", [application:get_env(erlcloud, aws_region)]),


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
