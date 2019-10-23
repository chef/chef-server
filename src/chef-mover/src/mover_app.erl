%% -*- erlang-indent-level: 4;indent-tabs-mode: nil;fill-column: 92 -*-
%% ex: ts=4 sw=4 et
%% @author Kevin Smith <kevin@chef.io>
%% @copyright 2019 Opscode, Inc.
-module(mover_app).

-behaviour(application).

%% Application callbacks
-export([start/2,
         stop/1]).

start(_StartType, _StartArgs) ->
    %% erlang 19.3.x SIGTERM changes caused runit failures.
    %% this reverts to previous working SIGTERM behavior.
    os:set_signal(sigterm, default),

    { ok, AppList } =  application:get_key(mover, included_applications),
    [ application:ensure_all_started(App, permanent) || App <- AppList ],
    mover_sup:start_link().

stop(_State) ->
    ok.
