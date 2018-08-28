%% -*- erlang-indent-level: 4;indent-tabs-mode: nil;fill-column: 92 -*-
%% ex: ts=4 sw=4 et
%% @author Kevin Smith <kevin@chef.io>
%% @copyright 2011 Chef Software, Inc.
-module(mover_app).

-behaviour(application).

%% Application callbacks
-export([start/2,
         stop/1]).

start(_StartType, _StartArgs) ->
    { ok, AppList } =  application:get_key(mover, included_applications),
    [ application:ensure_all_started(App, permanent) || App <- AppList ],
    mover_sup:start_link().

stop(_State) ->
    ok.
