%% -*- erlang-indent-level: 4;indent-tabs-mode: nil; fill-column: 92 -*-
%% ex: ts=4 sw=4 et
%% @author Eric B Merritt <ericbmerritt@gmail.com>
%% @copyright Copyright 2012 Opscode, Inc.
-module(bkss_app).

-behaviour(application).

-export([manual_start/0, manual_stop/0]).
-export([start/2, stop/1]).

-include("internal.hrl").

%%===================================================================
%% API functions
%%===================================================================

manual_start() ->
    application:start(kernel),
    application:start(stdlib),
    application:start(sasl),
    application:start(gen_leader),
    application:start(gproc),
    application:start(opset),
    application:start(inets),
    application:start(bookshelf_store).

manual_stop() ->
    application:stop(opset),
    application:stop(gproc),
    application:stop(gen_leader),
    application:stop(inets),
    application:stop(bookshelf_store).

start(_StartType, _StartArgs) ->
    opset:create(?BOOKSHELF_CONFIG,
                 [{disk_store, get_initial_config()}]),
    bkss_sup:start_link().

stop(_State) ->
    ok.

%%===================================================================
%% External
%%===================================================================
get_initial_config() ->
   Store1 =
        case application:get_env(bookshelf_store, disk_store) of
            undefined ->
                filename:join(code:priv_dir(bookshelf_store), "data");
            {ok, Store0} ->
                Store0
        end,
    filelib:ensure_dir(filename:join(Store1, "tmp")),
    Store1.
