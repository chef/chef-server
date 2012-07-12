%% -*- erlang-indent-level: 4;indent-tabs-mode: nil; fill-column: 92 -*-
%% ex: ts=4 sw=4 et
%% @author Eric B Merritt <ericbmerritt@gmail.com>
%% @copyright Copyright 2012 Opscode, Inc.
-module(bkss_app).

-behaviour(application).

-export([manual_start/0, manual_stop/0]).
-export([start/2, stop/1]).

%%===================================================================
%% API functions
%%===================================================================

manual_start() ->
    application:start(kernel),
    application:start(stdlib),
    application:start(sasl),
    application:start(gen_leader),
    application:start(gproc),
    application:start(inets),
    application:start(bookshelf_store).

manual_stop() ->
    application:stop(bookshelf_store),
    application:stop(inets),
    application:stop(gen_leader),
    application:stop(gproc).

start(_StartType, _StartArgs) ->
    ensure_disk_store(),
    bkss_sup:start_link().

stop(_State) ->
    ok.

%%===================================================================
%% External
%%===================================================================
ensure_disk_store() ->
    case application:get_env(bookshelf_store, disk_store) of
        undefined ->
            Store = filename:join(code:priv_dir(bookshelf_store), "data"),
            filelib:ensure_dir(filename:join(Store, "tmp")),
            application:set_env(bookshelf_store, disk_store, Store);
        {ok, DiskStore} ->
            case filelib:is_dir(DiskStore) of
                true ->
                    ok;
                false ->
                    throw({error, disk_store_does_not_exist})
            end
    end.
