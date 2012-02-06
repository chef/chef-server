%% @author Tim Dysinger <timd@opscode.com>
%% @copyright 2012 Opscode, Inc. All Rights Reserved

-module(bookshelf_app).
-behaviour(application).
-export([start/0, start/2, stop/1]).

start() ->
    application:start(crypto),
    application:start(public_key),
    application:start(ssl),
    application:start(cowboy),
    application:start(bookshelf).

start(_Type, _Args) ->
    Dispatch = [{'_', [{[], bookshelf_bucket, []}]},  %% service fqdn
                {'_', [{[], bookshelf_bucket, []}]}], %% bucket subdomain
    cowboy:start_listener(my_http_listener,
                          100,
                          cowboy_tcp_transport,
                          [{port, 8080}],
                          cowboy_http_protocol,
                          [{dispatch, Dispatch}]),
    bookshelf_sup:start_link().

stop(_State) ->
    ok.

%% TODO We have different behavior based on the request hostname
