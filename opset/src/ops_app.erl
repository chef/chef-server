%%%-------------------------------------------------------------------
%%% @author Eric B Merritt <ericbmerritt@gmail.com>
%%% @copyright (C) 2012, Opscode, Inc.
%%%-------------------------------------------------------------------
-module(ops_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

-export([manual_start/0, manual_stop/0]).

%%%===================================================================
%%% Application callbacks
%%%===================================================================

-spec start(normal | {takeover, node()} | {failover, node()}, term()) ->
                   {ok, pid()} | {ok, pid(), State::term()} | {error, term()}.
start(_StartType, _StartArgs) ->
    ops_sup:start_link().

-spec stop(Ignore::term()) -> ok.
stop(_State) ->
    ok.

%%%===================================================================
%%% API
%%%===================================================================
-spec manual_start() -> ok.
manual_start() ->
    application:start(kernel),
    application:start(stdlib),
    application:start(sasl),
    application:start(gproc),
    application:start(opset),
    ok.

-spec manual_stop() -> ok.
manual_stop() ->
    application:stop(opset),
    application:stop(gproc),
    ok.
