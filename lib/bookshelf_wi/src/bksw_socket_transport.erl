%% -*- erlang-indent-level: 4;indent-tabs-mode: nil; fill-column: 92 -*-
%% ex: ts=4 sw=4 et
%% @author Eric Merritt <ericbmerritt@gmail.com>
%% @copyright Copyright 2012 Opscode, Inc.
-module(bksw_socket_transport).

-behaviour(bkss_transport).

-export([new/1,
         recv/2,
         send/2]).

-export_type([state/0]).

-record(state, {transport, socket, timeout}).

%%%===================================================================
%%% Types
%%%===================================================================
-opaque state() :: record(state).

%%===================================================================
%% External API
%%===================================================================
-spec new(list()) -> state().
new([Transport, Socket, Timeout]) ->
    #state{transport = Transport, socket = Socket, timeout = Timeout}.

-spec recv(state(), non_neg_integer()) -> {ok, binary()} | term().
recv(#state{transport = Transport, socket = Socket, timeout = Timeout}, Length) ->
    Transport:recv(Socket, Length, Timeout).

-spec send(state(), binary()) -> ok | term().
send(#state{transport = Transport, socket = Socket, timeout = _Timeout}, Binary) ->
    Transport:send(Socket, Binary).
