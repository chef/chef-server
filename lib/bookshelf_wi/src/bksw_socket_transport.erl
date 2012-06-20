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

%%%===================================================================
%%% Types
%%%===================================================================
-opaque state() :: CowboyHttpReq::term().

%%===================================================================
%% External API
%%===================================================================
-spec new(list()) -> state().
new(Req) ->
    Req.

-spec recv(state(), non_neg_integer()) -> {ok, binary()} | eof | term().
recv(Req0, _Length) ->
    case cowboy_http_req:stream_body(Req0) of
        {ok, Bin, Req1} ->
            {Req1, {ok, Bin}};
        {done, Req1} ->
            {Req1, eof};
        Error = {error, _}  ->
            error_logger:error_msg("FOOOOOOOOOOOOOO"),
            {Req0, Error}
    end.

-spec send(state(), binary()) -> ok | term().
send(Req0, Binary) ->
    {ok, Transport, Socket} = cowboy_http_req:transport(Req0),
    {Req0, Transport:send(Socket, Binary)}.
