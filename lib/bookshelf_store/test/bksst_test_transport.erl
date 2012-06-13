%% -*- erlang-indent-level: 4;indent-tabs-mode: nil; fill-column: 92 -*-
%% ex: ts=4 sw=4 et
%% @author Eric Merritt <ericbmerritt@gmail.com>
%% @copyright Copyright 2012 Opscode, Inc.
-module(bksst_test_transport).

-behaviour(bkss_transport).

-export([new/1,
         recv/2,
         send/2]).

-export_type([state/0]).

-record(state, {data}).

%%%===================================================================
%%% Types
%%%===================================================================
-opaque state() :: record(state).

%%===================================================================
%% External API
%%===================================================================
-spec new(list()) -> state().
new([Data]) ->
    #state{data=Data}.

-spec recv(state(), non_neg_integer()) -> {ok, binary()} | term().
recv(#state{data=Data}, _Length) ->
    {ok, Data}.

-spec send(state(), binary()) -> ok | term().
send(#state{}, _Binary) ->
    ok.
