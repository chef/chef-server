%% -*- erlang-indent-level: 4;indent-tabs-mode: nil; fill-column: 92 -*-
%% ex: ts=4 sw=4 et
%% @author Eric B Merritt <ericbmerritt@gmail.com>
%% @copyright Copyright 2012 Opscode, Inc.
-module(bkss_transport).

-export([behaviour_info/1]).

-export([new/2,
         recv/2,
         send/2]).

-export_type([trans/0]).

-record(trans_t, {callback, data}).

%%%===================================================================
%%% Types
%%%===================================================================
-type trans() :: record(trans_t).

%%===================================================================
%% External API
%%===================================================================
-spec behaviour_info(InfoTypeSpecifier::term()) -> term().
behaviour_info(callbacks) ->
    [{new,1},
     {recv,2},
     {send,2}];
behaviour_info(_) ->
    undefined.

-spec new(ModuleName::atom(), Data::term()) -> trans().
new(ModuleName, Data) when is_atom(ModuleName) ->
    #trans_t{callback = ModuleName, data = ModuleName:new(Data)}.

-spec recv(trans(), Length::non_neg_integer()) -> {ok, binary()} | term().
recv(#trans_t{callback = Mod, data = Data}, Length) ->
    Mod:recv(Data, Length).

-spec send(trans(), Binary::binary()) -> ok | term().
send(#trans_t{callback = Mod, data = Data}, Binary) ->
    Mod:send(Data, Binary).
