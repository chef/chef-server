%%%-------------------------------------------------------------------
%%% @copyright (C) 2017, Chef
%%% @doc
%%%
%%% @end
%%%-------------------------------------------------------------------

-define(HEARTBEAT, 60*1000).

-record(state, {
          host, % :: binary(),
          port, % :: integer(),
          org, % :: binary(),
          name, % :: binary(),
          websocket % :: pid() | undefined
         }).

