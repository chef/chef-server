%%%-------------------------------------------------------------------
%%% @copyright (C) 2017, Chef
%%% @doc
%%%
%%% @end
%%%-------------------------------------------------------------------

-define(HEARTBEAT, 60*1000).
-define(HOST, "127.0.0.1").
-define(PORT, 8080).
-define(RESOURCE, "/organizations/testorg/websocket/someclient").

-record(state, {
          uri, % :: binary(),
          org, % :: binary(),
          name, % :: binary(),
          websocket % :: pid() | undefined
         }).

