-module(sky_websocket_handler).

-export([init/2]).
-export([websocket_init/1]).
-export([websocket_handle/2]).
-export([websocket_info/2]).

-record(state, {
          orgname = undefined,
          clientname = undefined
         }).

-define(DISCONNECT_TIMEOUT, 3*60*1000).

init(Req, _Opts) ->
    OrgName = cowboy_req:binding(orgname, Req),
    ClientName = cowboy_req:binding(clientname, Req),
    State = #state{ orgname = OrgName, clientname = ClientName },
    lager:info("Starting listener ~p for ~p ~p", [self(), OrgName, ClientName]),
    % Note: Req goes away after this; must extract all the things we need first.
    {cowboy_websocket, Req, State, ?DISCONNECT_TIMEOUT}.

%% Notes: We most likely will want to hibernate connections to save
%% memory (there's a CPU/latency cost, but we don't need that snappy
%% of a protocol)
websocket_init(State = #state{orgname=OrgName, clientname=ClientName}) ->
    Key = make_client_gproc_key(State),
    gproc:reg({n, l, Key}),
    gproc:reg({c, l, connected_clients}, 1),

    lager:info("Registering listener ~p for ~p ~p", [self(), OrgName, ClientName]),
    erlang:start_timer(1000, self(), <<"SERVER_HEARTBEAT">>),
    ServerHelloMsg = make_server_hello(State),
    {reply, {text, ServerHelloMsg}, State}.

websocket_handle({text, Msg}, State = #state{orgname=OrgName, clientname=ClientName}) ->
    lager:info("Handling message ~p for ~p ~p: ~p", [self(), OrgName, ClientName, Msg]),
    {ok, State};
websocket_handle(_Data, State) ->
    {ok, State}.

%% We can subscribe to the pings and gather liveness info.

websocket_info({timeout, _Ref, Msg}, State) ->
    erlang:start_timer(60000, self(), <<"SERVER_HEARTBEAT">>),
    {reply, {text, Msg}, State};
websocket_info(_Info, State) ->
    {ok, State}.

%% Server HELLO should send checking interval (in seconds) and the GUID of the client.
make_server_hello(_State) ->
    <<"CONFIG:60:DEADBEEF">>.


make_client_gproc_key(#state{orgname=OrgName, clientname=ClientName}) ->
    {client_connection, {OrgName, ClientName}}.
