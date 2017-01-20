-module(server_connect_integration_tests).

-include_lib("eunit/include/eunit.hrl").
%-include_lib("hoax/include/hoax.hrl").

-compile([export_all]).

%server_test_() ->
%    [
%     hoax:fixture(?MODULE, "server_")
%    ].

%server_starts_and_stops_gen_test_() ->
%    [ fun server_starts_and_stops/0 ].


server_starts_and_stops_test() ->
    start_server(),
    %% try connecting
    ?assert(1 == 2),
    %% cleanup
    stop_server().


%% helpers to manage the server
start_server() ->
    
    sky_app:start([], []).


stop_server() ->
    sky_app:stop([]).
 
