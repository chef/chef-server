-module(sky_app).
-behaviour(application).

-export([start/2]).
-export([stop/1]).

start(_Type, _Args) ->
    Dispatch = cowboy_router:compile([
                                      {'_', [{"/", sky_generic_handler, []},
                                             %% Note: Investigate constraints here.
                                             {"/organizations/:orgname/websocket/:clientname", sky_websocket_handler, []}
                                            ]}
                                     ]),
    {ok, _} = cowboy:start_clear(my_http_listener, 100,
                                 [{port, 8080}],
                                 #{env => #{dispatch => Dispatch}}
                                ),
    sky_sup:start_link().

stop(_State) ->
	ok.
