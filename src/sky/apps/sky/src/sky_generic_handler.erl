-module(sky_generic_handler).

-export([init/2]).
-export([handle/2]).
-export([terminate/3]).

-record(state, { }).

init(Req0, State) ->
    Req = cowboy_req:reply(200,
                           #{<<"content-type">> => <<"text/plain">>},
                           <<"Hello Erlang!">>,
                           Req0),
    {ok, Req, State}.

handle(Req, State=#state{}) ->
	{ok, Req2} = cowboy_req:reply(200, Req),
	{ok, Req2, State}.

terminate(_Reason, _Req, _State) ->
	ok.
