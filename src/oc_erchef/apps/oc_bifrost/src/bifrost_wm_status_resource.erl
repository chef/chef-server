-module(bifrost_wm_status_resource).

-export([
         allowed_methods/2,
         content_types_provided/2,
         init/1,
         to_json/2
        ]).

-include_lib("webmachine/include/webmachine.hrl").

init(_State) ->
    DummyState = [], %% <-- make that "real" later
    {ok, DummyState}.

allowed_methods(Req, State) ->
    {['GET'], Req, State}.

content_types_provided(Req, State) ->
    {[{"application/json", to_json}], Req, State}.

to_json(Req, State) ->
    case bifrost_db:ping() of
        ok ->
            {<<"{\"status\": \"ok\"}">>, Req, State};
        {error, Reason} ->
            lager:error(io_lib:format("status check: ~999p~n", [Reason])),
            {{halt, 500}, Req, State}
    end.
