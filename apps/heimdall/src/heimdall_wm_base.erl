-module(heimdall_wm_base).

-export([content_types_accepted/2,
         content_types_provided/2,
         finish_request/2,
         forbidden/2,
         malformed_request/2,
         ping/2,
         post_is_create/2,
         service_available/2]).

-include("heimdall_wm.hrl").

ping(Req, State) ->
    {pong, Req, State}.

service_available(Req, State) ->
    {true, Req, State}.

post_is_create(Req, State) ->
    {true, Req, State}.

%% This is a stub for now
malformed_request(Req, State) ->
    {false, Req, State}.

%% This is a stub for now
forbidden(Req, State) ->
    {false, Req, State}.

content_types_accepted(Req, State) ->
    {[{"application/json", from_json}], Req, State}.

content_types_provided(Req, State) ->
    {[{"application/json", to_json}], Req, State}.

finish_request(Req, State) ->
    {true, Req, State}.
