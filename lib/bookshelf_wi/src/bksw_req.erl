%% -*- erlang-indent-level: 4;indent-tabs-mode: nil; fill-column: 92 -*-
%% ex: ts=4 sw=4 et
%% @author Tim Dysinger <dysinger@opscode.com>
%% @copyright Copyright 2012 Opscode, Inc.
-module(bksw_req).

-export([with_amz_request_id/1, with_etag/2]).

-include_lib("cowboy/include/http.hrl").

%%===================================================================
%% API functions
%%===================================================================
with_amz_request_id(Rq) ->
    Id = term_to_binary({node(), erlang:now()}),
    Base64 = bksw_format:to_base64(Id),
    {ok, Rq2} =
        cowboy_http_req:set_resp_header(<<"x-amz-request-id">>,
                                        Base64, Rq),
    Rq2.

with_etag(Etag, Rq) ->
    {ok, Rq2} = cowboy_http_req:set_resp_header('Etag',
                                                Etag, Rq),
    Rq2.
