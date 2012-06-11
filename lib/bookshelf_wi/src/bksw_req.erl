%% -*- erlang-indent-level: 4;indent-tabs-mode: nil; fill-column: 92 -*-
%% ex: ts=4 sw=4 et
%% @author Tim Dysinger <dysinger@opscode.com>
%% @copyright Copyright 2012 Opscode, Inc.
-module(bksw_req).

-export([with_amz_request_id/1,
         with_amz_id_2/1,
         with_etag/2]).

-include_lib("cowboy/include/http.hrl").

%%===================================================================
%% API functions
%%===================================================================
with_amz_request_id(Rq) ->
    Base64 = generate_id(),
    {ok, Rq2} =
        cowboy_http_req:set_resp_header(<<"x-amz-request-id">>,
                                        Base64, Rq),
    {Base64, Rq2}.

with_amz_id_2(Rq) ->
    Base64 = generate_id(),
    {ok, Rq2} =
        cowboy_http_req:set_resp_header(<<"x-amz-id-2">>,
                                        Base64, Rq),
    Rq2.

with_etag(Etag, Rq) ->
    {ok, Rq2} = cowboy_http_req:set_resp_header('Etag',
                                                Etag, Rq),
    Rq2.

%%===================================================================
%% Internal Functions
%%===================================================================
generate_id() ->
    %% According to the docs erlang/new will always return a unique value on on the same
    %% node. There is still some small opportunity here for there to be unconnected nodes
    %% with the same node name that call erlang:now/0 in the same microsecond. However, that
    %% is both unlikely enough and low impact enough that I dont think its worth adding
    %% extra to this.

    Id = term_to_binary({node(), erlang:now()}),
    bksw_format:to_base64(Id).
