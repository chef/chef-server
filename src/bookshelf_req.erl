%% @copyright 2012 Opscode, Inc. All Rights Reserved
%% @author Tim Dysinger <dysinger@opscode.com>
%%
%% Licensed to the Apache Software Foundation (ASF) under one or more
%% contributor license agreements.  See the NOTICE file distributed
%% with this work for additional information regarding copyright
%% ownership.  The ASF licenses this file to you under the Apache
%% License, Version 2.0 (the "License"); you may not use this file
%% except in compliance with the License.  You may obtain a copy of
%% the License at http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or
%% implied.  See the License for the specific language governing
%% permissions and limitations under the License.

-module(bookshelf_req).
-include("bookshelf.hrl").
-export([
         fingerprint/0,
         to_base64/1,
         to_hex/1,
         with_amz_request_id/1,
         with_etag/2
        ]).

fingerprint() ->
    term_to_binary({node(), erlang:now()}).

to_base64(Bin) ->
    base64:encode_to_string(Bin).

to_hex(Bin) ->
    string:to_lower(
      lists:flatten([io_lib:format("~2.16.0b",[N]) || <<N>> <= Bin])
     ).

with_amz_request_id(Rq) ->
    Id        = to_base64(fingerprint()),
    {ok, Rq2} = cowboy_http_req:set_resp_header(<<"x-amz-request-id">>, Id, Rq),
    Rq2.

with_etag(Etag, Rq) ->
    {ok, Rq2} = cowboy_http_req:set_resp_header('Etag', Etag, Rq),
    Rq2.
