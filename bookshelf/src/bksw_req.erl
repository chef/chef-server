%% -*- erlang-indent-level: 4;indent-tabs-mode: nil; fill-column: 92 -*-
%% ex: ts=4 sw=4 et
%% @author Tim Dysinger <dysinger@opscode.com>
%% Copyright 2012 Opscode, Inc. All Rights Reserved.
%%
%% This file is provided to you under the Apache License,
%% Version 2.0 (the "License"); you may not use this file
%% except in compliance with the License.  You may obtain
%% a copy of the License at
%%
%%   http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing,
%% software distributed under the License is distributed on an
%% "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
%% KIND, either express or implied.  See the License for the
%% specific language governing permissions and limitations
%% under the License.
%%

-module(bksw_req).

-export([with_amz_request_id/1,
         with_amz_id_2/1,
         with_etag/2]).

%%===================================================================
%% API functions
%%===================================================================
with_amz_request_id(Rq) ->
    Base64 = generate_id(),
    {Base64, wrq:set_resp_header("x-amz-request-id", Base64, Rq)}.

with_amz_id_2(Rq) ->
    Base64 = generate_id(),
    wrq:set_resp_header("x-amz-id-2", Base64, Rq).

with_etag(Etag, Rq) ->
    wrq:set_resp_header("Etag", bksw_util:to_string(Etag), Rq).


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
