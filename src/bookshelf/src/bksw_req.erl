%% -*- erlang-indent-level: 4;indent-tabs-mode: nil; fill-column: 92 -*-
%% ex: ts=4 sw=4 et
%% @author Tim Dysinger <dysinger@chef.io>
%% Copyright 2012-2018 Chef Software, Inc.
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
    %% erlang:unique_integer/1 is unique during the lifetime of the node, i.e.,
    %% it reset on node restarts -- thus we add os:timestamp/0
    %% We can't use os:timestamp/0 only, since it's not guaranteed to be unique
    %% in multiple invocations.
    Id = term_to_binary({node(), os:timestamp(), erlang:unique_integer([positive])}),
    bksw_format:to_base64(Id).
