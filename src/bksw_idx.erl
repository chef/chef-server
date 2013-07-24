%% -*- erlang-indent-level: 4;indent-tabs-mode: nil; fill-column: 92 -*-
%% ex: ts=4 sw=4 et
%% @author Eric B Merritt <ericbmerritt@gmail.com>
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

-module(bksw_idx).

-export([init/1, is_authorized/2, allowed_methods/2, content_types_provided/2,
         finish_request/2,
         resource_exists/2, to_xml/2,
         service_available/2]).

-include_lib("webmachine/include/webmachine.hrl").
%%===================================================================
%% Public API
%%===================================================================

init(Config) ->
    {ok, bksw_conf:get_context(Config)}.

is_authorized(Rq, Ctx) ->
    bksw_sec:is_authorized(Rq, Ctx).

allowed_methods(Rq, Ctx) ->
    {['GET'], Rq, Ctx}.

content_types_provided(Rq, Ctx) ->
    {[{"text/xml", to_xml}], Rq, Ctx}.

service_available(Req, Ctx) ->
    bksw_util:service_available(Req, Ctx).

resource_exists(Rq, Ctx) ->
    {erlang:is_list(bksw_io:bucket_list()), Rq, Ctx}.

to_xml(Rq, Ctx) ->
    Buckets = bksw_io:bucket_list(),
    Term = bksw_xml:list_buckets(Buckets),
    Body = bksw_xml:write(Term),
    {Body, Rq, Ctx}.

finish_request(Rq0, Ctx) ->
    try
        case wrq:response_code(Rq0) of
            500 ->
                Rq1 = create_500_response(Rq0, Ctx),
                {true, Rq1, Ctx};
            _ ->
                {true, Rq0, Ctx}
        end
    catch
        X:Y ->
            error_logger:error_report({X, Y, erlang:get_stacktrace()})
    end.

create_500_response(Rq0, _Ctx) ->
    %% sanitize response body
    Msg = <<"internal service error">>,
    Rq1 = wrq:set_resp_header("Content-Type",
                               "text/plain", Rq0),
    wrq:set_resp_body(Msg, Rq1).
