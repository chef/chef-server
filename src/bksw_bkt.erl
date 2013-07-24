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
-module(bksw_bkt).

-export([init/1, is_authorized/2, allowed_methods/2, content_types_provided/2,
         content_types_accepted/2,
         finish_request/2,
         service_available/2,
         resource_exists/2, delete_resource/2, create_resource/2, to_xml/2]).

-include("amazon_s3.hrl").
-include_lib("webmachine/include/webmachine.hrl").

%%===================================================================
%% Public API
%%===================================================================
init(Config) ->
    {ok, bksw_conf:get_context(Config)}.

is_authorized(Rq, Ctx) ->
    bksw_sec:is_authorized(Rq, Ctx).

allowed_methods(Rq, Ctx) ->
    {['GET', 'PUT', 'DELETE'], Rq, Ctx}.

content_types_accepted(Rq, Ctx) ->
    CType =
        case wrq:get_req_header("Content-Type", Rq) of
            undefined ->
                "text/xml";
            C ->
                C
        end,
    {[{CType, create_resource}], Rq, Ctx}.

content_types_provided(Rq, Ctx) ->
    CType =
        case wrq:get_req_header("accept", Rq) of
            undefined ->
                "text/xml";
            C ->
                C
        end,
    {[{CType, to_xml}], Rq, Ctx}.

service_available(Req, Ctx) ->
    bksw_util:service_available(Req, Ctx).

resource_exists(Rq0, Ctx) ->
    Bucket = bksw_util:get_bucket(Rq0),
    {bksw_io:bucket_exists(Bucket), Rq0, Ctx}.

delete_resource(Rq0, Ctx) ->
    Bucket = bksw_util:get_bucket(Rq0),
    {bksw_io:bucket_delete(Bucket), Rq0, Ctx}.

create_resource(Rq0, Ctx) ->
    Bucket = bksw_util:get_bucket(Rq0),
    {bksw_io:bucket_create(Bucket), Rq0, Ctx}.

to_xml(Rq0, Ctx) ->
    Bucket = bksw_util:get_bucket(Rq0),
    Objects = bksw_io:entry_list(Bucket),
    Term = bksw_xml:list_objects(Bucket, Objects),
    Body = bksw_xml:write(Term),
    {Body, Rq0, Ctx}.

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

%%===================================================================
%% Internal API
%%===================================================================
