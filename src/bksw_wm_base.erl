%% Copyright 2012-2013 Opscode, Inc. All Rights Reserved.
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
%% @author Ho-Sheng Hsiao <hosh@opscode.com>
%% @author Tim Dysinger <dysinger@opscode.com>

-module(bksw_wm_base).

%% Complete webmachine callbacks
-export([init/1,
         is_authorized/2,
         finish_request/2,
         service_available/2]).

%% Helper functions
-export([create_500_response/2]).

-include("internal.hrl").

%%
%% Complete webmachine callbacks
%%

init(Config) ->
    {ok, bksw_conf:get_context(Config)}.

is_authorized(Rq, Ctx) ->
    bksw_sec:is_authorized(Rq, Ctx).

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

service_available(Req, #context{reqid_header_name = HeaderName} = State) ->
    %% Extract or generate a request id
    ReqId = oc_wm_request:read_req_id(HeaderName, Req),

    %% If no UserId is generated, this will return undefined. The opscoderl_wm request
    %% logger will omit user=; downstream.
    UserId = wrq:get_req_header("x-ops-userid", Req),

    Req0 = oc_wm_request:add_notes([{req_id, ReqId},
                                    {user, UserId}], Req),

    {true, Req0, State#context{reqid = ReqId}}.

%%
%% Helper functions
%%

create_500_response(Rq0, _Ctx) ->
    %% sanitize response body
    Msg = <<"internal service error">>,
    Rq1 = wrq:set_resp_header("Content-Type",
                               "text/plain", Rq0),
    wrq:set_resp_body(Msg, Rq1).
