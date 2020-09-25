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
%% @author Ho-Sheng Hsiao <hosh@chef.io>
%% @author Tim Dysinger <dysinger@chef.io>

-module(bksw_wm_base).

%% Complete webmachine callbacks
-export([finish_request/2,
         init/1,
         is_authorized/2,
         malformed_request/2,
         service_available/2]).

%% Helper functions
-export([create_500_response/2]).

-include("internal.hrl").

%%
%% Complete webmachine callbacks
%%

% why is this here?
-include_lib("eunit/include/eunit.hrl").

init(Config) ->
    {ok, bksw_conf:get_context(Config)}.

% seconds in 5 minutes and 1 week
-define(MIN5, "300"  ).
-define(WEEK1, 604800).

malformed_request(Req0, #context{auth_check_disabled=true} = Context) -> {false, Req0, Context};
malformed_request(Req0, #context{                        } = Context) ->
    HeadersRaw = mochiweb_headers:to_list(wrq:req_headers(Req0)),
    Headers    = bksw_sec:process_headers(HeadersRaw),
    {RequestId, Req1} = bksw_req:with_amz_request_id(Req0),
    try
        case proplists:get_value('Authorization', HeadersRaw, undefined) of
            undefined ->
                % presigned url verification
                % https://docs.aws.amazon.com/AmazonS3/latest/API/sigv4-query-string-auth.html
                AuthType = presigned_url,
                [XAmzAlgorithm, Credential, XAmzDate, SignedHeaderKeysString, XAmzExpiresString, IncomingSignature] =
                    [wrq:get_qs_value(X, "", Req1) || X <- ["X-Amz-Algorithm", "X-Amz-Credential", "X-Amz-Date", "X-Amz-SignedHeaders", "X-Amz-Expires", "X-Amz-Signature"]],
                case XAmzAlgorithm of
                    "AWS4-HMAC-SHA256" -> ok;
                    _                  -> throw({RequestId, Req1, Context})
                end,
                SignedHeaders = bksw_sec:get_signed_headers(bksw_sec:parse_x_amz_signed_headers(SignedHeaderKeysString), Headers, []),
                case bksw_sec:check_signed_headers_common(SignedHeaders, Headers) of
                    true -> ok;
                    _    -> throw({RequestId, Req1, Context})
                end;
            IncomingAuth ->
                % authorization header verification
                % https://docs.aws.amazon.com/AmazonS3/latest/API/sigv4-auth-using-authorization-header.html
                case bksw_sec:parse_authorization(IncomingAuth) of
                    {ok, [Credential, SignedHeaderKeysString, IncomingSignature]} ->
                        AuthType = auth_header,
                        XAmzDate = wrq:get_req_header("x-amz-date", Req1),
                        XAmzExpiresString = ?MIN5,
                        SignedHeaders = bksw_sec:get_signed_headers(bksw_sec:parse_x_amz_signed_headers(SignedHeaderKeysString), Headers, []),
                        case bksw_sec:check_signed_headers_authhead(SignedHeaders, Headers) of
                            true -> ok;
                            _    -> throw({RequestId, Req1, Context})
                        end;
                    _ ->
                        {AuthType, Credential, XAmzDate, SignedHeaders, XAmzExpiresString, IncomingSignature} =
                            {err, err, err, err, err, err},
                        throw({RequestId, Req1, Context})
                end
        end,

        case wrq:get_req_header("Host", Req1) of
            undefined -> throw({RequestId, Req1, Context});
            _         -> ok
        end,

        [AWSAccessKeyId, CredentialScopeDate, Region | _] =
            case bksw_sec:parse_x_amz_credential(Credential) of
                {error,      _} -> throw({RequestId, Req1, Context});
                {ok, ParseCred} -> ParseCred
            end,

        % https://docs.aws.amazon.com/general/latest/gr/sigv4-date-handling.html
        DateIfUndefined = wrq:get_req_header("date", Req1),
        case {_,  Date} = bksw_sec:get_check_date(XAmzDate, DateIfUndefined, CredentialScopeDate) of
            {error,  _} -> throw({RequestId, Req1, Context});
            {ok,     _} -> ok
        end,

        XAmzExpiresInt = list_to_integer(XAmzExpiresString),
        case XAmzExpiresInt > 1 andalso XAmzExpiresInt < ?WEEK1 of
            true -> ok;
            _    -> throw({RequestId, Req1, Context})
        end,

        {false, Req1, Context#context{
           aws_access_key_id      = AWSAccessKeyId,
           auth_type              = AuthType,
           date                   = Date,
           incoming_sig           = IncomingSignature,
           region                 = Region,
           signed_headers         = SignedHeaders,
           x_amz_expires_int      = XAmzExpiresInt,
           x_amz_expires_str      = XAmzExpiresString}}
    catch
        throw:{RequestId, Req1, Context} ->
            bksw_sec:encode_access_denied_error_response(RequestId, Req1, Context)
    end.

is_authorized(Rq, Ctx) ->
    bksw_sec:is_authorized(Rq, Ctx).

finish_request(Rq0, Ctx) ->
    try
        case wrq:response_code(Rq0) of
            500 ->
                Rq1 = create_500_response(Rq0, Ctx),
                {true, Rq1, Ctx};
            %% Ensure we don't tell upstream servers to cache 404s
            C when C >= 400 andalso C < 500 ->
                Rq1 = wrq:remove_resp_header("Cache-Control", Rq0),
                {true, Rq1, Ctx};
            _ ->
                {true, Rq0, Ctx}
        end
    catch
        X:Y:Stacktrace ->
            error_logger:error_report({X, Y, Stacktrace})
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
