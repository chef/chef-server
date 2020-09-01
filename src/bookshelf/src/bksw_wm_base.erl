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
%% @author Tim Dysinger   <dysinger@chef.io>
%% @author Lincoln Baker  <lbaker@chef.io>

-module(bksw_wm_base).

%% Complete webmachine callbacks
-export([finish_request/2,
         init/1,
         is_authorized/2,
         malformed_request/2,
         service_available/2]).

%% Helper functions
-export([create_500_response/2]).

-ifdef(TEST).
-compile([export_all, nowarn_export_all]).
-endif.

-include("internal.hrl").

%%
%% Complete webmachine callbacks
%%

% why is this here?
-include_lib("eunit/include/eunit.hrl").

-define(CACHE_CTRL_MAXAGE, "300"  ).
-define(WEEK,               604800).    % number of seconds in 1 week

init(Config) ->
    {ok, bksw_conf:get_context(Config)}.

malformed_request(Req0, #context{auth_check_disabled=true} = Context) -> {false, Req0, Context};
malformed_request(Req0, #context{                        } = Context) ->
    HeadersRaw = mochiweb_headers:to_list(wrq:req_headers(Req0)),
    Headers    = process_headers(HeadersRaw),
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
                SignedHeaders = get_signed_headers(parse_x_amz_signed_headers(SignedHeaderKeysString), Headers, []),
                case check_signed_headers_common(SignedHeaders, Headers) of
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
                        % X-Amz-Expires is not used for authorization header-style authentication,
                        % but is still used to set cache-control max-age.
                        % https://docs.aws.amazon.com/AmazonS3/latest/API/sigv4-query-string-auth.html
                        XAmzExpiresString = ?CACHE_CTRL_MAXAGE,
                        SignedHeaders = get_signed_headers(parse_x_amz_signed_headers(SignedHeaderKeysString), Headers, []),
                        case check_signed_headers_authhead(SignedHeaders, Headers) of
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
            case parse_x_amz_credential(Credential) of
                {error,      _} -> throw({RequestId, Req1, Context});
                {ok, ParseCred} -> ParseCred
            end,

        % https://docs.aws.amazon.com/general/latest/gr/sigv4-date-handling.html
        DateIfUndefined = wrq:get_req_header("date", Req1),
        case {_,  Date} = get_check_date(XAmzDate, DateIfUndefined, CredentialScopeDate) of
            {error,  _} -> throw({RequestId, Req1, Context});
            {ok,     _} -> ok
        end,

        XAmzExpiresInt = list_to_integer(XAmzExpiresString),
        case XAmzExpiresInt > 1 andalso XAmzExpiresInt < ?WEEK of
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

% https://docs.aws.amazon.com/AmazonS3/latest/API/sig-v4-header-based-auth.html
-spec check_signed_headers_authhead(proplists:proplist(), proplists:proplist()) -> boolean().
check_signed_headers_authhead(SignedHeaders, Headers) ->
    check_signed_headers_common(SignedHeaders, Headers) andalso

    % x-amz-content-sha256 header is required
    proplists:is_defined("x-amz-content-sha256", SignedHeaders) andalso

    % if content-type header is present in request, it is required
    case proplists:is_defined("content-type", Headers) of
        true -> proplists:is_defined("content-type", SignedHeaders);
        _    -> true
    end.

% required signed headers common to both authorization header verification
% and presigned url verification.
% https://docs.amazonaws.cn/en_us/AmazonS3/latest/API/sigv4-query-string-auth.html
-spec check_signed_headers_common(proplists:proplist(), proplists:proplist()) -> boolean().
check_signed_headers_common(SignedHeaders, Headers) ->
    % host header is required
    proplists:is_defined("host", SignedHeaders) andalso

    % any x-amz-* headers present in request are required
    [] == [Key || {Key, _} <- Headers, is_amz(Key), not proplists:is_defined(Key, SignedHeaders)].

% https://docs.aws.amazon.com/general/latest/gr/sigv4-date-handling.html
-spec get_check_date(ISO8601Date::string() | undefined, DateIfUndefined::string(), string()) -> {ok, string()} | {error, get_check_date}.
get_check_date(ISO8601Date, DateIfUndefined, [Y1, Y2, Y3, Y4, M1, M2, D1, D2]) ->
    Date = case ISO8601Date of
               undefined -> DateIfUndefined;
               _         -> ISO8601Date
           end,
    case Date of
        [Y1, Y2, Y3, Y4, M1, M2, D1, D2, $T, _, _, _, _, _, _, $Z] -> {ok, Date};
        _                                                          -> {error, get_check_date}
    end.

% @doc get key-value pairs (headers) associated with specified keys.
% for each key, get first occurance of key-value. for duplicated
% keys, get corresponding key-value pairs. results are undefined
% for nonexistent key(s).
%-spec get_signed_headers(proplist(), proplist(), proplist()) -> proplist(). % for erlang20+
-spec get_signed_headers(SignedHeaderKeys::[string()], Headers::[tuple()], SignedHeaders::[tuple()]) -> [tuple()].
get_signed_headers([], _, SignedHeaders) -> lists:reverse(SignedHeaders);
get_signed_headers(_, [], SignedHeaders) -> lists:reverse(SignedHeaders);
get_signed_headers([Key | SignedHeaderKeys], Headers0, SignedHeaders) ->
    {_, SignedHeader, Headers} = lists:keytake(Key, 1, Headers0),
    get_signed_headers(SignedHeaderKeys, Headers, [SignedHeader | SignedHeaders]).

-spec is_amz(string()) -> boolean().
is_amz([$x, $-, $a, $m, $z, $- | _]) ->
    true;
is_amz([$X, $-, $A, $m, $z, $- | _]) ->
    true;
is_amz(_) ->
    false.

% @doc split credentials string into component parts
% Cred = "<access-key-id>/<date>/<AWS-region>/<AWS-service>/aws4_request"
-spec parse_x_amz_credential(string()) -> {ok, [string()]} | {error, parse_x_amz_credential}.
parse_x_amz_credential(Cred) ->
    Parse = string:split(Cred, "/", all),
    case Parse of
        [_access_key_id, _date, _aws_region, "s3", "aws4_request"] -> {ok, Parse};
        _                                                          -> {error, parse_x_amz_credential}
    end.

% @doc split signed header string into component parts. return empty string on empty string.
% Headers = "<header1>;<header2>;...<headerN>"
-spec parse_x_amz_signed_headers(string()) -> [string()].
parse_x_amz_signed_headers(Headers) ->
   string:split(Headers, ";", all).

% @doc convert the keys of key-value pairs to lowercase strings
-spec process_headers(Headers::[tuple()]) -> [tuple()].
process_headers(Headers) ->
    [{string:casefold(
        case is_atom(Key) of
            true -> atom_to_list(Key);
            _    -> Key
        end), Val} || {Key, Val} <- Headers].

create_500_response(Rq0, _Ctx) ->
    %% sanitize response body
    Msg = <<"internal service error">>,
    Rq1 = wrq:set_resp_header("Content-Type",
                               "text/plain", Rq0),
    wrq:set_resp_body(Msg, Rq1).
