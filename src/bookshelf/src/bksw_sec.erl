%% -*- erlang-indent-level: 4;indent-tabs-mode: nil; fill-column: 92 -*-
%% ex: ts=4 sw=4 et
%% @author Eric B Merritt <ericbmerritt@gmail.com>
%% @author Lincoln Baker <lbaker@chef.io>
%% Copyright 2020 Chef Software, Inc.
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

-module(bksw_sec).

-export([encode_access_denied_error_response/3]).
-export([get_check_date/3                     ]).
-export([get_signed_headers/3                 ]).
-export([is_authorized/2                      ]).
-export([parse_authorization/1                ]).
-export([parse_x_amz_credential/1             ]).
-export([parse_x_amz_signed_headers/1         ]).
-export([process_headers/1                    ]).

-ifdef(TEST).
-compile([export_all, nowarn_export_all]).
-endif.

% is the necessary?  try removing.
-include("internal.hrl").

%%===================================================================
%% API functions
%%===================================================================
-include_lib("eunit/include/eunit.hrl").
is_authorized(Req0, #context{auth_check_disabled=true} = Context) ->
    {true, Req0, Context};
is_authorized(Req0, #context{                        } = Context) ->
    Headers = mochiweb_headers:to_list(wrq:req_headers(Req0)),
    {RequestId, Req1} = bksw_req:with_amz_request_id(Req0),
    case proplists:get_value('Authorization', Headers, undefined) of
        undefined ->
            % presigned url verification
            % https://docs.aws.amazon.com/AmazonS3/latest/API/sigv4-query-string-auth.html
            IncomingSignature = wrq:get_qs_value("X-Amz-Signature", "", Req0),
            auth(RequestId, Req1, Context, IncomingSignature, Headers, presigned_url);
        IncomingAuth ->
            % authorization header verification
            % https://docs.aws.amazon.com/AmazonS3/latest/API/sigv4-auth-using-authorization-header.html
            case parse_authorization(IncomingAuth) of
                {ok, [_Credential, _SignedHeaderKeysString, IncomingSignature]} ->
                    auth(RequestId, Req1, Context, IncomingSignature, Headers, auth_header);
                _ ->
                    encode_access_denied_error_response(RequestId, Req1, Context)
            end
    end.

auth(RequestId, Req0, #context{reqid                  = ReqId,
                               aws_access_key_id      = AWSAccessKeyId,
                               date                   = Date,
                               region                 = Region,
                               signed_header_keys_str = SignedHeaderKeysString,
                               x_amz_expires_int      = XAmzExpiresInt,
                               x_amz_expires_str      = XAmzExpiresString} = Context, IncomingSignature, Headers0, VerificationType) ->
    try
        AccessKey     = bksw_conf:access_key_id(Context),
        SecretKey     = bksw_conf:secret_access_key(Context),
        Config        = mini_s3:new(AccessKey, SecretKey, host(Req0)),
        Headers       = process_headers(Headers0),
        SignedHeaders = get_signed_headers(parse_x_amz_signed_headers(SignedHeaderKeysString), Headers, []),
        Method        = list_to_atom(string:to_lower(erlang:atom_to_list(wrq:method(Req0)))),
        Path          = wrq:path(Req0),

        % replace host header with alternate host header
        AltSignedHeaders = [case {K, V} of {"host", _} -> {"host", alt_host(host(Req0), Config)}; _ -> {K, V} end || {K, V} <- SignedHeaders],

        CalculatedSig = case VerificationType of
            presigned_url ->

                case wrq:get_qs_value("X-Amz-Algorithm", Req0) of
                    "AWS4-HMAC-SHA256" -> ok;
                    _                  -> throw({RequestId, Req0, Context})
                end,

                case check_signed_headers_common(SignedHeaders, Headers) of
                    true -> ok;
                    _    -> throw({RequestId, Req0, Context})
                end,

                {Bucketname, Key} = get_bucket_key(Path),

                ComparisonURL = mini_s3:s3_url(Method, Bucketname, Key, XAmzExpiresInt, SignedHeaders, Date, Config),

                % list_to_binary profiled faster than binary_to_list,
                % so use that for conversion and comparison.
                IncomingSig = list_to_binary(IncomingSignature),

                [_, ComparisonSig] = string:split(ComparisonURL, "&X-Amz-Signature=", trailing),

                % TODO: try to remove alt sig computation and see what happens
                % NOTE: this was tried, and caused compilation and test failures.

                case IncomingSig of
                    ComparisonSig ->
                        %AltComparisonSig = "not computed",
                        IncomingSig;
                    _ ->
                        AltComparisonURL = mini_s3:s3_url(Method, Bucketname, Key, XAmzExpiresInt, AltSignedHeaders, Date, Config),
                        [_, AltComparisonSig] = string:split(AltComparisonURL, "&X-Amz-Signature=", all),
                        AltComparisonSig
                end;
            auth_header ->

                ComparisonURL = "not-applicable",
                QueryParams = wrq:req_qs(Req0),

                case check_signed_headers_authhead(SignedHeaders, Headers) of
                    true -> ok;
                    _    -> throw({RequestId, Req0, Context})
                end,

                SigV4Headers = erlcloud_aws:sign_v4(Method, Path, Config, SignedHeaders, <<>>, Region, "s3", QueryParams, Date),

                IncomingSig = IncomingSignature,

                ComparisonSig = parseauth_or_throw(proplists:get_value("Authorization", SigV4Headers, ""), {RequestId, Req0, Context}),

                % TODO: try to remove alt sig computation and see what happens
                % NOTE: this was tried, and caused compilation and test failures.

                case IncomingSig of
                    ComparisonSig ->
                        %AltComparisonSig = "not computed",
                        IncomingSig;
                    _ ->
                        AltSigV4Headers   = erlcloud_aws:sign_v4(Method, Path, Config, AltSignedHeaders, <<>>, Region, "s3", QueryParams, Date),
                        _AltComparisonSig = parseauth_or_throw(proplists:get_value("Authorization", AltSigV4Headers, ""), {RequestId, Req0, Context})
                end
        end,

        case IncomingSig of
            CalculatedSig ->
                case is_expired(Date, XAmzExpiresInt) of
                    true ->
                        ?LOG_DEBUG("req_id=~p expired signature (~p) for ~p", [ReqId, XAmzExpiresInt, Path]),
                        encode_access_denied_error_response(RequestId, Req0, Context);
                    false ->
                        case erlang:iolist_to_binary(AWSAccessKeyId) == erlang:iolist_to_binary(AccessKey) of
                            true ->
                                MaxAge = "max-age=" ++ XAmzExpiresString,
                                Req1 = wrq:set_resp_header("Cache-Control", MaxAge, Req0),
                                {true, Req1, Context};
                            false ->
                                ?LOG_DEBUG("req_id=~p signing error for ~p", [ReqId, Path]),
                                encode_sign_error_response(AWSAccessKeyId, IncomingSignature, RequestId,
                                                           ComparisonURL, Req0, Context)
                        end
                end;
            _ ->
                encode_access_denied_error_response(RequestId, Req0, Context)
        end

    catch
        throw:{RequestId, Req, Context} -> encode_access_denied_error_response(RequestId, Req, Context)
    end.

-spec alt_host(string(), tuple()) -> string().
alt_host(Host, Config) ->
    mini_s3:get_host_toggleport(Host, Config).

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

% split  "<bucketname>/<key>" (possibly leading and/or trailing /) into {"bucketname", "key"}
% Path = "<bucketname>/<key>"
-spec get_bucket_key(Path::string()) -> {string(), string()}.
get_bucket_key(Path) ->
    case string:lexemes(Path, "/") of
        [            ] -> {"",     ""};
        [Bucket      ] -> {Bucket, ""};
        [Bucket | Key] -> {Bucket, filename:join(Key)}
    end.

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

-spec host(tuple()) -> list().
host(Req0) ->
    wrq:get_req_header("Host", Req0).

% @doc split authorization header into component parts
% https://docs.aws.amazon.com/AmazonS3/latest/API/sigv4-auth-using-authorization-header.html
-spec parse_authorization(string()) -> {ok, [string()]} | {error, parse_authorization}.
parse_authorization(Auth) ->
    case string:split(Auth, " ", all) of
        ["AWS4-HMAC-SHA256", "Credential="++Cred, "SignedHeaders="++SigHead, "Signature="++Signature] ->
            {ok, [string:trim(Cred, trailing, ","), string:trim(SigHead, trailing, ","), Signature]};
        _ ->
            {error, parse_authorization}
    end.

-spec parseauth_or_throw(string(), tuple()) -> string() | no_return().
parseauth_or_throw(Auth, Throw) ->
    case parse_authorization(Auth) of
        {ok, [_, _, Sig]} -> Sig;
        _                 -> throw(Throw)
    end.

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

% most ways of getting the date/time seem problematic.  for instance, docs for
% calendar:universal_time() and erlang:universaltime() say: 'Returns local time
% if universal time is unavailable.'
% since it is unknown which time would be used, we could use local time and
% convert to universal.  however, local time could be an 'illegal' time wrt
% universal time if switching to/from daylight savings time.
%
% note that we are neither measuring an elapsed time, nor determining an order
% of events, nor creating a unique name, so time correction should not be an
% issue.
% https://erlang.org/doc/apps/erts/time_correction.html
% 1 =< ExpiresSec =< 604800
-spec is_expired(DateTimeString::string(), ExpiresSec::integer()) -> boolean().
is_expired(DateTimeString, ExpiresSec) ->
    [Y1, Y2, Y3, Y4, M1, M2, D1, D2, _, H1, H2, N1, N2, S1, S2, _] = DateTimeString,
    Year    = list_to_integer([Y1, Y2, Y3, Y4]),
    Month   = list_to_integer([M1, M2        ]),
    Day     = list_to_integer([D1, D2        ]),
    Hour    = list_to_integer([H1, H2        ]),
    Min     = list_to_integer([N1, N2        ]),
    Sec     = list_to_integer([S1, S2        ]),

    % this could be used to check if the date constructed is valid
    % calendar:valid_date({{Year, Month, Day}, {Hour, Min, Sec}}),

    DateSeconds = calendar:datetime_to_gregorian_seconds({{Year, Month, Day}, {Hour, Min, Sec}}),
    UniversalTimeSec = calendar:datetime_to_gregorian_seconds(calendar:now_to_universal_time(os:timestamp())),
    DateSeconds + ExpiresSec < UniversalTimeSec.

encode_sign_error_response(AccessKeyId, Signature,
                           RequestId, StringToSign, Req0,
                          Context) ->
    Req1 = bksw_req:with_amz_id_2(Req0),
    Body = bksw_xml:signature_does_not_match_error(
             RequestId, bksw_util:to_string(Signature),
             bksw_util:to_string(StringToSign),
             bksw_util:to_string(AccessKeyId)),
    Req2 = wrq:set_resp_body(Body, Req1),
    {{halt, 403}, Req2, Context}.

encode_access_denied_error_response(RequestId, Req0, Context) ->
    Req1 = bksw_req:with_amz_id_2(Req0),
    Body = bksw_xml:access_denied_error(RequestId),
    Req2 = wrq:set_resp_body(Body, Req1),
    {{halt, 403}, Req2, Context}.

-spec is_amz(string()) -> boolean().
is_amz([$x, $-, $a, $m, $z, $- | _]) ->
    true;
is_amz([$X, $-, $A, $m, $z, $- | _]) ->
    true;
is_amz(_) ->
    false.
