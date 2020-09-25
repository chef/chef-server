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

-export([check_signed_headers_authhead/2      ]).
-export([check_signed_headers_common/2        ]).
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

% until erlang gets a shorthand way to access a key's value
-define(ACCESSKEY(Auth),          maps:get(accesskey,          Auth)).
-define(CONFIG(Auth),             maps:get(config,             Auth)).
-define(ALT_SIGNED_HEADERS(Auth), maps:get(alt_signed_headers, Auth)).
-define(METHOD(Auth),             maps:get(method,             Auth)).
-define(PATH(Auth),               maps:get(path,               Auth)).

% for instrumentation - REMOVE
-include_lib("eunit/include/eunit.hrl").

%%===================================================================
%% API functions
%%===================================================================

is_authorized(Req0, #context{auth_check_disabled=true} = Context) -> {true, Req0, Context};
is_authorized(Req0, #context{auth_type                 = presigned_url,
                             date                      = Date,
                             incoming_sig              = IncomingSignature,
                             signed_headers            = SignedHeaders,
                             x_amz_expires_int         = XAmzExpiresInt} = Context) ->
?debugFmt("~nIN is_authorized presigned_url", []),
    {RequestId, Req1} = bksw_req:with_amz_request_id(Req0),
    try
        Auth               = auth_init(Req1, Context, SignedHeaders),
        {Bucketname, Key}  = get_bucket_key(?PATH(Auth)),
        ComparisonURL      = mini_s3:s3_url(?METHOD(Auth), Bucketname, Key, XAmzExpiresInt, SignedHeaders, Date, ?CONFIG(Auth)),
        IncomingSig        = list_to_binary(IncomingSignature),
        [_, ComparisonSig] = string:split(ComparisonURL, "&X-Amz-Signature=", trailing),
?debugFmt("~nauth_type: presigned_url", []),
?debugFmt("~nDate: ~p", [Date]),
?debugFmt("~nIncomingSignature: ~p", [IncomingSignature]),
?debugFmt("~nSignedHeaders: ~p", [SignedHeaders]),
?debugFmt("~nXAmzExpiresInt: ~p", [XAmzExpiresInt]),
?debugFmt("~nAuth: ~p", [Auth]),
?debugFmt("~nBucketname: ~p", [Bucketname]),
?debugFmt("~nKey: ~p", [Key]),
?debugFmt("~nComparisonURL: ~p", [ComparisonURL]),
?debugFmt("~nIncomingSig: ~p", [IncomingSig]),
?debugFmt("~nComparisonSig: ~p", [ComparisonSig]),
        % TODO: try to remove alt sig computation and see what happens
        % NOTE: this was tried, and caused compilation and test failures.

        CalculatedSig =
            case IncomingSig of
                ComparisonSig ->
                    %AltComparisonSig = "not computed",
                    IncomingSig;
                _ ->
                    AltComparisonURL = mini_s3:s3_url(?METHOD(Auth), Bucketname, Key, XAmzExpiresInt, ?ALT_SIGNED_HEADERS(Auth), Date, ?CONFIG(Auth)),
                    [_, AltComparisonSig] = string:split(AltComparisonURL, "&X-Amz-Signature=", all),
                    AltComparisonSig
            end,
?debugFmt("~nCalculatedSig: ~p", [CalculatedSig]),
    auth_finish(RequestId, Req1, Context, Auth, ComparisonURL, IncomingSig, CalculatedSig)
    catch
        throw:{RequestId, Req, Context} -> encode_access_denied_error_response(RequestId, Req, Context)
    end;
is_authorized(Req0, #context{auth_type                 = auth_header,
                             date                      = Date,
                             incoming_sig              = IncomingSignature,
                             region                    = Region,
                             signed_headers            = SignedHeaders} = Context) ->
?debugFmt("~nIN is_authorized auth_header", []),
    {RequestId, Req1} = bksw_req:with_amz_request_id(Req0),
    try
        Auth          = auth_init(Req1, Context, SignedHeaders),
        ComparisonURL = "not-applicable",
        QueryParams   = wrq:req_qs(Req1),
        SigV4Headers  = erlcloud_aws:sign_v4(?METHOD(Auth), ?PATH(Auth), ?CONFIG(Auth), SignedHeaders, <<>>, Region, "s3", QueryParams, Date),
        IncomingSig   = IncomingSignature,
        ComparisonSig = parseauth_or_throw(proplists:get_value("Authorization", SigV4Headers, ""), {RequestId, Req1, Context}),
?debugFmt("~nauth_type: auth_header", []),
?debugFmt("~nDate: ~p", [Date]),
?debugFmt("~nIncomingSignature: ~p", [IncomingSignature]),
?debugFmt("~nRegion: ~p", [Region]),
?debugFmt("~nSignedHeaders: ~p", [SignedHeaders]),
?debugFmt("~nAuth: ~p", [Auth]),
?debugFmt("~nComparisonURL: ~p", [ComparisonURL]),
?debugFmt("~nQueryParams: ~p", [QueryParams]),
?debugFmt("~nSigV4Headers: ~p", [SigV4Headers]),
?debugFmt("~nIncomingSig: ~p", [IncomingSig]),
?debugFmt("~nComparisonSig: ~p", [ComparisonSig]),
        % TODO: try to remove alt sig computation and see what happens
        % NOTE: this was tried, and caused compilation and test failures.

        CalculatedSig =
            case IncomingSig of
                ComparisonSig ->
                    %AltComparisonSig = "not computed",
                    IncomingSig;
                _ ->
                    AltSigV4Headers   = erlcloud_aws:sign_v4(?METHOD(Auth), ?PATH(Auth), ?CONFIG(Auth), ?ALT_SIGNED_HEADERS(Auth), <<>>, Region, "s3", QueryParams, Date),
                    _AltComparisonSig = parseauth_or_throw(proplists:get_value("Authorization", AltSigV4Headers, ""), {RequestId, Req1, Context})
            end,
?debugFmt("~nCalculatedSig: ~p", [CalculatedSig]),
    auth_finish(RequestId, Req1, Context, Auth, ComparisonURL, IncomingSig, CalculatedSig)
    catch
        throw:{RequestId, Req, Context} -> encode_access_denied_error_response(RequestId, Req, Context)
    end;
is_authorized(Req0, Context) -> ?debugFmt("~nbksw_sec:is_authorized NO MATCHING CLAUSE!", []), {true, Req0, Context}.

% TODO: relocate this
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

% TODO: relocate this
% required signed headers common to both authorization header verification
% and presigned url verification.
% https://docs.amazonaws.cn/en_us/AmazonS3/latest/API/sigv4-query-string-auth.html
-spec check_signed_headers_common(proplists:proplist(), proplists:proplist()) -> boolean().
check_signed_headers_common(SignedHeaders, Headers) ->
    % host header is required
    proplists:is_defined("host", SignedHeaders) andalso

    % any x-amz-* headers present in request are required
    [] == [Key || {Key, _} <- Headers, is_amz(Key), not proplists:is_defined(Key, SignedHeaders)].

% TODO: relocate this
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

% TODO: relocate this
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

% TODO: relocate this
% @doc split credentials string into component parts
% Cred = "<access-key-id>/<date>/<AWS-region>/<AWS-service>/aws4_request"
-spec parse_x_amz_credential(string()) -> {ok, [string()]} | {error, parse_x_amz_credential}.
parse_x_amz_credential(Cred) ->
    Parse = string:split(Cred, "/", all),
    case Parse of
        [_access_key_id, _date, _aws_region, "s3", "aws4_request"] -> {ok, Parse};
        _                                                          -> {error, parse_x_amz_credential}
    end.

% TODO: relocate this
% @doc split signed header string into component parts. return empty string on empty string.
% Headers = "<header1>;<header2>;...<headerN>"
-spec parse_x_amz_signed_headers(string()) -> [string()].
parse_x_amz_signed_headers(Headers) ->
   string:split(Headers, ";", all).

% TODO: relocate this
% @doc convert the keys of key-value pairs to lowercase strings
-spec process_headers(Headers::[tuple()]) -> [tuple()].
process_headers(Headers) ->
    [{string:casefold(
        case is_atom(Key) of
            true -> atom_to_list(Key);
            _    -> Key
        end), Val} || {Key, Val} <- Headers].

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

%%===================================================================
% local functions, helpers, etc.
%%===================================================================

-spec alt_host(string(), tuple()) -> string().
alt_host(Host, Config) ->
    mini_s3:get_host_toggleport(Host, Config).

% common setup, init
-spec auth_init(any(), tuple(), string()) -> map().
auth_init(Req0, Context, SignedHeaders) ->
    AccessKey = bksw_conf:access_key_id(Context),
    Config    = mini_s3:new(AccessKey, bksw_conf:secret_access_key(Context), host(Req0)),
    #{accesskey          => AccessKey,
      config             => Config,
      alt_signed_headers => [case {K, V} of {"host", _} -> {"host", alt_host(host(Req0), Config)}; _ -> {K, V} end || {K, V} <- SignedHeaders],
      method             => list_to_atom(string:to_lower(erlang:atom_to_list(wrq:method(Req0)))),
      path               => wrq:path(Req0)}.

% TODO: spec
auth_finish(RequestId, Req1, #context{
                                aws_access_key_id = AWSAccessKeyId,
                                date              = Date,
                                reqid             = ReqId,
                                x_amz_expires_int = XAmzExpiresInt,
                                x_amz_expires_str = XAmzExpiresString
                               } = Context, Auth, ComparisonURL, IncomingSig, CalculatedSig) ->
    case IncomingSig of
        CalculatedSig ->
            case is_expired(Date, XAmzExpiresInt) of
                true ->
                    ?LOG_DEBUG("req_id=~p expired signature (~p) for ~p", [ReqId, XAmzExpiresInt, ?PATH(Auth)]),
                    encode_access_denied_error_response(RequestId, Req1, Context);
                false ->
                    case erlang:iolist_to_binary(AWSAccessKeyId) == erlang:iolist_to_binary(?ACCESSKEY(Auth)) of
                        true ->
                            MaxAge = "max-age=" ++ XAmzExpiresString,
                            Req2 = wrq:set_resp_header("Cache-Control", MaxAge, Req1),
                            {true, Req2, Context};
                        false ->
                            ?LOG_DEBUG("req_id=~p signing error for ~p", [ReqId, ?PATH(Auth)]),
                            %encode_sign_error_response(AWSAccessKeyId, IncomingSignature, RequestId,
                            encode_sign_error_response(AWSAccessKeyId, IncomingSig, RequestId,
                                                       ComparisonURL, Req1, Context)
                    end
            end;
        _ ->
            encode_access_denied_error_response(RequestId, Req1, Context)
    end;
auth_finish(_, Req, Context, _, _, _, _) ->
?debugFmt("~nbksw_sec:auth_finish NO MATCHING CLAUSE!", []),
    {{halt, 500}, Req, Context}.

% split  "<bucketname>/<key>" (possibly leading and/or trailing /) into {"bucketname", "key"}
% Path = "<bucketname>/<key>"
-spec get_bucket_key(Path::string()) -> {string(), string()}.
get_bucket_key(Path) ->
    case string:lexemes(Path, "/") of
        [            ] -> {"",     ""};
        [Bucket      ] -> {Bucket, ""};
        [Bucket | Key] -> {Bucket, filename:join(Key)}
    end.

-spec host(tuple()) -> list().
host(Req0) ->
    wrq:get_req_header("Host", Req0).

-spec is_amz(string()) -> boolean().
is_amz([$x, $-, $a, $m, $z, $- | _]) ->
    true;
is_amz([$X, $-, $A, $m, $z, $- | _]) ->
    true;
is_amz(_) ->
    false.

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

-spec parseauth_or_throw(string(), tuple()) -> string() | no_return().
parseauth_or_throw(Auth, Throw) ->
    case parse_authorization(Auth) of
        {ok, [_, _, Sig]} -> Sig;
        _                 -> throw(Throw)
    end.
