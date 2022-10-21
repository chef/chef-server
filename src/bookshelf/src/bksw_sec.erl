%% -*- erlang-indent-level: 4;indent-tabs-mode: nil; fill-column: 92 -*-
%% ex: ts=4 sw=4 et
%% @author Eric B Merritt <ericbmerritt@gmail.com>
%% @author Lincoln Baker  <lbaker@chef.io>
%% Copyright Chef Software, Inc. All Rights Reserved.
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

-export([encode_access_denied_error_response/3] ).
-export([is_authorized/2                      ] ).
-export([parse_authorization/1                ] ).
-include("internal.hrl"                         ).
-include_lib("erlcloud/include/erlcloud_aws.hrl").

-ifdef(TEST).
-compile([export_all, nowarn_export_all       ] ).
-endif.

%%===================================================================
%% API functions
%%===================================================================

is_authorized(Req0, #context{auth_check_disabled = true} = Context) -> {true, Req0, Context};
is_authorized(Req0, #context{auth_type           = presigned_url,
                             aws_access_key_id   = AWSAccessKeyId,
                             date                = Date,
                             incoming_sig        = IncomingSignature,
                             reqid               = ReqId,
                             signed_headers      = SignedHeaders,
                             x_amz_expires_str   = XAmzExpiresString,
                             x_amz_expires_int   = XAmzExpiresInt} = Context) ->
    Auth               = auth_init(Req0, Context, SignedHeaders),
    {Bucketname, Key } = get_bucket_key(path(Auth)),
    ComparisonURL      = mini_s3:s3_url(method(Auth), Bucketname, Key, XAmzExpiresInt, SignedHeaders, Date, config(Auth)),
    [_, ComparisonSig] = string:split(ComparisonURL, "&X-Amz-Signature=", all),

    % If the signature comparison fails, we attempt an alternate signature
    % calculation and comparison which adds or removes the port from the host
    % header depending on whether the port is present or absent. At the time
    % this was put in, there were problems with signature failures due to
    % inconsistent treatment of host headers by various chef clients
    % (present or missing ports).  Determining whether alt sig comparison is
    % still necessary would require investigation and testing.

    CalculatedSig =
        case const_time_compare(IncomingSignature, ComparisonSig, true) of
            true ->
                IncomingSignature;
            _ ->
                AltComparisonURL      = mini_s3:s3_url(method(Auth), Bucketname, Key, XAmzExpiresInt, alt_signed_headers(Auth), Date, config(Auth)),
                [_, AltComparisonSig] = string:split(AltComparisonURL, "&X-Amz-Signature=", all),
                AltComparisonSig
        end,

    case const_time_compare(IncomingSignature, CalculatedSig, true) of
        true ->
            case is_expired(Date, XAmzExpiresInt) of
                true ->
                    ?LOG_DEBUG("req_id=~p expired signature (~p) for ~p", [ReqId, XAmzExpiresInt, path(Auth)]),
                    encode_access_denied_error_response(reqid(Auth), req(Auth), Context);
                false ->
                    case erlang:iolist_to_binary(AWSAccessKeyId) == erlang:iolist_to_binary(accesskey(Auth)) of
                        true ->
                            MaxAge = "max-age=" ++ XAmzExpiresString,
                            Req2   = wrq:set_resp_header("Cache-Control", MaxAge, req(Auth)),
                            {true, Req2, Context};
                        false ->
                            ?LOG_DEBUG("req_id=~p signing error for ~p", [ReqId, path(Auth)]),
                            encode_sign_error_response(AWSAccessKeyId, IncomingSignature, reqid(Auth),
                                                       CalculatedSig, req(Auth), Context)
                    end
            end;
        _ ->
            encode_access_denied_error_response(reqid(Auth), req(Auth), Context)
    end;
is_authorized(Req0, #context{auth_type           = auth_header,
                             aws_access_key_id   = AWSAccessKeyId,
                             date                = Date,
                             incoming_sig        = IncomingSignature,
                             region              = Region,
                             signed_headers      = SignedHeaders} = Context) ->
    Auth          = auth_init(Req0, Context, SignedHeaders),
    QueryParams   = wrq:req_qs(req(Auth)),
    SigV4Headers  = erlcloud_aws:sign_v4(method(Auth), path(Auth), config(Auth), SignedHeaders, <<>>, Region, "s3", QueryParams, Date),
    ComparisonSig = parseauth(proplists:get_value("Authorization", SigV4Headers, "")),

    % If the signature comparison fails, we attempt an alternate signature
    % calculation and comparison which adds or removes the port from the host
    % header depending on whether the port is present or absent. At the time
    % this was put in, there were problems with signature failures due to
    % inconsistent treatment of host headers by various chef clients
    % (present or missing ports).  Determining whether alt sig comparison is
    % still necessary would require investigation and testing.

    CalculatedSig =
        case const_time_compare(IncomingSignature, ComparisonSig, true) of
            true ->
                IncomingSignature;
            _ ->
                AltSigV4Headers   = erlcloud_aws:sign_v4(method(Auth), path(Auth), config(Auth), alt_signed_headers(Auth), <<>>, Region, "s3", QueryParams, Date),
                parseauth(proplists:get_value("Authorization", AltSigV4Headers, ""))
            end,

    case const_time_compare(IncomingSignature, CalculatedSig, true) of
        true ->
            {true, req(Auth), Context};
        _ ->
            encode_sign_error_response(AWSAccessKeyId, IncomingSignature, reqid(Auth),
                                       CalculatedSig, req(Auth), Context)
    end.

%%===================================================================
% local functions, helpers, etc.
%%===================================================================

% common setup and init
-spec auth_init(any(), tuple(), string()) -> map().
auth_init(Req0, Context, SignedHeaders) ->
    AccessKey            =  bksw_conf:access_key_id(Context),
    {RequestId, Req1}    =  bksw_req:with_amz_request_id(Req0),
    Config               =  mini_s3:new(AccessKey, bksw_conf:secret_access_key(Context), host(Req1), path),
    #{accesskey          => AccessKey,
      config             => Config,
      alt_signed_headers => [case {K, V} of {"host", _} -> {"host", get_host_toggleport(host(Req1), Config)}; _ -> {K, V} end || {K, V} <- SignedHeaders],
      method             => list_to_atom(string:to_lower(erlang:atom_to_list(wrq:method(Req1)))),
      path               => wrq:path(Req1),
      req                => Req1,
      reqid              => RequestId}.

% due to a security vulnerability described by Mark Anderson, we should compare signatures
% in constant time, and not 'early out' on the first mismatched character.  this means we
% are purposefully using a 'deoptimized' string compare function.
-spec const_time_compare(string() | binary() | atom(), string() | binary() | atom(), boolean()) -> boolean().
const_time_compare(S1, S2, IsEqual) when is_binary(S1) ->
    const_time_compare(binary_to_list(S1), S2, IsEqual);
const_time_compare(S1, S2, IsEqual) when is_binary(S2) ->
    const_time_compare(S1, binary_to_list(S2), IsEqual);
const_time_compare(S1, S2, IsEqual) when is_list(S1), is_list(S2) ->
    ctcomp(S1, S2, IsEqual);
const_time_compare(_, _, _) ->
    false.

-spec ctcomp(string(), string(), boolean()) -> boolean().
ctcomp([    ], [    ], IsEqual) -> IsEqual;
ctcomp([    ], [_|S2],       _) -> ctcomp([], S2, false  );
ctcomp([_|S1], [    ],       _) -> ctcomp(S1, [], false  );
ctcomp([X|S1], [X|S2], IsEqual) -> ctcomp(S1, S2, IsEqual);
ctcomp([_|S1], [_|S2],       _) -> ctcomp(S1, S2, false  ).

encode_access_denied_error_response(RequestId, Req0, Context) ->
    Req1 = bksw_req:with_amz_id_2(Req0),
    Body = bksw_xml:access_denied_error(RequestId),
    Req2 = wrq:set_resp_body(Body, Req1),
    {{halt, 403}, Req2, Context}.

encode_sign_error_response(AccessKeyId, IncomingSignature,
                           RequestId, RecreatedSignature, Req0,
                           Context) ->
    Req1 = bksw_req:with_amz_id_2(Req0),
    Body = bksw_xml:signature_does_not_match_error(
             RequestId, bksw_util:to_string(IncomingSignature),
             bksw_util:to_string(RecreatedSignature),
             bksw_util:to_string(AccessKeyId)),
    Req2 = wrq:set_resp_body(Body, Req1),
    {{halt, 403}, Req2, Context}.

% split  "<bucketname>/<key>" (possibly leading and/or trailing /) into {"bucketname", "key"}
% Path = "<bucketname>/<key>"
-spec get_bucket_key(Path::string()) -> {string(), string()}.
get_bucket_key(Path) ->
    case string:lexemes(Path, "/") of
        [            ] -> {"",     ""};
        [Bucket      ] -> {Bucket, ""};
        [Bucket | Key] -> {Bucket, filename:join(Key)}
    end.


% get host and toggle the port (add port or remove it)
-spec get_host_toggleport(string(), aws_config()) -> string().
get_host_toggleport(Host, #aws_config{s3_port = S3Port}) ->
    Port = integer_to_list(S3Port),
    HostPort = string:join([Host, Port], ":"),
    case string:split(Host, ":", trailing) of
        [Host      ] ->
            HostPort;
        ["http",  _] ->
            HostPort;
        ["https", _] ->
            HostPort;
        [H,       _] ->
            H
    end.

-spec host(tuple()) -> list().
host(Req0) ->
    wrq:get_req_header("Host", Req0).

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
%
% https://docs.aws.amazon.com/general/latest/gr/sigv4-date-handling.html
% The time stamp must be in UTC and in the following ISO 8601 format: YYYYMMDD'T'HHMMSS'Z'.
% For example, 20150830T123600Z is a valid time stamp. Do not include milliseconds in the time stamp.
%
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

    % this could be used to check if the date constructed is valid:
    % calendar:valid_date({{Year, Month, Day}, {Hour, Min, Sec}}),

    DateSeconds      = calendar:datetime_to_gregorian_seconds({{Year, Month, Day}, {Hour, Min, Sec}}),
    UniversalTimeSec = calendar:datetime_to_gregorian_seconds(calendar:now_to_universal_time(os:timestamp())),
    DateSeconds + ExpiresSec < UniversalTimeSec.

-spec parseauth(string()) -> string() | err.
parseauth(Auth) ->
    case parse_authorization(Auth) of
        {ok, [_, _, Sig]} -> Sig;
        _                 -> err
    end.

% @doc split authorization header into component parts
% https://docs.aws.amazon.com/AmazonS3/latest/API/sigv4-auth-using-authorization-header.html
-spec parse_authorization(string()) -> {ok, [string()]} | {error, parse_authorization}.
parse_authorization(Authorization) ->
    case string:split(Authorization, " ", all) of
        ["AWS4-HMAC-SHA256", "Credential="++Cred, "SignedHeaders="++SigHead, "Signature="++Signature] ->
            {ok, [string:trim(Cred, trailing, ","), string:trim(SigHead, trailing, ","), Signature]};
        _ ->
            {error, parse_authorization}
    end.

% accessors
accesskey(Auth)          -> maps:get(accesskey,          Auth).
alt_signed_headers(Auth) -> maps:get(alt_signed_headers, Auth).
config(Auth)             -> maps:get(config,             Auth).
method(Auth)             -> maps:get(method,             Auth).
path(Auth)               -> maps:get(path,               Auth).
req(Auth)                -> maps:get(req,                Auth).
reqid(Auth)              -> maps:get(reqid,              Auth).
