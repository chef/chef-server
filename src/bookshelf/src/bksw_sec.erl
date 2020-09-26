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
-export([is_authorized/2                      ]).
-export([parse_authorization/1                ]).

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

%%===================================================================
%% API functions
%%===================================================================

is_authorized(Req0, #context{auth_check_disabled=true} = Context) -> {true, Req0, Context};
is_authorized(Req0, #context{auth_type                 = presigned_url,
                             date                      = Date,
                             incoming_sig              = IncomingSignature,
                             signed_headers            = SignedHeaders,
                             x_amz_expires_int         = XAmzExpiresInt} = Context) ->
    {RequestId, Req1} = bksw_req:with_amz_request_id(Req0),
    try
        Auth               = auth_init(Req1, Context, SignedHeaders),
        {Bucketname, Key}  = get_bucket_key(?PATH(Auth)),
        ComparisonURL      = mini_s3:s3_url(?METHOD(Auth), Bucketname, Key, XAmzExpiresInt, SignedHeaders, Date, ?CONFIG(Auth)),
        IncomingSig        = list_to_binary(IncomingSignature),
        [_, ComparisonSig] = string:split(ComparisonURL, "&X-Amz-Signature=", trailing),

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
    auth_finish(RequestId, Req1, Context, Auth, ComparisonURL, IncomingSig, CalculatedSig)
    catch
        throw:{RequestId, Req, Context} -> encode_access_denied_error_response(RequestId, Req, Context)
    end;
is_authorized(Req0, #context{auth_type                 = auth_header,
                             date                      = Date,
                             incoming_sig              = IncomingSignature,
                             region                    = Region,
                             signed_headers            = SignedHeaders} = Context) ->
    {RequestId, Req1} = bksw_req:with_amz_request_id(Req0),
    try
        Auth          = auth_init(Req1, Context, SignedHeaders),
        ComparisonURL = "not-applicable",
        QueryParams   = wrq:req_qs(Req1),
        SigV4Headers  = erlcloud_aws:sign_v4(?METHOD(Auth), ?PATH(Auth), ?CONFIG(Auth), SignedHeaders, <<>>, Region, "s3", QueryParams, Date),
        IncomingSig   = IncomingSignature,
        ComparisonSig = parseauth_or_throw(proplists:get_value("Authorization", SigV4Headers, ""), {RequestId, Req1, Context}),

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
    auth_finish(RequestId, Req1, Context, Auth, ComparisonURL, IncomingSig, CalculatedSig)
    catch
        throw:{RequestId, Req, Context} -> encode_access_denied_error_response(RequestId, Req, Context)
    end.

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

% common setup, init
-spec auth_init(any(), tuple(), string()) -> map().
auth_init(Req0, Context, SignedHeaders) ->
    AccessKey = bksw_conf:access_key_id(Context),
    Config    = mini_s3:new(AccessKey, bksw_conf:secret_access_key(Context), host(Req0)),
    #{accesskey          => AccessKey,
      config             => Config,
      alt_signed_headers => [case {K, V} of {"host", _} -> {"host", get_host_toggleport(host(Req0), Config)}; _ -> {K, V} end || {K, V} <- SignedHeaders],
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
    end.

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
get_host_toggleport(Host, Config) ->
    case string:split(Host, ":", trailing) of
        [Host      ] ->
            Port = integer_to_list(Config#aws_config.s3_port),
            string:join([Host, Port], ":");
        ["http",  _] ->
            Port = integer_to_list(Config#aws_config.s3_port),
            string:join([Host, Port], ":");
        ["https", _] ->
            Port = integer_to_list(Config#aws_config.s3_port),
            string:join([Host, Port], ":");
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
