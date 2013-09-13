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

-module(bksw_sec).

-export([is_authorized/2]).

-define(SECONDS_AT_EPOCH, 62167219200).

%%===================================================================
%% API functions
%%===================================================================
is_authorized(Req0, Context) ->
    Headers = mochiweb_headers:to_list(wrq:req_headers(Req0)),
    {RequestId, Req1} = bksw_req:with_amz_request_id(Req0),
    case proplists:get_value('Authorization', Headers, undefined) of
        undefined ->
            do_signed_url_authorization(RequestId, Req1, Context);
        IncomingAuth ->
            do_standard_authorization(RequestId, IncomingAuth, Req1, Context)
    end.

do_signed_url_authorization(RequestId, Req0, Context) ->
    AWSAccessKeyId = wrq:get_qs_value("AWSAccessKeyId", Req0),
    Expires = wrq:get_qs_value("Expires", Req0),
    IncomingSignature = wrq:get_qs_value("Signature", Req0),
    RawMethod = wrq:method(Req0),
    Method = string:to_lower(erlang:atom_to_list(RawMethod)),
    Headers = mochiweb_headers:to_list(wrq:req_headers(Req0)),
    Path  = wrq:path(Req0),
    AccessKey = bksw_conf:access_key_id(Context),
    SecretKey = bksw_conf:secret_access_key(Context),
    case make_signed_url_authorization(SecretKey,
                                       Method,
                                       Path,
                                       Expires,
                                       Headers) of
        {StringToSign, Signature} ->
            case is_expired(Expires) of
                true ->
                    encode_access_denied_error_response(RequestId, Req0, Context);
                false ->
                    case ((erlang:iolist_to_binary(AWSAccessKeyId) ==
                               erlang:iolist_to_binary(AccessKey)) andalso
                          erlang:iolist_to_binary(Signature) ==
                              erlang:iolist_to_binary(IncomingSignature)) of
                        true ->
                            {true, Req0, Context};
                        false ->
                            encode_sign_error_response(AWSAccessKeyId, IncomingSignature, RequestId,
                                                       StringToSign, Req0, Context)
                    end
                end;
        error ->
            encode_access_denied_error_response(RequestId, Req0, Context)
    end.

make_signed_url_authorization(SecretKey, Method, Path, Expires, Headers) ->
    try
        mini_s3:make_signed_url_authorization(SecretKey,
                                              erlang:list_to_existing_atom(Method),
                                              Path,
                                              Expires,
                                              Headers)
    catch
        _:Why ->
            error_logger:error_report({error, {{mini_s3, make_signed_url_authorization},
                                               [<<"SECRETKEY">>, Method, Path, Expires, Headers],
                                               Why}}),
            error
    end.

do_standard_authorization(RequestId, IncomingAuth, Req0, Context) ->
    Headers = mochiweb_headers:to_list(wrq:req_headers(Req0)),
    AmzHeaders = amz_headers(Headers),
    RawMethod = wrq:method(Req0),
    Method = string:to_lower(erlang:atom_to_list(RawMethod)),
    ContentMD5 = proplists:get_value('Content-Md5', Headers, ""),
    ContentType = proplists:get_value('Content-Type', Headers, ""),
    Date = proplists:get_value('Date', Headers, ""),
    %% get_object_and_bucket decodes the bucket, but the request will have been signed with
    %% the encoded bucket.
    {ok, Bucket0, Resource} = bksw_util:get_object_and_bucket(Req0),
    Bucket = bksw_io_names:encode(Bucket0),
    AccessKey = bksw_conf:access_key_id(Context),
    SecretKey = bksw_conf:secret_access_key(Context),

    {StringToSign, RawCheckedAuth} =
        mini_s3:make_authorization(AccessKey, SecretKey,
                                   erlang:list_to_existing_atom(Method),
                                   bksw_util:to_string(ContentMD5),
                                   bksw_util:to_string(ContentType),
                                   bksw_util:to_string(Date),
                                   AmzHeaders, bksw_util:to_string(Bucket),
                                   "/" ++ bksw_util:to_string(Resource),
                                   ""),
    CheckedAuth = erlang:iolist_to_binary(RawCheckedAuth),
    [AccessKeyId, Signature] = split_authorization(IncomingAuth),
    case erlang:iolist_to_binary(IncomingAuth) of
        CheckedAuth ->
            {true, Req0, Context};
        _ ->
            encode_sign_error_response(AccessKeyId, Signature,
                                       RequestId, StringToSign, Req0, Context)
    end.

-spec is_expired(binary()) -> boolean().
is_expired(Expires) ->
    Now = calendar:datetime_to_gregorian_seconds(erlang:universaltime()),
    bksw_util:to_integer(Expires) < (Now - ?SECONDS_AT_EPOCH).

%% get_bucket([Bucket, _, _]) ->
%%     Bucket.

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

split_authorization([$A, $W, $S, $\s, $: | Rest]) ->
    [<<>>, Rest];
split_authorization([$A, $W, $S, $\s  | Rest]) ->
    string:tokens(Rest, ":").

is_amz([$X, $-, $A, $m, $z, $-, $A, $c, $l | _]) ->
    true;
is_amz([$x, $-, $a, $m, $z, $-, $a, $c, $l | _]) ->
    true;
is_amz(_) ->
    false.

amz_headers(Headers) ->
    [{process_header(K), V} || {K,V} <- Headers, is_amz(K)].

process_header(Key) ->
    string:to_lower(bksw_util:to_string(Key)).
