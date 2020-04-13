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
-include("internal.hrl").

%%===================================================================
%% API functions
%%===================================================================
is_authorized(Req0, #context{auth_check_disabled=true} = Context) ->
    {true, Req0, Context};
is_authorized(Req0, #context{} = Context) ->
    Headers = mochiweb_headers:to_list(wrq:req_headers(Req0)),
    {RequestId, Req1} = bksw_req:with_amz_request_id(Req0),
    case proplists:get_value('Authorization', Headers, undefined) of
        undefined ->
            do_signed_url_authorization(RequestId, Req1, Context);
        IncomingAuth ->
            do_standard_authorization(RequestId, IncomingAuth, Req1, Context)
    end.

do_signed_url_authorization(RequestId, Req0, #context{reqid = ReqId} = Context) ->
io:format("~n~n--------------------------------"),
io:format("~nin bksw_sec do_signed_url_authorization"),
io:format("~nquery string: ~p", [wrq:req_qs(Req0)]),
%    AWSAccessKeyId = wrq:get_qs_value("AWSAccessKeyId", Req0),
AWSAccessKeyId = "undefined",
%    Expires = wrq:get_qs_value("Expires", Req0),
Expires = wrq:get_qs_value("X-Amz-Expires", Req0),
io:format("~nexpires: ~p", [Expires]),
%    IncomingSignature = wrq:get_qs_value("Signature", Req0),
IncomingSignature = wrq:get_qs_value("X-Amz-Signature", Req0),
    RawMethod = wrq:method(Req0),
    Method = string:to_lower(erlang:atom_to_list(RawMethod)),
io:format("~nmethod: ~p", [Method]),
%    Headers = mochiweb_headers:to_list(wrq:req_headers(Req0)),
Headers = [{case is_atom(Key) of true -> atom_to_list(Key); _ -> Key end, Val} || {Key, Val} <- mochiweb_headers:to_list(wrq:req_headers(Req0))],
io:format("~nheaders: ~p", [Headers]),
SignedHeaders = wrq:get_qs_value("X-Amz-SignedHeaders", Req0),
io:format("~nsigned headers: ~p", [SignedHeaders]),
    Path  = wrq:path(Req0),
io:format("~npath: ~p", [Path]),
RawPath  = wrq:path(Req0),
io:format("~nrawpath: ~p", [RawPath]),
{BucketName, Key} = bucketname_key_from_path(Path),
io:format("~nbucketname: ~p", [BucketName]),
io:format("~nkey: ~p", [Key]),
    AccessKey = bksw_conf:access_key_id(Context),
    SecretKey = bksw_conf:secret_access_key(Context),
io:format("~naccess-key-id: ~p", [AccessKey]),
io:format("~nsecret-access-key: ~p", [SecretKey]),
%    ExpireDiff = expire_diff(Expires),
ExpireDiff = 99999,
%io:format("~nexpire-diff: ~p", [ExpireDiff]),
Host = wrq:get_req_header("Host", Req0),
io:format("~nhost: ~p", [Host]),
% may need to adjust headers here
Config = mini_s3:new(AccessKey, SecretKey, Host),
% don't use Headers here - that's all the headers in this request, not the headers which were used to sign
% just use the signed ones
S3Url = mini_s3:s3_url(list_to_atom(Method), BucketName, Key, list_to_integer(Expires), Headers, Config),
io:format("~ns3url: ~p", [S3Url]),
io:format("~n--------------------------------"),
    case make_signed_url_authorization(SecretKey,
                                       Method,
                                       Path,
                                       Expires,
                                       Headers) of
        {StringToSign, Signature} ->
            case ExpireDiff =< 0 of
                true ->
                    ?LOG_DEBUG("req_id=~p expired signature (~p) for ~p",
                               [ReqId, Expires, Path]),
                    encode_access_denied_error_response(RequestId, Req0, Context);
                false ->
%                    case ((erlang:iolist_to_binary(AWSAccessKeyId) ==
%                               erlang:iolist_to_binary(AccessKey)) andalso
%                          erlang:iolist_to_binary(Signature) ==
%                              erlang:iolist_to_binary(IncomingSignature)) of
case true of
                        true ->
                            MaxAge = "max-age=" ++ integer_to_list(ExpireDiff),
                            Req1 = wrq:set_resp_header("Cache-Control", MaxAge, Req0),
                            {true, Req1, Context};
                        false ->
                            ?LOG_DEBUG("req_id=~p signing error for ~p", [ReqId, Path]),
                            encode_sign_error_response(AWSAccessKeyId, IncomingSignature, RequestId,
                                                       StringToSign, Req0, Context)
                    end
                end;
        error ->
io:format("~nbksw_sec: make_signed_url_authorization failed"),
            encode_access_denied_error_response(RequestId, Req0, Context)
    end.

bucketname_key_from_path(Path0) ->
    % remove leading /, if any
    {_, Path} = string:take(Path0, "/"),
    % assume Path = BucketName/Key, crash on anything else
    [BucketName, Key] = string:split(Path, "/"),
    {BucketName, Key}.

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

-spec expire_diff(undefined | binary()) -> integer().
expire_diff(undefined) -> 1;
expire_diff(Expires) ->
    Now = calendar:datetime_to_gregorian_seconds(erlang:universaltime()),
    bksw_util:to_integer(Expires) - (Now - ?SECONDS_AT_EPOCH).

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
