%% -*- erlang-indent-level: 4;indent-tabs-mode: nil; fill-column: 92 -*-
%% ex: ts=4 sw=4 et
%% @author Eric B Merritt <ericbmerritt@gmail.com>
%% @copyright Copyright 2012 Opscode, Inc.
-module(bksw_sec).

-export([handle_request/1]).

%%===================================================================
%% API functions
%%===================================================================
handle_request(Req0) ->
    {RequestId, Req1} = bksw_req:with_amz_request_id(Req0),
    case cowboy_http_req:header('Authorization', Req1) of
        {undefined, Req1} ->
            do_signed_url_authorization(RequestId, Req1);
        {IncomingAuth, Req1}  ->
            do_standard_authorization(RequestId, IncomingAuth, Req1)
    end.

do_signed_url_authorization(RequestId, Req0) ->
    {AWSAccessKeyId, Req1} = cowboy_http_req:qs_val(<<"AWSAccessKeyId">>, Req0),
    {Expires, Req2} = cowboy_http_req:qs_val(<<"Expires">>, Req1),
    {IncomingSignature, Req3} =cowboy_http_req:qs_val(<<"Signature">>, Req2),
    {RawMethod, Req4} = cowboy_http_req:method(Req3),
    Method = string:to_lower(erlang:atom_to_list(RawMethod)),
    {Headers, Req5} = cowboy_http_req:headers(Req4),
    {Path, Req6} = cowboy_http_req:raw_path(Req5),
    {keys, {AccessKey, SecretKey}} = bksw_conf:keys(),
    {StringToSign, Signature} =
        mini_s3:make_signed_url_authorization(SecretKey,
                                              erlang:list_to_existing_atom(Method),
                                              Path,
                                              Expires,
                                              Headers),
    case ((AWSAccessKeyId == AccessKey) andalso
          Signature == IncomingSignature) of
        true ->
            Req6;
        false ->
            encode_error_response(AWSAccessKeyId, IncomingSignature, RequestId,
                                  StringToSign, Req6)
    end.

do_standard_authorization(RequestId, IncomingAuth, Req0) ->
    {Headers, Req1} = cowboy_http_req:headers(Req0),
    AmzHeaders = amz_headers(Headers),
    {RawMethod, Req2} = cowboy_http_req:method(Req1),
    Method = string:to_lower(erlang:atom_to_list(RawMethod)),
    {ContentMD5, Req3} = cowboy_http_req:header('Content-Md5', Req2, ""),
    {ContentType, Req4} = cowboy_http_req:header('Content-Type', Req3, ""),
    {Date, Req5} = cowboy_http_req:header('Date', Req4, ""),
    {Host, Req6} = get_bucket(cowboy_http_req:host(Req5)),
    {Resource, Req7} = cowboy_http_req:raw_path(Req6),
    {Qs, Req8} = cowboy_http_req:raw_qs(Req7),
    {keys, {AccessKey, SecretKey}} = bksw_conf:keys(),
    {StringToSign, RawCheckedAuth} =
        mini_s3:make_authorization(AccessKey, SecretKey,
                                   erlang:list_to_existing_atom(Method),
                                   bksw_util:to_string(ContentMD5),
                                   bksw_util:to_string(ContentType),
                                   bksw_util:to_string(Date),
                                   AmzHeaders, bksw_util:to_string(Host),
                                   bksw_util:to_string(Resource),
                                   bksw_util:to_string(Qs)),
    CheckedAuth = erlang:iolist_to_binary(RawCheckedAuth),
    [AccessKeyId, Signature] = split_authorization(IncomingAuth),
    case CheckedAuth == IncomingAuth of
        true ->
            Req8;
        false ->
            encode_error_response(AccessKeyId, Signature, RequestId, StringToSign, Req8)
    end.

get_bucket({[_, _], Req6}) ->
    {"", Req6};
get_bucket({[Bucket, _, _], Req6}) ->
    {Bucket, Req6}.


encode_error_response(AccessKeyId, Signature,
                      RequestId, StringToSign, Req0) ->
    Req1 = bksw_req:with_amz_id_2(Req0),
    Body = bksw_xml:signature_does_not_match_error(
             RequestId, bksw_util:to_string(Signature),
             bksw_util:to_string(StringToSign),
             bksw_util:to_string(AccessKeyId)),
    {ok, Req2} = cowboy_http_req:reply(403, [], Body, Req1),
    Req2.

split_authorization(<<"AWS :", Rest/binary>>) ->
    [<<>>, Rest];
split_authorization(<<"AWS ", Rest/binary>>) ->
    binary:split(Rest, <<":">>).

is_amz(<<"X-Amz-Acl", _Rest/binary>>) ->
    true;
is_amz(<<"x-amz-acl", _Rest/binary>>) ->
    true;
is_amz(_) ->
    false.

to_processable(K) ->
    string:to_lower(erlang:binary_to_list(K)).

amz_headers(Headers) ->
    [{to_processable(K), V} || {K,V} <- Headers, is_amz(K)].
