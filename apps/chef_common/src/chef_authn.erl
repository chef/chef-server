%
% License:: Apache License, Version 2.0
%
% Licensed under the Apache License, Version 2.0 (the "License");
% you may not use this file except in compliance with the License.
% You may obtain a copy of the License at
% 
%     http://www.apache.org/licenses/LICENSE-2.0
% 
% Unless required by applicable law or agreed to in writing, software
% distributed under the License is distributed on an "AS IS" BASIS,
% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
% See the License for the specific language governing permissions and
% limitations under the License.
%
% @author Seth Falcon <seth@opscode.com>
% @copyright Copyright 2011 Opscode, Inc.
% @version 0.0.2
% @end

% @doc chef_authn - Request signing and authentication for Opscode Chef
%
% This module is an Erlang port of the mixlib-authentication Ruby gem.
% It can be used to sign HTTP requests to send to a Chef server or to
% validate such requests (for server implementation).
-module(chef_authn).

-define(buf_size, 16384).
-define(signing_version, <<"1.0">>).
-define(signing_version_key, <<"version">>).

-define(required_headers, [<<"X-Ops-UserId">>,
                           <<"X-Ops-Timestamp">>,
                           <<"X-Ops-Sign">>,
                           % FIXME: mixlib-authorization requires host, but
                           % it is not used as part of the signing protocol AFAICT
                           % <<"host">>,
                           <<"X-Ops-Content-Hash">>]).

-export([
         extract_private_key/1,
         hash_string/1,
         hash_file/1,
         sign_request/5,
         sign_request/6,
         authenticate_user_request/6,
         validate_headers/2
         ]).

-include_lib("public_key/include/public_key.hrl").

-type calendar_time() :: { non_neg_integer(),  non_neg_integer(),  non_neg_integer() }.
-type calendar_date() :: { integer(),  1..12, 1..31 }.

-type get_header_fun() :: fun((header_name()) -> header_value()).
-type http_body() :: binary() | pid().
-type user_id() :: binary().
-type http_method() :: binary().
-type http_time() :: binary().
-type iso8601_time() :: binary().
-type http_path() :: binary().
-type sha_hash64() :: binary().
-type erlang_time() :: {calendar_date(), calendar_time()}.
-type raw_public_key() :: binary().
-type header_name() :: binary().
-type header_value() :: binary() | 'undefined'.
-type header_fun() :: fun((header_name()) -> header_value()).
-type time_skew() :: non_neg_integer().


-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-spec extract_private_key(binary()) -> term() | {error, bad_key}.
extract_private_key(RawKey) ->
    case catch public_key:pem_decode(RawKey) of
        [{Type, Der, _}] ->
            public_key:der_decode(Type, Der);
        [] ->
            {error, bad_key}
    end.

-spec(hash_string(string()|binary()) -> sha_hash64()).
%% @doc Base 64 encoded SHA1 of `Str'
hash_string(Str) ->
    base64:encode(crypto:sha(Str)).

-spec(hash_file(file:io_device()) -> sha_hash64()).
%% @doc Base 64 encoded SHA1 of contents of `F'
hash_file(F) ->
    hash_file(F, crypto:sha_init()).

-spec hash_file(file:io_device(),binary()) -> sha_hash64().
hash_file(F, Ctx) ->
    case io:get_chars(F, "", ?buf_size) of
        eof ->
            base64:encode(crypto:sha_final(Ctx));
        Data ->
            hash_file(F, crypto:sha_update(Ctx, Data))
    end.



%% @doc Converts Erlang time-tuple to iso8601 formatted date string.
%%
%% Example output looks like <<"2003-12-13T18:30:02Z">>
-spec(time_iso8601(erlang_time() | 'now') -> binary()).
time_iso8601(now) ->
    time_iso8601(calendar:universal_time());
time_iso8601({{Year, Month, Day}, {Hour, Min, Sec}}) ->
    % Is there a way to build a binary straight away?
    Fmt = "~4B-~2..0B-~2..0BT~2..0B:~2..0B:~2..0BZ",
    list_to_binary(lists:flatten(io_lib:format(Fmt,
                                               [Year, Month, Day,
                                                Hour, Min, Sec]))).

%% @doc Convert an iso8601 time string to Erlang date time
%% representation.
-spec(time_iso8601_to_date_time(string()|binary()) -> erlang_time()).
time_iso8601_to_date_time(ATime) when is_binary(ATime) ->
    time_iso8601_to_date_time(binary_to_list(ATime));
time_iso8601_to_date_time(ATime) ->
    [Year, Month, Day, Hour, Min, Sec] =
        [ list_to_integer(S) || S <- string:tokens(ATime, "-T:Z") ],
    {{Year, Month, Day}, {Hour, Min, Sec}}.

-spec(canonical_time(string() | binary()) -> iso8601_time()).
%% @doc Convert a string or binary HTTP request time to iso8601 format
canonical_time(T) when is_binary(T) ->
    canonical_time(binary_to_list(T));
canonical_time(T) when is_list(T) ->
    time_iso8601(httpd_util:convert_request_date(T)).

%% @doc Canonicalize an HTTP request path by removing doubled slashes
%% and trailing slash (except for case of root path).
-spec  canonical_path(binary()) -> binary().
canonical_path(Path = <<"/">>) ->
    Path;
canonical_path(Path) ->
    NoDoubles = re:replace(Path, "/+/", <<"/">>, [{return, binary}, global]),
    re:replace(NoDoubles, "/$", % fix emacs erlang-mode: "
               "", [{return, binary}]).

%% @doc Canonicalize HTTP method as all uppercase binary

canonical_method(Method) ->
    list_to_binary(string:to_upper(binary_to_list(Method))).

-spec(hashed_body(binary() | pid()) -> binary()).
%% @doc Return the SHA1 hash of the body which can either be a binary
%% or the pid of a file.
hashed_body(Body) when is_pid(Body) ->
    hash_file(Body);
hashed_body(Body) when is_binary(Body) ->
    hash_string(Body);
hashed_body(Body) when is_list(Body) ->
    hashed_body(iolist_to_binary(Body)).

-spec(canonicalize_request(sha_hash64(), user_id(), http_method(), iso8601_time(), http_path())
      -> binary()).
%% @doc Canonicalize an HTTP request into a binary that can be signed
%% for verification.
%%
%% NOTE: this function assumes that `Time' is already in canonical
%% form (see canonical_time/1).  Other arguments are canonicalized.
%%
canonicalize_request(BodyHash, UserId, _Method, Time, _Path)
  when BodyHash =:= undefined orelse
         UserId =:= undefined orelse
           Time =:= undefined ->
    undefined;
canonicalize_request(BodyHash, UserId, Method, Time, Path) ->
    Format = <<"Method:~s\nHashed Path:~s\nX-Ops-Content-Hash:~s\nX-Ops-Timestamp:~s\nX-Ops-UserId:~ts">>,
    iolist_to_binary(io_lib:format(Format, [canonical_method(Method),
                                            hash_string(canonical_path(Path)),
                                            BodyHash,
                                            Time,
                                            UserId])).

-spec sign_request(rsa_private_key(), user_id(), http_method(),
                   http_time(), http_path()) ->
      [{binary(), binary()}, ...].
%% @doc Sign an HTTP request without a body (primarily GET)
sign_request(PrivateKey, User, Method, Time, Path) ->
    sign_request(PrivateKey, <<"">>, User, Method, Time, Path).

%% @doc Sign an HTTP request so it can be sent to a Chef server.
%%
%% Returns a list of header tuples that should be included in the
%% final HTTP request.
%%
-spec sign_request(rsa_private_key(), http_body(), user_id(), http_method(),
                   http_time(), http_path()) ->
    [{binary(), binary()}, ...].
sign_request(PrivateKey, Body, User, Method, Time, Path) ->
    CTime = canonical_time(Time),
    HashedBody = hashed_body(Body),
    SignThis = canonicalize_request(HashedBody, User, Method, CTime, Path),
    Sig = base64:encode(public_key:encrypt_private(SignThis, PrivateKey)),
    [{<<"X-Ops-Content-Hash">>, HashedBody},
     {<<"X-Ops-UserId">>, User},
     {<<"X-Ops-Sign">>, <<"version=1.0">>},
     {<<"X-Ops-Timestamp">>, CTime}]
       ++ sig_header_items(Sig).

%% @doc Generate X-Ops-Authorization-I for use in building auth headers
-spec xops_header(non_neg_integer()) -> header_name().
xops_header(I) ->
    iolist_to_binary(io_lib:format(<<"X-Ops-Authorization-~B">>, [I])).

%% @doc Given an encrypted signature base64 binary, split it up with
%% line feeds evry 60 characters and build up a list of
%% X-Ops-Authorization-i header tuples.
%%
-spec sig_header_items(binary()) -> [{binary(),binary()}].
sig_header_items(Sig) ->
    % Ruby's Base64.encode64 method inserts line feeds every 60
    % encoded characters.
    Lines = sig_to_list(Sig, 60),
    [ {xops_header(I), L} ||
        {L, I} <- lists:zip(Lines, lists:seq(1, length(Lines))) ].

%% @doc Split a binary into chunks of size N
%-spec sig_to_list(binary(), pos_integer()) -> [binary()]. % TODO PROBLEMATIC
sig_to_list(Sig, N) ->
    lists:reverse(sig_to_list(Sig, N, [])).

-spec sig_to_list(binary(), pos_integer(), any()) -> [binary()].
sig_to_list(Sig, N, Acc) ->
    case iolist_size(Sig) =< N of
        true ->
            [Sig|Acc];
        false ->
            <<Line:N/binary, Rest/binary>> = Sig,
            sig_to_list(Rest, N, [Line|Acc])
    end.

%% @doc Validate that all required headers are present
%%
%% Returns 'ok' if all required headers are present.  Otherwise, throws
%% `{missing, [header_name()]}' providing a list of the
%% missing headers in the exception.
%%
%% @throws {missing, [binary()]} | bad_clock | bad_sign_desc
%%
-spec validate_headers(header_fun(), time_skew()) -> 'ok' | no_return().
validate_headers(GetHeader, TimeSkew) ->
    Missing = [ H || H <- ?required_headers, GetHeader(H) == undefined ],
    case Missing of
        [] ->
            validate_time_in_bounds(GetHeader, TimeSkew),
            validate_sign_description(GetHeader);
        TheList -> throw({missing_headers, TheList})
    end.

%% @doc Validate that the request time is within `TimeSkew' seconds of now.
%%
%% Returns 'ok' if request time in the X-Ops-Timestamp header is
%% wihtin bounds.  Otherwise, throws `bad_clock'
%%
%% @throws bad_clock
%%
-spec validate_time_in_bounds(header_fun(), time_skew()) -> 'ok' | no_return().
validate_time_in_bounds(GetHeader, TimeSkew) ->
    ReqTime = GetHeader(<<"X-Ops-Timestamp">>),
    case time_in_bounds(ReqTime, TimeSkew) of
        true -> ok;
        false -> throw(bad_clock)
    end.

%% @doc Validate that the X-Ops-Sign header describes a supported signing format.
%%
%% Returns 'ok' if the signing format is supported.  Otherwise, throws
%% `bad_sign_desc'
%%
%% @throws bad_sign_desc
%%
-spec validate_sign_description(header_fun()) -> 'ok' | no_return().
validate_sign_description(GetHeader) ->
    SignDesc = parse_signing_description(GetHeader(<<"X-Ops-Sign">>)),
    SignVersion = proplists:get_value(?signing_version_key, SignDesc),
    try
        ?signing_version = SignVersion,
        ok
    catch
        error:{badmatch, _} -> throw(bad_sign_desc)
    end.

%% @doc Determine if a request is valid
%%
%% The `GetHeader' argument is a fun that closes over the request
%% headers and can be called to obtain the value of a header.  It
%% should either return the value of the header as binary or
%% 'undefined'.
%%
%% A request signed with a timestamp more than `TimeSkew' seconds from
%% now will not be authenticated.
%%
%% `PublicKey' is a binary containing an RSA public key in PEM format.
%%
-spec authenticate_user_request(get_header_fun(),
                                   http_method(),
                                   http_path(),
                                   http_body(),
				   raw_public_key(),
                                   time_skew()) ->
				       {name, user_id()} | {no_authn, Reason::term()}.
authenticate_user_request(GetHeader, Method, Path, Body, PublicKey, TimeSkew) ->
    try
        validate_headers(GetHeader, TimeSkew),
        do_authenticate_user_request(GetHeader, Method, Path, Body, PublicKey)
    catch
        throw:Why -> {no_authn, Why}
    end.

-spec do_authenticate_user_request(get_header_fun(), 
				   http_method(),
				   http_path(),
				   http_body(),
				   raw_public_key() ) 
				  ->  {name, user_id()} | {no_authn, bad_sig}.

do_authenticate_user_request(GetHeader, Method, Path, Body, PublicKey) ->
    % NOTE: signing description validation and time_skew validation
    % are done in the wrapper function.
    UserId = GetHeader(<<"X-Ops-UserId">>),
    ReqTime = GetHeader(<<"X-Ops-Timestamp">>),
    AuthSig = sig_from_headers(GetHeader, 1, []),
    Decrypted = decrypt_sig(AuthSig, PublicKey),
    Plain = canonicalize_request(hashed_body(Body), UserId, Method, ReqTime,
                                 Path),
    try
        Decrypted = Plain,
        {name, UserId}
    catch
        error:{badmatch, _} -> {no_authn, bad_sig}
    end.

-spec decrypt_sig(binary(), raw_public_key()) -> binary() | decrypt_failed.
decrypt_sig(Sig, PublicCert) ->
    PK = read_cert(PublicCert),
    try
        public_key:decrypt_public(base64:decode(Sig), PK)
    catch
        error:decrypt_failed ->
            decrypt_failed
    end.

sig_from_headers(GetHeader, I, Acc) ->
    Header = xops_header(I),
    case GetHeader(Header) of
        undefined ->
            iolist_to_binary(lists:reverse(Acc));
        Part ->
            sig_from_headers(GetHeader, I+1, [Part|Acc])
    end.

-spec time_in_bounds(undefined | string() | binary(), pos_integer()) -> boolean().
time_in_bounds(undefined, _Skew) ->
    false;
time_in_bounds(ReqTime, Skew) ->
    Now = calendar:now_to_universal_time(erlang:now()),
    time_in_bounds(time_iso8601_to_date_time(ReqTime), Now, Skew).

-spec time_in_bounds(erlang_time(), erlang_time(), pos_integer() ) -> boolean().
time_in_bounds(T1, T2, Skew) ->
    S1 = calendar:datetime_to_gregorian_seconds(T1),
    S2 = calendar:datetime_to_gregorian_seconds(T2),
    (S2 - S1) < Skew.

-spec parse_signing_description('undefined' | binary()) -> [{binary(),binary()}].
parse_signing_description(undefined) ->
    [];
parse_signing_description(Desc) ->
    [ {Key, Value} ||
        [Key, Value] <- [ re:split(KV, "=") || KV <- re:split(Desc, ";") ] ].

% % --
% %% at some point, the functions in the public_key module should be
% %% sufficient, but for now we need the following to read in an RSA
% %% public key.
% read_rsa_public_key(Key) ->
%     Bin = erlang:iolist_to_binary(public_key_lines(re:split(Key, "\n"), [])),
%     Spki = public_key:der_decode('SubjectPublicKeyInfo', base64:mime_decode(Bin)),
%     {_, _, {0, KeyDer}} = Spki,
%     public_key:der_decode('RSAPublicKey', KeyDer).

% public_key_lines([<<"-----BEGIN PUBLIC KEY-----">>|Rest], Acc) ->
%     public_key_lines(Rest, Acc);
% public_key_lines([<<"-----END PUBLIC KEY-----">>|_Rest], Acc) ->
%     lists:reverse(Acc);
% public_key_lines([Line|Rest], Acc) ->
%     public_key_lines(Rest, [Line|Acc]).

-spec read_cert(binary()) -> term().  %% der_decode only spec's term
read_cert(Bin) when is_binary(Bin) ->
    Cert = public_key:pem_entry_decode(hd(public_key:pem_decode(Bin))),
    TbsCert = Cert#'Certificate'.tbsCertificate,
    Spki = TbsCert#'TBSCertificate'.subjectPublicKeyInfo,
    {0, KeyDer} = Spki#'SubjectPublicKeyInfo'.subjectPublicKey,
    public_key:der_decode('RSAPublicKey', KeyDer).

-ifdef(TEST).

-define(path, <<"/organizations/clownco">>).
-define(hashed_path, <<"YtBWDn1blGGuFIuKksdwXzHU9oE=">>).

-define(body, <<"Spec Body">>).
-define(hashed_body, <<"DFteJZPVv6WKdQmMqZUQUumUyRs=">>).
-define(request_time_http, <<"Thu, 01 Jan 2009 12:00:00 GMT">>).
-define(request_time_iso8601, <<"2009-01-01T12:00:00Z">>).
-define(user, <<"spec-user">>).

-define(X_OPS_AUTHORIZATION_LINES,
        [
         <<"jVHrNniWzpbez/eGWjFnO6lINRIuKOg40ZTIQudcFe47Z9e/HvrszfVXlKG4">>,
         <<"NMzYZgyooSvU85qkIUmKuCqgG2AIlvYa2Q/2ctrMhoaHhLOCWWoqYNMaEqPc">>,
         <<"3tKHE+CfvP+WuPdWk4jv4wpIkAz6ZLxToxcGhXmZbXpk56YTmqgBW2cbbw4O">>,
         <<"IWPZDHSiPcw//AYNgW1CCDptt+UFuaFYbtqZegcBd2n/jzcWODA7zL4KWEUy">>,
         <<"9q4rlh/+1tBReg60QdsmDRsw/cdO1GZrKtuCwbuD4+nbRdVBKv72rqHX9cu0">>,
         <<"utju9jzczCyB+sSAQWrxSsXB/b8vV2qs0l4VD2ML+w==">>
        ]).

-define(X_OPS_CONTENT_HASH, <<"DFteJZPVv6WKdQmMqZUQUumUyRs=">>).

-define(expected_sign_string,
        iolist_to_binary(io_lib:format(
                           "Method:~s\nHashed Path:~s\n"
                           "X-Ops-Content-Hash:~s\n"
                           "X-Ops-Timestamp:~s\n"
                           "X-Ops-UserId:~s",
                           ["POST", ?hashed_path, ?hashed_body,
                            ?request_time_iso8601, ?user]))).

hashed_path_test() ->
    ?assertEqual(?hashed_path, hash_string(canonical_path(?path))).

hashed_body_test() ->
    ?assertEqual(?hashed_body, hashed_body(?body)).

canonical_time_test() ->
    % This date format comes from Ruby's default printing,
    % but doesn't correspond to the HTTP rfc2616 format
    % Time = "Thu Jan 01 12:00:00 -0000 2009",
    ?assertEqual(?request_time_iso8601, canonical_time(?request_time_http)).
    
canonicalize_request_test() ->
    Val1 = canonicalize_request(?hashed_body, ?user, <<"post">>, ?request_time_iso8601, ?path),
    ?assertEqual(?expected_sign_string, Val1),

    % verify normalization
    Val2 = canonicalize_request(?hashed_body, ?user, <<"post">>, ?request_time_iso8601,
                                <<"/organizations//clownco/">>),
    ?assertEqual(?expected_sign_string, Val2).

sign_request_test() ->
    {ok, RawKey} = file:read_file("../test/private_key"),
    Private_key = extract_private_key(RawKey),
    AuthLine = fun(I) -> lists:nth(I, ?X_OPS_AUTHORIZATION_LINES) end,
    EXPECTED_SIGN_RESULT =
        [
         {<<"X-Ops-Content-Hash">>, ?X_OPS_CONTENT_HASH},
         {<<"X-Ops-UserId">>, ?user},
         {<<"X-Ops-Sign">>, <<"version=1.0">>},
         {<<"X-Ops-Timestamp">>, ?request_time_iso8601},
         {<<"X-Ops-Authorization-1">>, AuthLine(1)},
         {<<"X-Ops-Authorization-2">>, AuthLine(2)},
         {<<"X-Ops-Authorization-3">>, AuthLine(3)},
         {<<"X-Ops-Authorization-4">>, AuthLine(4)},
         {<<"X-Ops-Authorization-5">>, AuthLine(5)},
         {<<"X-Ops-Authorization-6">>, AuthLine(6)}
        ],
    Sig = sign_request(Private_key, ?body, ?user, <<"post">>,
                       ?request_time_http, ?path),
    ?assertEqual(EXPECTED_SIGN_RESULT, Sig).

decrypt_sig_test() ->
    AuthSig = iolist_to_binary(?X_OPS_AUTHORIZATION_LINES),
    {ok, Public_key} = file:read_file("../test/example_cert.pem"),
    ?assertEqual(?expected_sign_string, decrypt_sig(AuthSig, Public_key)).

time_in_bounds_test() ->
    T1 = {{2011,1,26},{2,3,0}},

    % test seconds
    T2 = {{2011,1,26},{2,3,4}},
    ?assertEqual(false, time_in_bounds(T1, T2, 2)),
    ?assertEqual(true, time_in_bounds(T1, T2, 5)),

    % test minutes
    T3 = {{2011,1,26},{2,6,0}},
    ?assertEqual(false, time_in_bounds(T1, T3, 60*2)),
    ?assertEqual(true, time_in_bounds(T1, T3, 60*5)),

    % test hours
    T4 = {{2011,1,26},{4,0,0}},
    ?assertEqual(false, time_in_bounds(T1, T4, 60*60)),
    ?assertEqual(true, time_in_bounds(T1, T4, 60*60*3)).

make_skew_time() ->
    % force time skew to allow for now
    ReqTimeEpoch = calendar:datetime_to_gregorian_seconds(
                     time_iso8601_to_date_time(?request_time_iso8601)),
    NowEpoch = calendar:datetime_to_gregorian_seconds(
                 calendar:now_to_universal_time(erlang:now())),
    (NowEpoch - ReqTimeEpoch) + 100.
    
authenticate_user_request_test_() ->
    {ok, RawKey} = file:read_file("../test/private_key"),
    Private_key = extract_private_key(RawKey),
    {ok, Public_key} = file:read_file("../test/example_cert.pem"),
    Headers = sign_request(Private_key, ?body, ?user, <<"post">>,
                           ?request_time_http, ?path),
    GetHeader = fun(X) -> proplists:get_value(X, Headers) end,
    % force time skew to allow a request to be processed 'now'
    TimeSkew = make_skew_time(),

    [
     {"authenticated user request",
      fun() ->
              Ok = authenticate_user_request(GetHeader, <<"post">>, ?path, ?body,
                                             Public_key, TimeSkew),
              ?assertEqual({name, ?user}, Ok)
      end
     },

     {"no_authn: bad path",
      fun() ->
              BadPath = authenticate_user_request(GetHeader, <<"post">>,
                                                  <<"/organizations/foo">>,
                                                  ?body, Public_key, TimeSkew),
              ?assertEqual({no_authn, bad_sig}, BadPath)
      end
     },

     {"no_authn: bad method",
      fun() ->
              BadMethod = authenticate_user_request(GetHeader, <<"PUT">>, ?path,
                                                    ?body, Public_key, TimeSkew),
              ?assertEqual({no_authn, bad_sig}, BadMethod)
      end
     },

     {"no_authn: bad body",
      fun() ->
              BadBody = authenticate_user_request(GetHeader, <<"post">>, ?path,
                                                  <<"xyz">>, Public_key, TimeSkew),
              ?assertEqual({no_authn, bad_sig}, BadBody)
      end
     },

     {"no_authn: bad time",
      fun() ->
              BadTime = authenticate_user_request(GetHeader, <<"post">>, ?path,
                                                  ?body, Public_key, 600),
              ?assertEqual({no_authn, bad_clock}, BadTime)
      end
      },

     {"no_authn: bad key",
      fun() ->
              {ok, Other_key} = file:read_file("../test/other_cert.pem"),
              BadKey = authenticate_user_request(GetHeader, <<"post">>, ?path,
                                                 ?body, Other_key, TimeSkew),
              ?assertEqual({no_authn, bad_sig}, BadKey)
      end
      },

     {"no_authn: missing timestamp header",
      fun() ->
              Headers2 = proplists:delete(<<"X-Ops-Timestamp">>, Headers),
              GetHeader2 = fun(X) -> proplists:get_value(X, Headers2) end,
              ?assertEqual({no_authn, {missing_headers, [<<"X-Ops-Timestamp">>]}},
                           authenticate_user_request(GetHeader2, <<"post">>, ?path,
                                                     ?body, Public_key, TimeSkew))
      end
     },

     {"no_authn: missing user header",
      fun() ->
              Headers2 = proplists:delete(<<"X-Ops-UserId">>, Headers),
              GetHeader2 = fun(X) -> proplists:get_value(X, Headers2) end,
              ?assertEqual({no_authn, {missing_headers, [<<"X-Ops-UserId">>]}},
                           authenticate_user_request(GetHeader2, <<"post">>, ?path,
                                                     ?body, Public_key, TimeSkew))
      end
     },

     {"no_authn: missing all authorization-i headers",
      fun() ->
              Headers2 = lists:filter(
                           fun({<<"X-Ops-Authorization-", _/binary>>, _}) -> false;
                              (_Else) -> true
                           end, Headers),
              GetHeader2 = fun(X) -> proplists:get_value(X, Headers2) end,
              ?assertEqual({no_authn, bad_sig},
                           authenticate_user_request(GetHeader2, <<"post">>, ?path,
                                                     ?body, Public_key, TimeSkew))
      end
     },

     {"no_authn: missing one authorization-i header",
      fun() ->
              Headers2 = lists:filter(
                           fun({<<"X-Ops-Authorization-5", _/binary>>, _}) -> false;
                              (_Else) -> true
                           end, Headers),
              GetHeader2 = fun(X) -> proplists:get_value(X, Headers2) end,
              ?assertEqual({no_authn, bad_sig},
                           authenticate_user_request(GetHeader2, <<"post">>, ?path,
                                                     ?body, Public_key, TimeSkew))
      end
     },

     {"no_authn: mismatched signing description",
      fun() ->
              Headers2 = lists:keyreplace(<<"X-Ops-Sign">>, 1, Headers,
                                          {<<"X-Ops-Sign">>, <<"version=2.0">>}),
              GetHeader2 = fun(X) -> proplists:get_value(X, Headers2) end,
              ?assertEqual({no_authn, bad_sign_desc},
                           authenticate_user_request(GetHeader2, <<"post">>, ?path,
                                                     ?body, Public_key, TimeSkew))
      end
     },

     {"no_authn: missing signing description",
      fun() ->
              Headers2 = lists:keydelete(<<"X-Ops-Sign">>, 1, Headers),
              GetHeader2 = fun(X) -> proplists:get_value(X, Headers2) end,
              ?assertEqual({no_authn, {missing_headers, [<<"X-Ops-Sign">>]}},
                            authenticate_user_request(GetHeader2, <<"post">>, ?path,
                                                      ?body, Public_key, TimeSkew))
      end
     }
     ].

validate_headers_test_() ->
    {ok, RawKey} = file:read_file("../test/private_key"),
    Private_key = extract_private_key(RawKey),
    Headers = sign_request(Private_key, ?body, ?user, <<"post">>,
                           httpd_util:rfc1123_date(), ?path),
    GetHeader = fun(X) -> proplists:get_value(X, Headers) end,
    MissingOneTests =
        [ fun() ->
                  Headers2 = proplists:delete(H, Headers),
                  GetHeader2 = fun(X) -> proplists:get_value(X, Headers2) end,
                  ?assertThrow({missing_headers, [H]}, validate_headers(GetHeader2, 10))
          end || H <- ?required_headers ],
    [?_assertEqual(ok, validate_headers(GetHeader, 1)),
     ?_assertThrow({missing_headers, ?required_headers},
                   validate_headers(fun(_X) -> undefined end, 0)) ]
        ++ MissingOneTests.

parse_signing_description_test_() ->
    Cases = [{<<"version=1.0">>, [{<<"version">>, <<"1.0">>}]},
             {undefined, []},
             {<<"a=1;b=2">>, [{<<"a">>, <<"1">>}, {<<"b">>, <<"2">>}]}],
    [ ?_assertEqual(Want, parse_signing_description(In))
      || {In, Want} <- Cases ].

read_cert_test() ->
    {ok, Bin} = file:read_file("../test/example_cert.pem"),
    Cert = read_cert(Bin),
    ?assertEqual('RSAPublicKey', erlang:element(1, Cert)).

-endif.
