%% -*- erlang-indent-level: 4;indent-tabs-mode: nil; fill-column: 92 -*-
%% ex: ts=4 sw=4 et
%% @author Stephen Delano <stephen@chef.io>
%% Copyright 2013 Opscode, Inc. All Rights Reserved.

-module(chef_authn).

-export([
         authenticate_user_request/6,
         validate_headers/2,
         extract_public_or_private_key/1,
         extract_private_key/1,
         extract_public_key/1,
         extract_pem_encoded_public_key/1
        ]).

-include_lib("public_key/include/public_key.hrl").

authenticate_user_request(_GetHeader,
                          _Method,
                          _Path,
                          _Body,
                          _PublicKey,
                          _TimeSkew) ->
    {name, <<"mock_user">>}.

validate_headers(_GetHeader, _TimeSkew) ->
    [{'algorithm', <<"SHA1">>},
     {'version', <<"1.0">>}].


%% @doc Given PEM content as binary, return either an RSA public or private key record (or
%% error tuple). The PEM can contain an RSA public key in PKCS1, SPKI (X509), or an X509
%% certificate wrapping an SPKI formatted key. Note that private keys will not be extracted
%% from X509 certificate data.
-spec extract_public_or_private_key(binary()) -> #'RSAPublicKey'{}  |
                                                 #'RSAPrivateKey'{} |
                                                 {error, bad_key}.
extract_public_or_private_key(RawKey) ->
    try
        [Key] = public_key:pem_decode(RawKey),
        process_key(Key)
    catch
        _:_ ->
            {error, bad_key}
    end.

-spec extract_public_key(binary()) -> #'RSAPublicKey'{} | {error, bad_key}.
extract_public_key(RawKey) ->
    case extract_public_or_private_key(RawKey) of
        #'RSAPublicKey'{} = Key ->
            Key;
        _ ->
            {error, bad_key}
    end.

-spec extract_private_key(binary()) -> #'RSAPrivateKey'{} | {error, bad_key}.
extract_private_key(RawKey) ->
    case extract_public_or_private_key(RawKey) of
        #'RSAPrivateKey'{} = Key ->
            Key;
        _ ->
            {error, bad_key}
    end.

%% @doc Given PEM X509 certificate as a binary, return the RSA public key
%% in PEM format. If the argument is not a certificate, bad_key will be returned.
-spec extract_pem_encoded_public_key(binary()) -> binary() | {error, bad_key}.
extract_pem_encoded_public_key( <<"-----BEGIN CERTIFICATE", _Bin/binary>> = RawCert) ->
    try
        DecodedPublicKey = extract_public_key(RawCert),
        EncodedEntry = public_key:pem_entry_encode('SubjectPublicKeyInfo', DecodedPublicKey),
        public_key:pem_encode([EncodedEntry])
    catch
        _:_ ->
            {error, bad_key}
    end;
extract_pem_encoded_public_key(_) ->
    {error, bad_key}.

-spec process_key({'RSAPublicKey',  binary(), _} |
                  {'RSAPrivateKey', binary(), _} |
                  {'SubjectPublicKeyInfo', _, _}) ->
                         public_key:rsa_public_key() |
                         public_key:rsa_private_key() |
                         {error, bad_key}.
process_key({'SubjectPublicKeyInfo', _, _} = PubEntry) ->
    public_key:pem_entry_decode(PubEntry);
process_key({'RSAPublicKey', Der, _}) ->
    public_key:der_decode('RSAPublicKey', Der);
process_key({'RSAPrivateKey', Der, _}) ->
        public_key:der_decode('RSAPrivateKey', Der);
process_key({'Certificate', _Der, _} = CertEntry) ->
    %% NOTE: assumes the certificate contains public key info and only extracts that.
    Cert = public_key:pem_entry_decode(CertEntry),
    TbsCert = Cert#'Certificate'.tbsCertificate,
    Spki = TbsCert#'TBSCertificate'.subjectPublicKeyInfo,
    {0, KeyDer} = Spki#'SubjectPublicKeyInfo'.subjectPublicKey,
    public_key:der_decode('RSAPublicKey', KeyDer).
