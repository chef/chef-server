%% -*- erlang-indent-level: 4;indent-tabs-mode: nil; fill-column: 92 -*-
%% ex: ts=4 sw=4 et
%% @author Stephen Delano <stephen@opscode.com>
%% Copyright 2013 Opscode, Inc. All Rights Reserved.

-module(chef_authn).

-export([
         authenticate_user_request/6,
         validate_headers/2
        ]).

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
