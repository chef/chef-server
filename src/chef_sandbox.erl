%% -*- erlang-indent-level: 4;indent-tabs-mode: nil; fill-column: 92-*-
%% ex: ts=4 sw=4 et
%% @author Seth Falcon <seth@opscode.com>
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


-module(chef_sandbox).

-export([
         parse_binary_json/2
        ]).

-ifdef(TEST).
-compile(export_all).
-endif.

-include("chef_types.hrl").

%% Maximum size of raw JSON sandbox data, in bytes
-define(MAX_SIZE, 1000000).

-define(VALIDATION_CONSTRAINTS,
        [
         {<<"checksums">>, is_ejson_proplist}
         ]).

-define(BAD_CHECKSUM_MSG, <<"Invalid checksum in sandbox.">>).

%% @doc Convert a binary JSON string representing a Chef Role into an
%% EJson-encoded Erlang data structure.
%% @end
-spec parse_binary_json( binary(), create ) -> { ok, ejson_term() }. % or throw
parse_binary_json(Bin, create) ->
    Size = iolist_size(Bin),
    case Size > ?MAX_SIZE of
        true ->
            Msg = iolist_to_binary([<<"Sandbox JSON must be less than ">>,
                                    integer_to_list(?MAX_SIZE),
                                    <<" bytes. You sent ">>,
                                    integer_to_list(Size),
                                    <<" bytes.">>]),
            throw({json_too_large, Msg});
        false -> ok
    end,
    Sandbox = ejson:decode(Bin),
    validate_sandbox(Sandbox, create).


-spec validate_sandbox(ejson_term(), create) -> {ok, ejson_term()}.
validate_sandbox(Sandbox, create) ->
    case chef_json_validator:validate_json_by_regex_constraints(Sandbox, ?VALIDATION_CONSTRAINTS) of
        ok ->
            case ej:get({<<"checksums">>}, Sandbox) of
                {[]} ->
                    throw({empty_checksums, <<"A sandbox must contain at least one checksum">>});
                {Checksums} ->
                    case valid_checksums(Checksums) of
                        ok -> {ok, Sandbox};
                        Why -> throw(Why)
                    end
            end;
        Bad ->
            throw(Bad)
    end.

valid_checksums([{Checksum, null}| Rest]) ->
    case is_md5_hex(Checksum) of
        true ->
            valid_checksums(Rest);
        false ->
            {bad_checksum, ?BAD_CHECKSUM_MSG}
    end;
valid_checksums([_|_Rest]) ->
    {bad_checksum, ?BAD_CHECKSUM_MSG};
valid_checksums([]) ->
    ok.


is_md5_hex(<<Bin:32/binary>>) ->
    is_hex(Bin);
is_md5_hex(_) ->
    false.

is_hex(<<>>)                 -> true;
is_hex(<<"0", Rest/binary>>) -> is_hex(Rest);
is_hex(<<"1", Rest/binary>>) -> is_hex(Rest);
is_hex(<<"2", Rest/binary>>) -> is_hex(Rest);
is_hex(<<"3", Rest/binary>>) -> is_hex(Rest);
is_hex(<<"4", Rest/binary>>) -> is_hex(Rest);
is_hex(<<"5", Rest/binary>>) -> is_hex(Rest);
is_hex(<<"6", Rest/binary>>) -> is_hex(Rest);
is_hex(<<"7", Rest/binary>>) -> is_hex(Rest);
is_hex(<<"8", Rest/binary>>) -> is_hex(Rest);
is_hex(<<"9", Rest/binary>>) -> is_hex(Rest);
is_hex(<<"a", Rest/binary>>) -> is_hex(Rest);
is_hex(<<"b", Rest/binary>>) -> is_hex(Rest);
is_hex(<<"c", Rest/binary>>) -> is_hex(Rest);
is_hex(<<"d", Rest/binary>>) -> is_hex(Rest);
is_hex(<<"e", Rest/binary>>) -> is_hex(Rest);
is_hex(<<"f", Rest/binary>>) -> is_hex(Rest);
is_hex(<<"A", Rest/binary>>) -> is_hex(Rest);
is_hex(<<"B", Rest/binary>>) -> is_hex(Rest);
is_hex(<<"C", Rest/binary>>) -> is_hex(Rest);
is_hex(<<"D", Rest/binary>>) -> is_hex(Rest);
is_hex(<<"E", Rest/binary>>) -> is_hex(Rest);
is_hex(<<"F", Rest/binary>>) -> is_hex(Rest);
is_hex(_)                    -> false.
