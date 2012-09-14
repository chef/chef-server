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
-module(chef_wm_password).

-export([encrypt/1,
         migrate_sha1/2,
         verify/2]).

-ifdef(TEST).
-compile([export_all]).
-endif.

-define(DEFAULT_HASH_TYPE, <<"erlang-bcrypt-0.5.0">>).
-define(MIGRATION_HASH_TYPE, <<"sha1+bcrypt">>).

-type str_or_bin() :: string() | binary().

%% @doc Return hashed password, salt, and hash type. Uses bcrypt with
%% the number of rounds as configured in the bcrypt application key
%% `default_log_rounds'. It is an error to attempt to hash an empty
%% password, but no other restrictions are placed on the password.
-spec encrypt(Password) -> {HashedPassword, Salt, Type} when
      Password :: str_or_bin(),
      HashedPassword :: binary(),
      Salt :: binary(),
      Type :: binary().
encrypt(Empty) when Empty =:= "";
                    Empty =:= <<"">> ->
    erlang:error({invalid_password, Empty});
encrypt(Password) ->
    {ok, Salt} = bcrypt:gen_salt(),
    {ok, HashedPass} = bcrypt:hashpw(to_str(Password), Salt),
    {list_to_binary(HashedPass), list_to_binary(Salt), ?DEFAULT_HASH_TYPE}.

%% @doc Return `true' if the plain password hashes to the same value
%% using the specified hash type and salt.
-spec verify(Password, {HashedPass, Salt, HashType}) -> boolean() when
      Password :: str_or_bin(),
      HashedPass :: str_or_bin(),
      Salt :: str_or_bin(),
      HashType :: str_or_bin().
verify(Password, {HashedPass, Salt, ?DEFAULT_HASH_TYPE}) ->
    {ok, ThisHashedPass} = bcrypt:hashpw(to_str(Password), to_str(Salt)),
    slow_compare(ThisHashedPass, to_str(HashedPass));
verify(Password, {HashedPass, Salt, ?MIGRATION_HASH_TYPE}) ->
    verify_sha1_bcrypt(Password, HashedPass, Salt).

verify_sha1_bcrypt(Password, HashedPass, Salt) ->
    {OrigSalt, BcryptSalt} = parse_salt(Salt),
    %% sha takes an iolist
    SHA = hexstring(crypto:sha(["--", OrigSalt, "--", Password, "--"])),
    {ok, ThisHashedPass} = bcrypt:hashpw(SHA, BcryptSalt),
    slow_compare(ThisHashedPass, to_str(HashedPass)).

%% @doc Upgrade stored hashed password data that was already hashed
%% using `OrigSalt' and a SHA1 scheme. The resulting data will be
%% assigned a hash type of `<<"sha1+bcrypt">>'. The returned salt
%% value will be a compount salt delimited by `\t' with the first part
%% being a newly assigned bcrypt generated salt and the second
%% containing the original salt.
migrate_sha1(SHA, OrigSalt) ->
    {ok, Salt} = bcrypt:gen_salt(),
    {ok, HashedPass} = bcrypt:hashpw(to_str(SHA), Salt),
    CompoundSalt = compound_salt(OrigSalt, Salt),
    {list_to_binary(HashedPass), CompoundSalt, ?MIGRATION_HASH_TYPE}.

parse_salt(Salt) ->
    [BcryptSalt, OrigSalt] = re:split(Salt, "\t", [{return, list}]),
    {OrigSalt, BcryptSalt}.

compound_salt(OrigSalt, Salt) ->
    %% relies on \t not appearing in either salt value
    iolist_to_binary([Salt, <<"\t">>, OrigSalt]).

%% Using straight `=:=' could provide an opening for a timing attack
%% since shared prefix strings will take longer to determine unequal
%% than not.
slow_compare(S1, S2) when length(S1) =:= length(S2) ->
    lists:foldl(fun({X, X}, Acc) ->
                        Acc;
                   ({_, _}, _Acc) ->
                        false
                end,
                true,
                lists:zip(S1, S2));
slow_compare(_, _) ->
    false.

to_str(S) when is_list(S) ->
    S;
to_str(S) when is_binary(S) ->
    binary_to_list(S).

hexstring(<<X:160/big-unsigned-integer>>) ->
    lists:flatten(io_lib:format("~40.16.0b", [X])).
%% hexstring(<<X:128/big-unsigned-integer>>) ->
%%     lists:flatten(io_lib:format("~32.16.0b", [X]));
%% hexstring(<<X:256/big-unsigned-integer>>) ->
%%     lists:flatten(io_lib:format("~64.16.0b", [X]));
%% hexstring(<<X:512/big-unsigned-integer>>) ->
%%     lists:flatten(io_lib:format("~128.16.0b", [X])).

