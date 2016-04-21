%% -*- erlang-indent-level: 4;indent-tabs-mode: nil; fill-column: 92-*-
%% ex: ts=4 sw=4 et
%% @author Seth Falcon <seth@chef.io>
%% @author Marc Paradise <marc@chef.io>
%% Copyright 2012-14 Chef Software, Inc. All Rights Reserved.
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
-module(chef_password).

-export([encrypt/1,
         upgrade/2,
         verify/2]).

-ifdef(TEST).
-compile([export_all]).
-endif.

-define(DEFAULT_HASH_TYPE, <<"bcrypt">>).
-define(MIGRATION_HASH_TYPE, <<"SHA1-bcrypt">>).

-define(OSC_DEFAULT_HASH_TYPE, <<"erlang-bcrypt-0.5.0">>).
-define(OSC_MIGRATION_HASH_TYPE, <<"sha1+bcrypt">>).

-type str_or_bin() :: string() | binary().
-type password_data() :: { binary(),  binary(), binary() }.

%% @doc Return hashed password, salt, and hash type. Uses bcrypt with
%% the number of rounds as configured in the bcrypt application key
%% `default_log_rounds'. It is an error to attempt to hash an empty
%% password, but no other restrictions are placed on the password.
-spec encrypt(str_or_bin()) -> password_data().
encrypt(Empty) when Empty =:= "";
                    Empty =:= <<"">> ->
    erlang:error({invalid_password, Empty});
encrypt(Password) ->
    {ok, Salt} = bcrypt:gen_salt(),
    {ok, HashedPass} = bcrypt:hashpw(to_str(Password), Salt),
    {list_to_binary(HashedPass), list_to_binary(Salt), ?DEFAULT_HASH_TYPE}.

%% @doc if required, upgrade password encryption from SHA1 or migration type (OSC or EC)
%% to bcrypt.
%% Returns a password_data() tuple which will differ from the input
%% only if a password encryption upgrade was required.
-spec upgrade(str_or_bin(), password_data()) -> password_data().
upgrade(_Password, {HashedPass, Salt, ?DEFAULT_HASH_TYPE = Type}) ->
    {HashedPass, Salt, Type};
upgrade(Password, {_OldHash, _OldSalt, _AnyType}) ->
    % If the passowrd is not currently hashed via DEFAULT_HASH_TYPE,
    % we're going to upgrade it - regardless of whether it's
    % using a migrated hash type, or completely unconverted.
    encrypt(Password).

%% @doc Return `true' if the plain password hashes to the same value
%% using the specified hash type and salt. This supports multiple encoding schemas:
%% plain sha1+salt, OSC-migration/transitional, EC-migration/transitional, and
%% pure bcrypt.  It is expected that clients will call 'upgrade' upon successful
%% authentication and save the resulting password hash data if it has changed.
-spec verify(str_or_bin(), password_data()) -> boolean().
verify(Password, {HashedPass, Salt, ?OSC_DEFAULT_HASH_TYPE}) ->
    verify(Password, {HashedPass, Salt, ?DEFAULT_HASH_TYPE});
verify(Password, {HashedPass, _, ?DEFAULT_HASH_TYPE}) ->
    % the bcrypt library will automatically use the salt portion of the hashed password
    % string, so for bcrypt-encoded passwords we will provided the hashed password as the
    % salt value as well.
    {ok, ThisHashedPass} = bcrypt:hashpw(to_str(Password), to_str(HashedPass)),
    slow_compare(ThisHashedPass, to_str(HashedPass));
verify(Password, {HashedPass, Salt, ?MIGRATION_HASH_TYPE}) ->
    InterimPass = sha1(Salt, Password, ec),
    {ok, ThisHashedPass} = bcrypt:hashpw(to_str(InterimPass), to_str(HashedPass)),
    slow_compare(ThisHashedPass, to_str(HashedPass));
verify(Password, {HashedPass, Salt, ?OSC_MIGRATION_HASH_TYPE}) ->
    {OrigSalt, BcryptSalt} = parse_salt(Salt),
    SHA = sha1(OrigSalt, Password, osc),
    {ok, ThisHashedPass} = bcrypt:hashpw(SHA, BcryptSalt),
    slow_compare(ThisHashedPass, to_str(HashedPass));
verify(Password, {HashedPass, Salt, _}) ->
    % Support unconverted passwords too, for existing installations that have
    % not been upgraded in any way.
    ThisHashedPass = sha1(Salt, Password, ec),
    case slow_compare(ThisHashedPass, to_str(HashedPass)) of
        true ->
            true;
        false ->
            ThisHashedPass2 = sha1(Salt, Password, osc),
            slow_compare(ThisHashedPass2, to_str(HashedPass))
    end.

sha1(Salt, Password, osc) ->
    hexstring(crypto:hash(sha, ["--", Salt, "--", Password, "--"]));
sha1(Salt, Password, ec) ->
    hexstring(crypto:hash(sha, [Salt, "--", Password, "--"])).

hexstring(<<X:160/big-unsigned-integer>>) ->
    lists:flatten(io_lib:format("~40.16.0b", [X])).

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

parse_salt(Salt) ->
    [BcryptSalt, OrigSalt] = re:split(Salt, "\t", [{return, list}]),
    {OrigSalt, BcryptSalt}.

to_str(S) when is_list(S) ->
    S;
to_str(S) when is_binary(S) ->
    binary_to_list(S).

