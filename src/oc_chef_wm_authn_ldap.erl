%% -*- erlang-indent-level: 4;indent-tabs-mode: nil -*-
%% ex: ts=4 sw=4 et
%% @author Marc Paradise <marc@getchef.com>
%% Copyright 2014 Chef, Inc. All Rights Reserved.
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
%
-module(oc_chef_wm_authn_ldap).

-export([auth_method/1, authenticate/2]).


%% Determines auth method to use for this request based on
%% configuration and any override present in the request data.
-spec auth_method(term()) -> ldap | local.
auth_method(Req) ->
    auth_method(envy:get(oc_chef_wm, ldap, list), Req).

auth_method(undefined, _Req) ->
    local;
auth_method(Config, Req) when is_list(Config) ->
    auth_method_for_request(proplists:get_value(host, Config), Req).

auth_method_for_request(undefined, _Req) ->
    local;
auth_method_for_request(_HostValue, Req) ->
    case wrq:get_qs_value("local", Req) of
        undefined -> ldap;
        _X ->        local
    end.


%% Open a direct connection to a configure LDAP server and authenticate the user
%% with credentials provided. Note that it connects to the LDAP server at time of request
%% and does not maintain an open connection. This is the same method used by opsocde-acocunt,
%% and is something we might wish to revisit.
-spec authenticate(string(), string()) -> {ok, term()} |
                                          {error, connection} |
                                          {error, unauthorized}.

authenticate(User, Password) ->
    Config = envy:get(oc_chef_wm, ldap, list),
    Host = proplists:get_value(host, Config),
    Timeout = proplists:get_value(timeout, Config, 60000),
    Port = proplists:get_value(port, Config, 389),
    Connection = eldap:open([Host], [{port, Port}, {timeout, Timeout}]),
    Result = try
        find_and_authenticate_user(Connection, User, Password, Timeout, Config)
    catch
        _Class:_Reason ->
            {error, connection}
    end,
    close(Connection),
    Result.


find_and_authenticate_user({ok, Session}, User, Password, Timeout, Config) ->
    BindDN = proplists:get_value(bind_dn, Config),
    BindPass = proplists:get_value(bind_password, Config),
    BaseDN = proplists:get_value(base_dn, Config),
    LoginAttr = proplists:get_value(login_attribute, Config, "samaccountname"),
    Encryption = proplists:get_value(encryption, Config, false),
    Base = {base, BaseDN},
    Filter = {filter, eldap:equalityMatch(LoginAttr, User)},

    ok = encrypt_session(Encryption, Session, Timeout),

    % Auth so we can search for the user
    ok = bind(Session, BindDN, BindPass),

    % And then search
    {ok, {eldap_search_result, Result, _}} = eldap:search(Session, [Base, Filter]),
    case result_to_user_ejson(LoginAttr, User, Result) of
        {error, Reason} ->
            {error, Reason};
        {CN, UserName, Data} ->
            % We found the user identified by username, now we need to
            % see if we can authorize as that user, using the provided password.
            case eldap:simple_bind(Session, CN, Password) of
                ok -> {UserName, Data};
                Error ->
                    lager:info("ldap authentication failed for ~p/~p: ~p", [User, UserName, Error]),
                    {error, unauthorized}
            end
    end;
find_and_authenticate_user(Data,_,_,_,_) ->
    lager:error("ldap connection failed: ~p", [Data]),
    {error, connection}.

bind(Session, BindDN, BindPassword) ->
    case eldap:simple_bind(Session, BindDN, BindPassword) of
        ok -> ok;
        {error, Error} ->
            lager:error("Could not bind as ~p, please check private-chef.rb for correct ldap['bind_dn'] and ldap['bind_password']", [BindDN]),
            {error, Error}
    end.

encrypt_session(false, _, _) ->
    ok;
encrypt_session(true, _Session, _Timeout) ->
    %case eldap:start_tls(Session, [], Timeout) of
    %    {error, tls_already_started} -> ok; % connection secure
    %    {error, {response, _Any}} -> ok; % connection is still good but not secure.
    %    ok -> ok;
    %    _ -> {error, connection}
    %end.
    % R16B03 introduces TLS upgrade, otherwise they
    % should be connecting initially to a secure port.
    {error, encryption_not_supported}.



% plus: username, external_authentication_uid, recovery_authentication_enabled=false
result_to_user_ejson(_, UserName, []) ->
    lager:info("User ~p not found in LDAP", [UserName]),
    {error, unauthorized};
result_to_user_ejson(LoginAttr, _, [{eldap_entry, CN, DataIn}|_]) ->

    % No guarantees on casing, so let's not make assumptions:
    Data = [ { string:to_lower(Key), Value} || {Key, Value} <- DataIn ],

    % loginattr was used to find this record, so we know it must exist
    [UserName0] = proplists:get_value(LoginAttr, Data),
    UserName1 = string:to_lower(UserName0),
    UserName2 = re:replace(UserName1, "[^0-z0-9_-]", "_", [{return ,list}, global]),
    UserName = characters_to_binary(UserName2),

    % If you are debugging an issue where a new user has authenticated successfully
    % via opscode-manage , but received an odd 400 message when trying to create a
    % new linked chef-server account, you've found the right place.
    %
    % The reason for this problem is that the user's directory entry with
    % the ldap provider does not have any valid email address associated with the
    % "mail" attribute.  Resolve it by updating the directory entry.
    %
    % Note that any missing fields in the user's directory entry (LookupFields)
    % will have "unknown" substituted in the returned json record.
    LookupFields = [{"displayname", <<"display_name">>},
                    {"givenname", <<"first_name">>},
                    {"sn", <<"last_name">>},
                    {"c", <<"country">>},
                    {"l", <<"city">>},
                    {"mail", <<"email">>} ],

    Terms = [ {Name, value_of(Key, Data, "unknown") } || {Key, Name} <- LookupFields ],
    Result = Terms ++ [ { <<"username">>, UserName },
                        { <<"external_authentication_uid">>, UserName },
                        { <<"recovery_authentication_enabled">>, false } ],
    {CN, UserName, {Result}}.

close({ok, Session}) ->
    eldap:close(Session);
close(_) ->
    ok.

value_of(Key, Data, Default) ->
    [R] = proplists:get_value(Key, Data, [Default]),
    characters_to_binary(R).

characters_to_binary(Characters) when is_list(Characters);
                                      is_binary(Characters) ->
    unicode:characters_to_binary(Characters);
% In case of unexpected value, don't crash the auth process:
characters_to_binary(Atom) when is_atom(Atom) ->
    atom_to_binary(Atom, utf8).

