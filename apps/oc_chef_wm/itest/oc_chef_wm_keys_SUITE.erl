
%% ex: ts=4 sw=4 et
%% @author Stephen Delano <stephen@opscode.com>
%% Copyright 2013 Opscode, Inc. All Rights Reserved.

-module(oc_chef_wm_keys_SUITE).

-include_lib("common_test/include/ct.hrl").
-include("../../../include/chef_types.hrl").
-include("../../../include/oc_chef_types.hrl").
-include_lib("eunit/include/eunit.hrl").

-compile([export_all, {parse_transform, lager_transform}]).

-define(ORG_AUTHZ_ID, <<"10000000000000000000000000000002">>).
-define(CLIENT_AUTHZ_ID, <<"00000000000000000000000000000003">>).
-define(USER_AUTHZ_ID, <<"00000000000000000000000000000004">>).
-define(ADMIN_AUTHZ_ID, <<"90000000000000000000000000000004">>).
-define(CLIENT_NAME, <<"client1">>).
-define(USER_NAME, <<"user1">>).
-define(ADMIN_USER_NAME, <<"admin">>).
-define(ORG_NAME, <<"testorg">>).

-define(KEY1NAME, <<"key1">>).
-define(KEY1EXPIRE, {datetime, {{2099,12,31},{00,00,00}}}).
-define(KEY2NAME, <<"key2">>).
-define(KEY2EXPIRE, {datetime, {{2010,12,31},{00,00,00}}}).

-define(DEFAULT_KEY_ENTRY, {<<"default">>, false}).
-define(KEY_1_ENTRY, { ?KEY1NAME, false } ).
-define(KEY_2_ENTRY, { ?KEY2NAME, true } ).

init_per_suite(LastConfig) ->
    Config = chef_test_db_helper:start_db(LastConfig, "oc_chef_wm_itests"),
    Config2 = setup_helper:start_server(Config),
    make_org(),
    OrgId = chef_db:fetch_org_id(context(), ?ORG_NAME),
    {ok, PubKey} = file:read_file("../../spki_public.pem"),
    [{org_id, OrgId}, {pubkey, PubKey}] ++ Config2.

end_per_suite(Config) ->
    setup_helper:base_end_per_suite(Config).

all() ->
    [list_client_default_key,
     list_user_default_key,
     list_client_multiple_keys,
     list_user_multiple_keys,
     list_client_no_keys,
     list_user_no_keys,
     post_user_new_valid_key,
     post_client_new_valid_key
     ].

%% Test cases.
list_client_default_key(_) ->
    Result = http_keys_request(get, client, ?CLIENT_NAME),
    ?assertMatch({ok, "200", _, _} , Result),
    BodyEJ = chef_json:decode(response_body(Result)),
    ExpectedEJ = client_key_list_ejson(?CLIENT_NAME, [?DEFAULT_KEY_ENTRY]),
    ?assertMatch(ExpectedEJ, BodyEJ),
    ok.

list_user_default_key(_) ->
    Result = http_keys_request(get, user, ?USER_NAME),
    ?assertMatch({ok, "200", _, _} , Result),
    BodyEJ = chef_json:decode(response_body(Result)),
    ExpectedEJ = user_key_list_ejson(?USER_NAME, [?DEFAULT_KEY_ENTRY]),
    ?assertMatch(ExpectedEJ, BodyEJ),
    ok.

list_client_multiple_keys(_) ->
    Result = http_keys_request(get, client, ?CLIENT_NAME),
    ?assertMatch({ok, "200", _, _} , Result),
    BodyEJ = chef_json:decode(response_body(Result)),
    ExpectedEJ = client_key_list_ejson(?CLIENT_NAME, [?DEFAULT_KEY_ENTRY, ?KEY_1_ENTRY, ?KEY_2_ENTRY]),
    ?assertMatch(ExpectedEJ, BodyEJ),
    ok.

list_user_multiple_keys(_) ->
    Result = http_keys_request(get, user, ?USER_NAME),
    ?assertMatch({ok, "200", _, _} , Result),
    BodyEJ = chef_json:decode(response_body(Result)),
    ExpectedEJ = user_key_list_ejson(?USER_NAME, [?DEFAULT_KEY_ENTRY, ?KEY_1_ENTRY, ?KEY_2_ENTRY]),
    ?assertMatch(ExpectedEJ, BodyEJ),
    ok.

list_client_no_keys(_) ->
    Result = http_keys_request(get, client, ?ADMIN_USER_NAME),
    ?assertMatch({ok, "200", _, "[]"} , Result),
    ok.

list_user_no_keys(_) ->
    Result = http_keys_request(get, user, ?ADMIN_USER_NAME),
    ?assertMatch({ok, "200", _, "[]"} , Result),
    ok.

post_client_new_valid_key(Config) ->
    Body = chef_json:encode(new_key_ejson(Config, <<"test1">>, <<"2099-10-24T22:49:08">>)),
    Result = http_keys_request(post, client, ?ADMIN_USER_NAME, Body),
    ?assertMatch({ok, "201", _, _}, Result).

post_user_new_valid_key(Config) ->
    Body = chef_json:encode(new_key_ejson(Config, <<"test1">>, <<"2099-10-25T22:49:08">>)),
    Result = http_keys_request(post, user, ?ADMIN_USER_NAME, Body),
    ?assertMatch({ok, "201", _, _}, Result).

%% Test case initializers
init_per_testcase(list_user_default_key,  Config) ->
    make_user(Config, ?USER_NAME, ?USER_AUTHZ_ID),
    Config;
init_per_testcase(list_client_default_key, Config) ->
    make_client(Config, ?CLIENT_NAME),
    Config;
init_per_testcase(list_client_multiple_keys, Config) ->
    make_client(Config, ?CLIENT_NAME),
    ClientId = client_id(Config, ?CLIENT_NAME),
    add_key(Config, ClientId, ?KEY1NAME, ?KEY1EXPIRE),
    add_key(Config, ClientId, ?KEY2NAME, ?KEY2EXPIRE),
    Config;
init_per_testcase(list_user_multiple_keys, Config) ->
    make_user(Config, ?USER_NAME, ?USER_AUTHZ_ID),
    UserId = user_id(?USER_NAME),
    add_key(Config, UserId, ?KEY1NAME, ?KEY1EXPIRE),
    add_key(Config, UserId, ?KEY2NAME, ?KEY2EXPIRE),
    Config;
init_per_testcase(list_client_no_keys, Config) ->
    make_client(Config, ?CLIENT_NAME),
    sqerl:adhoc_delete(<<"keys">>, all),
    % make this user after clearing keys, so that we have a user
    % who can make the request.
    make_user(Config, ?ADMIN_USER_NAME, ?ADMIN_AUTHZ_ID),
    Config;
init_per_testcase(list_user_no_keys, Config) ->
    make_user(Config, ?USER_NAME, ?USER_AUTHZ_ID),
    sqerl:adhoc_delete(<<"keys">>, all),
    make_user(Config, ?ADMIN_USER_NAME, ?ADMIN_AUTHZ_ID),
    %chef_db:fetch(#chef_user{username = ?ADMIN_USER_NAME}, context()),
    Config;
init_per_testcase(post_client_new_valid_key, Config) ->
    make_user(Config, ?ADMIN_USER_NAME, ?ADMIN_AUTHZ_ID),
    make_client(Config, ?CLIENT_NAME),
    Config;
init_per_testcase(post_user_new_valid_key, Config) ->
    make_user(Config, ?ADMIN_USER_NAME, ?ADMIN_AUTHZ_ID),
    make_user(Config, ?USER_NAME, ?USER_AUTHZ_ID),
    make_client(Config, ?CLIENT_NAME),
    Config;
init_per_testcase(_, Config) ->
    Config.

%% Test case cleanup
end_per_testcase(_, Config) ->
    sqerl:adhoc_delete("clients", all),
    sqerl:adhoc_delete("users", all),
    Config.

http_keys_request(Method, Type, Requestor) ->
    http_keys_request(Method, Type, Requestor, <<>>).

http_keys_request(Method, user, Requestor, Body) ->
    Url = "http://localhost:8000/users/user1/keys",
    ibrowse:send_req(Url, [{"x-ops-userid", binary_to_list(Requestor)},
                           {"accept", "application/json"},
                           {"content-type", "application/json"}], Method, Body);
http_keys_request(Method, client, Requestor, Body) ->
    Url = "http://localhost:8000/organizations/testorg/clients/client1/keys",
    ibrowse:send_req(Url, [{"x-ops-userid", binary_to_list(Requestor)},
                           {"accept", "application/json"},
                           {"content-type", "application/json"}], Method, Body).

% Some helpers to keep noise out of the tests...

make_org() ->
    Org = chef_object:new_record(oc_chef_organization, nil, ?ORG_AUTHZ_ID,
                                 {[{<<"name">>, ?ORG_NAME}, {<<"full_name">>, ?ORG_NAME}]}),
    chef_db:create(Org, context(), ?ORG_AUTHZ_ID).

make_client(Config, Name) ->
    OrgId = proplists:get_value(org_id, Config),
    PubKey = proplists:get_value(pubkey, Config),
    Client = chef_object:new_record(chef_client, OrgId, ?CLIENT_AUTHZ_ID,
                                    {[{<<"name">>, Name},
                                      {<<"validator">>, true},
                                      {<<"admin">>, true},
                                      {<<"public_key">>, PubKey}]}),
    chef_db:create(Client, context(), ?CLIENT_AUTHZ_ID).

make_user(Config, Name, AuthzId) ->
    OrgId = proplists:get_value(org_id, Config),
    PubKey = proplists:get_value(pubkey, Config),
    Dom = <<"@somewhere.com">>,
    User = chef_object:new_record(chef_user, OrgId, AuthzId,
                                   {[{<<"username">>, Name},
                                     {<<"password">>, <<"zuperzecret">>},
                                     {<<"email">>, <<Name/binary,Dom/binary>>},
                                     {<<"public_key">>, PubKey},
                                     {<<"display_name">>, <<"someone">>}]}),
    chef_db:create(User, context(), ?USER_AUTHZ_ID).

add_key(Config, Id, KeyName, ExpirationDate) ->
    PubKey = proplists:get_value(pubkey, Config),
    {ok, 1} = sqerl:execute(<<"INSERT INTO KEYS (id, key_name, public_key, key_version, created_at, expires_at, last_updated_by, updated_at)
                               VALUES ($1, $2, $3, 1, CURRENT_TIMESTAMP, $4, 'me', CURRENT_TIMESTAMP )">>,
                  [Id, KeyName, PubKey, ExpirationDate]).

response_body({_, _, _, Body}) ->
    Body.

context() ->
    chef_db:make_context(<<"AB">>).

client_id(Config, Name) ->
    OrgId = proplists:get_value(org_id, Config),
    #chef_client{id = ClientId} = chef_db:fetch(#chef_client{org_id = OrgId, name = Name}, context()),
    ClientId.

user_id(Name) ->
    #chef_user{id = UserId} = chef_db:fetch(#chef_user{username = Name}, context()),
    UserId.

%% Expected Results and Inputs
%%
user_key_list_ejson(Name, KeyInfo) ->
    Base = <<"http://localhost:8000/users/">>,
    Keys = <<"/keys/">>,
    key_list_ejson(<<Base/binary,Name/binary,Keys/binary>>, KeyInfo).
client_key_list_ejson(Name, KeyInfo) ->
    Base = <<"http://localhost:8000/organizations/testorg/clients/">>,
    Keys = <<"/keys/">>,
    key_list_ejson(<<Base/binary,Name/binary,Keys/binary>>, KeyInfo).

key_list_ejson(BaseURI, KeyInfo) ->
    [ {[{<<"uri">>, <<BaseURI/binary,KeyName/binary>>},
        {<<"name">>, KeyName},
        {<<"expired">>, Expired}] } || {KeyName, Expired} <- KeyInfo].

new_key_ejson(Config, Name, Expiration) ->
    PubKey = proplists:get_value(pubkey, Config),
    {[{name, Name}, {public_key, PubKey}, {expiration_date, Expiration}]}.

