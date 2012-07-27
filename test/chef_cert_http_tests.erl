%% -*- erlang-indent-level: 4;indent-tabs-mode: nil; fill-column: 92-*-
%% ex: ts=4 sw=4 et
%% @author James Casey <james@opscode.com>
%% @copyright 2012 Opscode, Inc.

-module(chef_cert_http_tests).


-define(cert_url, "http://localhost:5140/certificate").

-include_lib("eunit/include/eunit.hrl").

setup_env() ->
    ok = application:set_env(chef_common, certificate_root_url, ?cert_url).

make_response_body(Cert, KeyPair) ->
    ejson:encode({[{<<"cert">>, Cert}, {<<"keypair">>, KeyPair}]}).

simple_test_() ->
    MockedModules = [ibrowse],
    %% request data
    Guid = <<"foo">>,
    RequestId = <<"1234567890123">>,
    %% response data
    Cert = <<"foobarcert">>,
    Keypair = <<"bazbuzzkeypair">>,
    {foreach,
     fun() ->
             setup_env(),
             test_utils:mock(MockedModules)
     end,
     fun(_) ->
              meck:unload()
     end,
    [{"Simple success test",
      fun() -> meck:expect(ibrowse, send_req,
                           fun(_Url, _Headers, post, _Body) ->
                                    {ok, "200", [], make_response_body(Cert, Keypair)}
                            end),
               {GotCert, GotKeypair} = chef_cert_http:gen_cert(Guid, RequestId),
               ?assertEqual(Cert, GotCert),
               ?assertEqual(Keypair, GotKeypair)
               end},
     {"404 test",
      fun() -> meck:expect(ibrowse, send_req,
                           fun(_Url, _Headers, post, _Body) ->
                               {ok, "404", [], []}
                           end),

               ?assertThrow({error, {not_found, {"404", [], []}}}, chef_cert_http:gen_cert(Guid, RequestId))
      end},
     {"500 test",
      fun() -> meck:expect(ibrowse, send_req,
                           fun(_Url, _Headers, post, _Body) ->
                               {ok, "500", [], []}
                           end),
               ?assertThrow({error, {server_error, {"500", [], []}}}, chef_cert_http:gen_cert(Guid, RequestId))
      end},
     {"403 test",
      fun() -> meck:expect(ibrowse, send_req,
                           fun(_Url, _Headers, post, _Body) ->
                               {ok, "403", [], []}
                           end),
               ?assertThrow({error, {client_error, {"403",[], []}}}, chef_cert_http:gen_cert(Guid, RequestId))
      end}
    ]}.
