-module(search_resource_test).

-include_lib("eunit/include/eunit.hrl").
-include("../src/chef_rest_search_resource.hrl").
-include("../../chef_common/src/chef_solr.hrl").

make_state() ->
    #state{start_time = now(),
           resource = "chef_rest_search_resource",
           batch_size = 5,
           estatsd_server = "127.0.0.1",
           estatsd_port = 3365,
           hostname = net_adm:localhost(),
           organization_name = "mock-org",
           request_type = "search.get"}.

set_env() ->
    application:set_env(chef_rest, bulk_fetch_batch_size, 10),
    application:set_env(chef_rest, estatsd_server, "192.168.4.50"),
    application:set_env(chef_rest, estatsd_port, 5656),
    application:set_env(chef_rest, auth_skew, 900),
    application:set_env(chef_rest, reqid_header_name, "X-Request-Id").


make_authorized_state(Type) ->
    State = make_state(),
    State#state{requester_type=Type}.

init_test_() ->
    {foreach,
     fun() ->
             set_env()
     end,
     fun(_) -> stopping end,
     [{"init basic",
       fun() ->
               {ok, State} = chef_rest_search_resource:init(ignored),
               ?assertEqual(10, State#state.batch_size),
               ?assertEqual(5656, State#state.estatsd_port),
               ?assertEqual("chef_rest_search_resource", State#state.resource)
       end}
     ]}.

malformed_request_test_() ->
    {setup,
     fun() ->
             application:start(crypto),
             set_env()
     end,
     fun(_) ->
             stopping
     end,
     malformed_request_tests()}.

malformed_request_tests() ->
    {foreach,
     fun() ->
             meck:new(wrq),
             meck:new(fast_log),
             meck:expect(fast_log, info, fun(_, _, _) -> ok end),
             meck:expect(fast_log, info, fun(_, _, _, _) -> ok end)
     end,
     fun(_) ->
             meck:unload()
     end,
    [
     {"properly formed",
      fun() ->
              SignedHeadersBin = get_signed_headers(),
              meck:expect(wrq, get_req_header,
                          fun(HName, req_mock) -> proplists:get_value(HName, SignedHeadersBin) end),
              meck:expect(wrq, path_info, fun(object_type, req_mock) ->
                                                  "node";
                                             (organization_id, req_mock) ->
                                                  "testorg"
                                          end),
              meck:expect(wrq, get_qs_value, fun("q", req_mock) ->
                                                     "myquery";
                                                ("start", req_mock) ->
                                                     "0";
                                                ("rows", req_mock) ->
                                                     "20";
                                                ("sort", req_mock) ->
                                                     "sort should be ignored."
                                             end),
              {IsMalformed, _Req1, State} =
                  chef_rest_search_resource:malformed_request(req_mock, make_state()),
              ?assertEqual(false, IsMalformed),
              ?assertEqual("testorg", State#state.organization_name),
              SolrQuery = State#state.solr_query,
              %% FIXME: this is a record defined in chef_solr
              ?assertEqual(#chef_solr_query{
                              query_string = "myquery",
                              filter_query = "+X_CHEF_type_CHEF_X:node",
                              start = 0, rows = 20,
                              sort = "X_CHEF_id_CHEF_X asc",
                              index = node},
                           SolrQuery),
              ?assert(meck:validate(wrq))
      end},

     {"q param defaults to '*:*'",
      fun() ->
              SignedHeadersBin = get_signed_headers(),
              meck:expect(wrq, get_req_header,
                          fun(HName, req_mock) -> proplists:get_value(HName, SignedHeadersBin) end),
              meck:expect(wrq, path_info, fun(object_type, req_mock) ->
                                                  "node";
                                             (organization_id, req_mock) ->
                                                  "testorg"
                                          end),
              meck:expect(wrq, get_qs_value, fun("q", req_mock) ->
                                                     undefined;
                                                ("start", req_mock) ->
                                                     "0";
                                                ("rows", req_mock) ->
                                                     "20";
                                                ("sort", req_mock) ->
                                                     "X_CHEF_id_CHEF_X+asc"
                                             end),
              {IsMalformed, _Req1, State} =
                  chef_rest_search_resource:malformed_request(req_mock, make_state()),
              ?assertEqual(false, IsMalformed),
              %% FIXME: this is a record defined in chef_solr
              SolrQuery = State#state.solr_query,
              ?assertEqual(#chef_solr_query{
                              query_string = "*:*",
                              filter_query = "+X_CHEF_type_CHEF_X:node",
                              start = 0, rows = 20,
                              sort = "X_CHEF_id_CHEF_X asc",
                              index = node}, SolrQuery),
              ?assert(meck:validate(wrq))
      end},

     {"bad query; unable to parse",
      fun() ->
              SignedHeadersBin = get_signed_headers(),
              meck:expect(wrq, get_req_header,
                          fun(HName, req_mock) -> proplists:get_value(HName, SignedHeadersBin) end),
              meck:expect(wrq, path_info, fun(object_type, req_mock) ->
                                                  "node";
                                             (organization_id, req_mock) ->
                                                  "testorg"
                                          end),
              meck:expect(wrq, get_qs_value, fun("q", req_mock) ->
                                                     "a[b";
                                                ("start", req_mock) ->
                                                     "0";
                                                ("rows", req_mock) ->
                                                     "20";
                                                ("sort", req_mock) ->
                                                     "X_CHEF_id_CHEF_X+asc"
                                             end),
              meck:expect(wrq, set_resp_body, fun(Body, req_mock) -> Body end),
              {IsMalformed, GotMsg, _State} =
                  chef_rest_search_resource:malformed_request(req_mock, make_state()),
              ErrorMsg = <<"{\"error\":[\"invalid search query: 'a[b'\"]}">>,
              ?assertEqual(true, IsMalformed),
              ?assertEqual(ErrorMsg, GotMsg),
              ?assert(meck:validate(wrq))
      end},

     {"bad start",
      fun() ->
              SignedHeadersBin = get_signed_headers(),
              meck:expect(wrq, get_req_header,
                          fun(HName, req_mock) -> proplists:get_value(HName, SignedHeadersBin) end),
              meck:expect(wrq, path_info, fun(object_type, req_mock) ->
                                                  "node";
                                             (organization_id, req_mock) ->
                                                  "testorg"
                                          end),
              meck:expect(wrq, get_qs_value, fun("q", req_mock) ->
                                                     "abc:123";
                                                ("start", req_mock) ->
                                                     "abc";
                                                ("rows", req_mock) ->
                                                     "20";
                                                ("sort", req_mock) ->
                                                     "X_CHEF_id_CHEF_X+asc"
                                             end),
              meck:expect(wrq, set_resp_body, fun(Body, req_mock) -> Body end),
              {IsMalformed, GotMsg, _State} =
                  chef_rest_search_resource:malformed_request(req_mock, make_state()),
              ErrorMsg = <<"{\"error\":[\"invalid 'start' value: 'abc'\"]}">>,
              ?assertEqual(true, IsMalformed),
              ?assertEqual(ErrorMsg, GotMsg),
              ?assert(meck:validate(wrq))
      end},

     {"bad rows",
      fun() ->
              SignedHeadersBin = get_signed_headers(),
              meck:expect(wrq, get_req_header,
                          fun(HName, req_mock) -> proplists:get_value(HName, SignedHeadersBin) end),
              meck:expect(wrq, path_info, fun(object_type, req_mock) ->
                                                  "node";
                                             (organization_id, req_mock) ->
                                                  "testorg"
                                          end),
              meck:expect(wrq, get_qs_value, fun("q", req_mock) ->
                                                     "abc:123";
                                                ("start", req_mock) ->
                                                     "0";
                                                ("rows", req_mock) ->
                                                     "-20";
                                                ("sort", req_mock) ->
                                                     "X_CHEF_id_CHEF_X+asc"
                                             end),
              meck:expect(wrq, set_resp_body, fun(Body, req_mock) -> Body end),
              {IsMalformed, GotMsg, _State} =
                  chef_rest_search_resource:malformed_request(req_mock, make_state()),
              ErrorMsg = <<"{\"error\":[\"invalid 'rows' value: '-20'\"]}">>,
              ?assertEqual(true, IsMalformed),
              ?assertEqual(ErrorMsg, GotMsg),
              ?assert(meck:validate(wrq))
      end},

     {"missing all auth headers",
      fun() ->
              meck:expect(wrq, get_req_header, fun(_HName, req_mock) -> undefined end),
              meck:expect(wrq, path_info, fun(organization_id, req_mock) ->
                                                  "testorg"
                                          end),
              meck:expect(wrq, set_resp_body, fun(Body, req_mock) -> Body end),
              {IsMalformed, GotMsg, _State} =
                  chef_rest_search_resource:malformed_request(req_mock, make_state()),
              ErrorMsg = list_to_binary("{\"error\":[\"missing required authentication header(s) "
                                        "'X-Ops-UserId', 'X-Ops-Timestamp', "
                                        "'X-Ops-Sign', 'X-Ops-Content-Hash'\"]}"),
              ?assertEqual({true, ErrorMsg}, {IsMalformed, GotMsg}),
              ?assert(meck:validate(wrq))
      end},

     {"missing X-Ops-Timestamp header",
      fun() ->
              HeaderFun = make_header_fun(),
              meck:expect(wrq, get_req_header,
                          fun("x-ops-timestamp", req_mock) ->
                                  undefined;
                             (HName, req_mock) ->
                                  HeaderFun(HName)
                          end),
              meck:expect(wrq, path_info, fun(object_type, req_mock) ->
                                                  "node";
                                             (organization_id, req_mock) ->
                                                  "testorg"
                                          end),
              meck:expect(wrq, get_qs_value, fun(_, req_mock) -> undefined end),
              meck:expect(wrq, set_resp_body, fun(Body, req_mock) -> Body end),
              {IsMalformed, GotMsg, _State} =
                  chef_rest_search_resource:malformed_request(req_mock, make_state()),
              ErrorMsg = list_to_binary("{\"error\":[\"missing required authentication header(s) "
                                        "'X-Ops-Timestamp'\"]}"),
              ?assertEqual({true, ErrorMsg}, {IsMalformed, GotMsg}),
              ?assert(meck:validate(wrq))
      end},


     {"time out of bounds",
      fun() ->
              SignedHeadersBin = get_signed_headers(),
              SignedHeadersBin1 = lists:keyreplace("x-ops-timestamp", 1, SignedHeadersBin,
                                                   {"x-ops-timestamp", <<"2011-06-21T19:06:35Z">>}),
              meck:expect(wrq, get_req_header,
                          fun(HName, req_mock) -> proplists:get_value(HName, SignedHeadersBin1) end),
              meck:expect(wrq, path_info, fun(object_type, req_mock) ->
                                                  "node";
                                             (organization_id, req_mock) ->
                                                  "testorg"
                                          end),
              meck:expect(wrq, get_qs_value, fun(_, req_mock) -> undefined end),
              meck:expect(wrq, set_resp_body, fun(Body, req_mock) -> Body end),
              {IsMalformed, GotMsg, _State} =
                  chef_rest_search_resource:malformed_request(req_mock, make_state()),
              ?assertEqual(true, IsMalformed),

              ErrorMsg = list_to_binary("{\"error\":[\"Failed to authenticate as alice."
                                        " Synchronize the clock on your host.\"]}"),
              ?assertEqual({true, ErrorMsg}, {IsMalformed, GotMsg}),
              ?assert(meck:validate(wrq))
      end}

    ]}.

is_authorized_test_() ->
    {setup,
     fun() ->
             application:start(crypto),
             set_env()
     end,
     fun(_) ->
             stopping
     end,
     is_authorized_tests()}.

is_authorized_tests() ->
    {foreach,
     fun() ->
             meck:new(wrq),
             meck:new(fast_log),
             meck:new(chef_otto),
             meck:expect(fast_log, info, fun(_, _, _) -> ok end),
             meck:expect(fast_log, info, fun(_, _, _, _) -> ok end)
     end,
     fun(_) ->
             meck:unload()
     end,
    [
     {"is user authorized YES",
      fun() ->
              HeaderFun = make_header_fun(),
              meck:expect(wrq, get_req_header,
                          fun(HName, req_mock) -> HeaderFun(HName) end),
              meck:expect(wrq, path_info, fun(object_type, req_mock) ->
                                                  "node";
                                             (organization_id, req_mock) ->
                                                  "testorg"
                                          end),
              meck:expect(wrq, set_resp_body, fun(Body, req_mock) -> Body end),
              meck:expect(chef_otto, connect, fun() -> mock_otto_connect end),
              meck:expect(chef_otto, fetch_user_or_client_cert,
                          fun(mock_otto_connect, "mock-org", "alice") ->
                                  {ok, Cert} = file:read_file("../test/acert.pem"),
                                  [{key_data, {cert, Cert}},
                                   {type, user}]
                          end),
              meck:expect(wrq, req_body, fun(req_mock) -> undefined end),
              meck:expect(wrq, method, fun(req_mock) -> 'GET' end),
              meck:expect(wrq, path, fun(req_mock) -> "does-not-matter-comes-from-meck-mocks" end),
              {IsAuth, _Req, State} =
                  chef_rest_search_resource:is_authorized(req_mock, make_state()),
              ?assertEqual(true, IsAuth),
              ?assertMatch(user, State#state.requester_type),
              ?assert(meck:validate(wrq)),
              ?assert(meck:validate(chef_otto))
      end},

     {"is user authorized no cert YES",
      fun() ->
              HeaderFun = make_header_fun(),
              meck:expect(wrq, get_req_header,
                          fun(HName, req_mock) -> HeaderFun(HName) end),
              meck:expect(wrq, path_info, fun(object_type, req_mock) ->
                                                  "node";
                                             (organization_id, req_mock) ->
                                                  "testorg"
                                          end),
              meck:expect(wrq, set_resp_body, fun(Body, req_mock) -> Body end),
              meck:expect(chef_otto, connect, fun() -> mock_otto_connect end),
              meck:expect(chef_otto, fetch_user_or_client_cert,
                          fun(mock_otto_connect, "mock-org", "alice") ->
                                  KeyPath = "../test/akey_public.pem",
                                  {ok, PubKey} = file:read_file(KeyPath),
                                  [{key_data, {key, PubKey}},
                                   {type, user}]
                          end),
              meck:expect(wrq, req_body, fun(req_mock) -> undefined end),
              meck:expect(wrq, method, fun(req_mock) -> 'GET' end),
              meck:expect(wrq, path, fun(req_mock) -> "does-not-matter-comes-from-meck-mocks" end),
              {IsAuth, _Req, State} =
                  chef_rest_search_resource:is_authorized(req_mock, make_state()),
              ?assertEqual(true, IsAuth),
              ?assertMatch(user, State#state.requester_type),
              ?assert(meck:validate(wrq)),
              ?assert(meck:validate(chef_otto))
      end},


     {"is client authorized YES",
      fun() ->
              HeaderFun = make_header_fun(),
              meck:expect(wrq, get_req_header,
                          fun(HName, req_mock) -> HeaderFun(HName) end),
              meck:expect(wrq, path_info, fun(object_type, req_mock) ->
                                                  "node";
                                             (organization_id, req_mock) ->
                                                  "testorg"
                                          end),
              meck:expect(wrq, set_resp_body, fun(Body, req_mock) -> Body end),
              meck:expect(chef_otto, connect, fun() -> mock_otto_connect end),
              meck:expect(chef_otto, fetch_user_or_client_cert,
                          fun(mock_otto_connect, "mock-org", "alice") ->
                                  {ok, Cert} = file:read_file("../test/acert.pem"),
                                  [{key_data, {cert, Cert}},
                                   {type, client}]
                          end),
              meck:expect(wrq, req_body, fun(req_mock) -> undefined end),
              meck:expect(wrq, method, fun(req_mock) -> 'GET' end),
              meck:expect(wrq, path, fun(req_mock) -> "does-not-matter-comes-from-meck-mocks" end),
              {IsAuth, _Req, State} =
                  chef_rest_search_resource:is_authorized(req_mock, make_state()),
              ?assertEqual(true, IsAuth),
              ?assertMatch(client, State#state.requester_type),
              ?assert(meck:validate(wrq)),
              ?assert(meck:validate(chef_otto))
      end},

     {"not authorized: key mismatch",
      fun() ->
              HeaderFun = make_header_fun(),
              meck:expect(wrq, get_req_header,
                          fun(HName, req_mock) -> HeaderFun(HName) end),
              meck:expect(wrq, path_info, fun(object_type, req_mock) ->
                                                  "node";
                                             (organization_id, req_mock) ->
                                                  "testorg"
                                          end),
              meck:expect(wrq, set_resp_body, fun(Body, req_mock) -> Body end),
              meck:expect(chef_otto, connect, fun() -> mock_otto_connect end),
              meck:expect(chef_otto, fetch_user_or_client_cert,
                          fun(mock_otto_connect, "mock-org", "alice") ->
                                  {ok, Cert} = file:read_file("../test/other_cert.pem"),
                                  [{key_data, {cert, Cert}}]
                          end),
              meck:expect(wrq, req_body, fun(req_mock) -> undefined end),
              meck:expect(wrq, method, fun(req_mock) -> 'GET' end),
              meck:expect(wrq, path, fun(req_mock) -> "does-not-matter-comes-from-meck-mocks" end),
              {IsAuth, Req, _State} =
                  chef_rest_search_resource:is_authorized(req_mock, make_state()),
              WantMsg = <<"{\"error\":[\"'alice' not authorized to search 'mock-org'.\"]}">>,
              ?assertEqual("X-Ops-Sign version=\"1.0\"", IsAuth),
              ?assertEqual(WantMsg, Req),
              ?assert(meck:validate(wrq)),
              ?assert(meck:validate(chef_otto))
      end},

     {"user/client not found",
      fun() ->
              HeaderFun = make_header_fun(),
              meck:expect(wrq, get_req_header,
                          fun(HName, req_mock) -> HeaderFun(HName) end),
              meck:expect(wrq, path_info, fun(object_type, req_mock) ->
                                                  "node";
                                             (organization_id, req_mock) ->
                                                  "testorg"
                                          end),
              meck:expect(wrq, set_resp_body, fun(Body, req_mock) -> Body end),
              meck:expect(chef_otto, connect, fun() -> mock_otto_connect end),
              meck:expect(chef_otto, fetch_user_or_client_cert,
                          fun(mock_otto_connect, "mock-org", "alice") ->
                                  {not_found, client}
                          end),
              meck:expect(wrq, req_body, fun(req_mock) -> undefined end),
              meck:expect(wrq, method, fun(req_mock) -> 'GET' end),
              meck:expect(wrq, path, fun(req_mock) -> "does-not-matter-comes-from-meck-mocks" end),
              {IsAuth, Req, _State} =
                  chef_rest_search_resource:is_authorized(req_mock, make_state()),
              WantMsg = iolist_to_binary(["{\"error\":[\"",
                                          "Failed to authenticate as '", "alice", "'. ",
                                          "Ensure that your node_name and client key ",
                                          "are correct.", "\"]}"]),
              ?assertEqual("X-Ops-Sign version=\"1.0\"", IsAuth),
              ?assertEqual(WantMsg, Req),
              ?assert(meck:validate(wrq)),
              ?assert(meck:validate(chef_otto))
      end},
     {"org not found",
      fun() ->
              HeaderFun = make_header_fun(),
              meck:expect(wrq, get_req_header,
                          fun(HName, req_mock) -> HeaderFun(HName) end),
              meck:expect(wrq, path_info, fun(object_type, req_mock) ->
                                                  "node";
                                             (organization_id, req_mock) ->
                                                  "testorg"
                                          end),
              meck:expect(wrq, set_resp_body, fun(Body, req_mock) -> Body end),
              meck:expect(chef_otto, connect, fun() -> mock_otto_connect end),
              meck:expect(chef_otto, fetch_user_or_client_cert,
                          fun(mock_otto_connect, "mock-org", "alice") ->
                                  {not_found, org}
                          end),
              meck:expect(wrq, req_body, fun(req_mock) -> undefined end),
              meck:expect(wrq, method, fun(req_mock) -> 'GET' end),
              meck:expect(wrq, path, fun(req_mock) -> "does-not-matter-comes-from-meck-mocks" end),
              {IsAuth, Req, _State} =
                  chef_rest_search_resource:is_authorized(req_mock, make_state()),
              WantMsg = <<"{\"error\":[\"organization 'mock-org' does not exist.\"]}">>,
              ?assertEqual("X-Ops-Sign version=\"1.0\"", IsAuth),
              ?assertEqual(WantMsg, Req),
              ?assert(meck:validate(wrq)),
              ?assert(meck:validate(chef_otto))
      end}
]}.


forbidden_test_() ->
    {setup,
     fun() ->
             application:start(crypto),
             set_env()
     end,
     fun(_) ->
             stopping
     end,
     forbidden_tests()}.

forbidden_tests() ->
    {foreach,
     fun() ->
             meck:new(wrq),
             meck:new(fast_log),
             meck:new(chef_otto),
             meck:expect(fast_log, info, fun(_, _, _) -> ok end),
             meck:expect(fast_log, info, fun(_, _, _, _) -> ok end)
     end,
     fun(_) ->
             meck:unload()
     end,
    [
     {"NOT forbidden user",
      fun() ->
              meck:expect(wrq, get_req_header,
                          fun("x-ops-userid", req_mock) -> "alice" end),
              meck:expect(chef_otto, connect, fun() -> mock_otto_connect end),
              meck:expect(chef_otto, is_user_in_org,
                          fun(mock_otto_connect, <<"alice">>, "mock-org") ->
                                  true
                          end),
              {IsForbidden, _Req, _State} =
                  chef_rest_search_resource:forbidden(req_mock, make_authorized_state(user)),
              ?assertEqual(false, IsForbidden),
              ?assert(meck:validate(wrq)),
              ?assert(meck:validate(chef_otto))
      end},

     {"NOT forbidden client",
      fun() ->
              meck:expect(wrq, get_req_header,
                          fun("x-ops-userid", req_mock) -> "alice" end),
              meck:expect(chef_otto, connect, fun() -> mock_otto_connect end),
              meck:expect(chef_otto, is_user_in_org,
                          fun(mock_otto_connect, <<"alice">>, "mock-org") ->
                                  true
                          end),
              {IsForbidden, _Req, _State} =
                  chef_rest_search_resource:forbidden(req_mock, make_authorized_state(client)),
              ?assertEqual(false, IsForbidden),
              ?assert(meck:validate(wrq)),
              ?assert(meck:validate(chef_otto))
      end},

     {"403 forbidden user",
      fun() ->
              meck:expect(wrq, get_req_header,
                          fun("x-ops-userid", req_mock) -> "alice" end),
              meck:expect(chef_otto, connect, fun() -> mock_otto_connect end),
              meck:expect(chef_otto, is_user_in_org,
                          fun(mock_otto_connect, <<"alice">>, "mock-org") ->
                                  false
                          end),
              meck:expect(wrq, set_resp_body, fun(Body, req_mock) -> Body end),
              {IsForbidden, Req, _State} =
                  chef_rest_search_resource:forbidden(req_mock, make_authorized_state(user)),
              WantMsg = <<"{\"error\":[\"'alice' not authorized to search 'mock-org'.\"]}">>,
              ?assertEqual(true, IsForbidden),
              ?assertEqual(WantMsg, Req),
              ?assert(meck:validate(wrq)),
              ?assert(meck:validate(chef_otto))
      end},
     {"Unexpected DB error",
      fun() ->
              meck:expect(wrq, get_req_header,
                          fun("x-ops-userid", req_mock) -> "alice" end),
              meck:expect(chef_otto, connect, fun() -> mock_otto_connect end),
              meck:expect(chef_otto, is_user_in_org,
                          fun(mock_otto_connect, <<"alice">>, "mock-org") ->
                                  {error, failed}
                          end),
              meck:expect(wrq, set_resp_body, fun(Body, req_mock) -> Body end),
              {IsForbidden, Req, _State} =
                  chef_rest_search_resource:forbidden(req_mock, make_state()),
              WantMsg = <<"{\"error\":[\"Failed to verify user 'alice' as a member of organization 'mock-org'.\"]}">>,
              ?assertEqual(true, IsForbidden),
              ?assertEqual(WantMsg, Req),
              ?assert(meck:validate(wrq)),
              ?assert(meck:validate(chef_otto))
      end}
    ]}.

resource_exists_test_() ->
    {foreach,
     fun() ->
             meck:new([wrq, chef_otto])
     end,
     fun(_) ->
             meck:unload()
     end,
     [
      {"org not found",
       fun() ->
               meck:expect(wrq, path_info,
                           fun(organization_id, req_mock) -> "mock-org" end),
               meck:expect(chef_otto, fetch_org_id,
                           fun(_, <<"mock-org">>) -> not_found end),
               meck:expect(wrq, set_resp_body,
                           fun(Msg, req_mock) -> Msg end),
               {Exists, Msg, _State} =
                   chef_rest_search_resource:resource_exists(req_mock,
                                                             make_state()),
               WantMsg = iolist_to_binary(["{\"error\":[\""
                                           "organization 'mock-org' does "
                                           "not exist.\"]}"]),
               ?assertEqual(false, Exists),
               ?assertEqual(WantMsg, Msg),
               ?assert(meck:validate(wrq)),
               ?assert(meck:validate(chef_otto))
       end
      },

      {"org guid already set",
       fun() ->
               Query = #chef_solr_query{query_string = "myquery",
                                        filter_query = "+X_CHEF_type_CHEF_X:node",
                                        start = 0, rows = 20,
                                        sort = "X_CHEF_id_CHEF_X asc",
                                        index = node},
               State0 = make_state(),
               State1 = State0#state{couchbeam = cb_mock,
                                    organization_guid = <<"123abc">>,
                                    solr_query = Query},
               meck:expect(wrq, path_info,
                           fun(organization_id, req_mock) -> "mock-org" end),
               {Exists, _Req, State2} =
                   chef_rest_search_resource:resource_exists(req_mock, State1),
               ?assertEqual(true, Exists),
               ?assertEqual("+X_CHEF_database_CHEF_X:chef_123abc "
                            "+X_CHEF_type_CHEF_X:node",
                            State2#state.solr_query#chef_solr_query.filter_query),
               ?assert(meck:validate(wrq)),
               ?assert(meck:validate(chef_otto))
       end
      }

     ]}.

to_json_test_() ->
    {setup,
     fun() ->
             application:start(crypto),
             set_env()
     end,
     fun(_) ->
             stopping
     end,
     json_test_generator(450)}.

mock_query_pipeline(Size) ->
    meck:expect(chef_solr, search, fun(_) ->
                                           Ids = lists:seq(1, Size),
                                           {ok, 1, Size, Ids} end).

json_test_generator(Count) ->
    {generator,
     fun() ->
             if Count > 0 ->
                     [json_test(Count) | json_test_generator(Count - 3)];
                true ->
                     []
             end
     end}.

json_test(Count) ->
    fun() ->
            meck:new([chef_otto, chef_solr]),
            meck:expect(chef_otto, bulk_get, fun(_, _, Ids) ->
                                                     R = [[{<<"_rev">>, Id},
                                                          {<<"id">>, Id}] || Id <- Ids],
                                                     case random:uniform() > 0.49 of
                                                         true ->
                                                             tl(R);
                                                         false ->
                                                             R
                                                     end end),
            mock_query_pipeline(Count),
            State = make_state(),
            State1 = State#state{solr_query=#chef_solr_query{index=node}, couchbeam=couchbeam_mock,
                                 batch_size=5, organization_guid=test},
            {Json, _Req, _State} = chef_rest_search_resource:to_json(req_mock, State1),
            try
                ejson:decode(Json)
            catch
                throw:_Why ->
                    ?debugFmt("INVALID JSON: ~p~n", [Json]),
                    erlang:halt(1)
            end,
            meck:unload() end.


get_signed_headers() ->
    {ok, KeyBin} = file:read_file("../test/akey.pem"),
    PrivateKey = chef_authn:extract_private_key(KeyBin),
    Time = httpd_util:rfc1123_date(),
    Path = "does-not-matter-comes-from-meck-mocks",
    Headers = chef_authn:sign_request(PrivateKey, <<"alice">>, <<"GET">>,
                                      Time, Path),
    [ {string:to_lower(binary_to_list(K)), binary_to_list(V)} || {K, V} <- Headers ].

make_header_fun() ->
    Headers = get_signed_headers(),
    fun(H) ->
            proplists:get_value(normalize_header_name(H), Headers)
    end.

normalize_header_name(H) when is_binary(H) ->
    normalize_header_name(binary_to_list(H));
normalize_header_name(H) ->
    string:to_lower(H).

