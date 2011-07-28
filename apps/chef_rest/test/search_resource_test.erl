-module(search_resource_test).

-include_lib("eunit/include/eunit.hrl").
-include("../src/chef_rest_search_resource.hrl").

make_state() ->
    #state{start_time = now(),
           resource = "chef_rest_search_resource",
           batch_size = 5,
           estatsd_server = "127.0.0.1",
           estatsd_port = 3365,
           hostname = net_adm:localhost(),
           organization_name = "mock-org",
           request_type = "search.get"}.
%% /FIXME

malformed_request_test_() ->
    {setup,
     fun() ->
             application:start(crypto),
             application:set_env(chef_rest, reqid_header_name, "X-Request-Id")
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
             meck:expect(fast_log, info, fun(_, _, _, _) -> ok end),
             application:set_env(chef_rest, reqid_header_name, "X-Request-Id")
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
              ?assertEqual({chef_solr_query, "myquery", "+X_CHEF_type_CHEF_X:node",
                            0, 20, "X_CHEF_id_CHEF_X asc"}, SolrQuery),
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
              ?assertEqual({chef_solr_query, "*:*", "+X_CHEF_type_CHEF_X:node",
                            0, 20, "X_CHEF_id_CHEF_X asc"}, SolrQuery),
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
             application:set_env(chef_rest, reqid_header_name, "X-Request-Id")
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
             meck:expect(fast_log, info, fun(_, _, _, _) -> ok end),
             application:set_env(chef_rest, reqid_header_name, "X-Request-Id")
     end,
     fun(_) ->
             meck:unload()
     end,
    [
     {"is authorized YES",
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
                                  [{cert, Cert}]
                          end),
              meck:expect(wrq, req_body, fun(req_mock) -> undefined end),
              meck:expect(wrq, method, fun(req_mock) -> 'GET' end),
              meck:expect(wrq, path, fun(req_mock) -> "does-not-matter-comes-from-meck-mocks" end),
              {IsAuth, _Req, _State} =
                  chef_rest_search_resource:is_authorized(req_mock, make_state()),
              ?assertEqual(true, IsAuth),
              ?assert(meck:validate(wrq)),
              ?assert(meck:validate(chef_otto))
      end}]}.


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

