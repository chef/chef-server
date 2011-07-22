-module(search_resource_test).

-include_lib("eunit/include/eunit.hrl").

%% FIXME: this is copy/paste from the chef_rest_search_resource module
-record(state, {start_time,
                resource,
                organization_guid,
                organization_name,
                object_type,
                user_name,
                header_fun = undefined,
                couchbeam = undefined,
                solr_url = undefined,
                solr_query = undefined,
		org_guid = undefined,
                end_time,
                batch_size = 5
}).
%% /FIXME

malformed_request_test_() ->
    {setup,
     fun() ->
             application:start(crypto)
     end,
     fun(_) ->
             stopping
     end,
     malformed_request_tests()}.

malformed_request_tests() ->
    {foreach,
     fun() ->
             meck:new(wrq)
     end,
     fun(_) ->
             meck:unload(wrq)
     end,
    [
     {"properly formed",
      fun() ->
              SignedHeadersBin = get_signed_headers(),
              meck:expect(wrq, get_req_header,
                          fun(HName, req_mock) -> proplists:get_value(HName, SignedHeadersBin) end),
              meck:expect(wrq, path_info, fun(object_type, req_mock) -> "node" end),
              meck:expect(wrq, get_qs_value, fun("q", req_mock) ->
                                                     "myquery";
                                                ("start", req_mock) ->
                                                     "0";
                                                ("rows", req_mock) ->
                                                     "20";
                                                ("sort", req_mock) ->
                                                     "X_CHEF_id_CHEF_X+asc"
                                             end),
              {IsMalformed, _Req1, State} =
                  chef_rest_search_resource:malformed_request(req_mock, #state{}),
              ?assertEqual(false, IsMalformed),
              SolrQuery = State#state.solr_query,
              %% FIXME: this is a record defined in chef_solr
              ?assertEqual({chef_solr_query, "myquery", "+X_CHEF_type_CHEF_X:node",
                            0, 20, "X_CHEF_id_CHEF_X+asc"}, SolrQuery),
              ?assert(meck:validate(wrq))
      end},

     {"q param defaults to '*:*'",
      fun() ->
              SignedHeadersBin = get_signed_headers(),
              meck:expect(wrq, get_req_header,
                          fun(HName, req_mock) -> proplists:get_value(HName, SignedHeadersBin) end),
              meck:expect(wrq, path_info, fun(object_type, req_mock) -> "node" end),
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
                  chef_rest_search_resource:malformed_request(req_mock, #state{}),
              ?assertEqual(false, IsMalformed),
              %% FIXME: this is a record defined in chef_solr
              SolrQuery = State#state.solr_query,
              ?assertEqual({chef_solr_query, "*:*", "+X_CHEF_type_CHEF_X:node",
                            0, 20, "X_CHEF_id_CHEF_X+asc"}, SolrQuery),
              ?assert(meck:validate(wrq))
      end},

     {"bad query; unable to parse",
      fun() ->
              SignedHeadersBin = get_signed_headers(),
              meck:expect(wrq, get_req_header,
                          fun(HName, req_mock) -> proplists:get_value(HName, SignedHeadersBin) end),
              meck:expect(wrq, path_info, fun(object_type, req_mock) -> "node" end),
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
                  chef_rest_search_resource:malformed_request(req_mock, #state{}),
              ErrorMsg = <<"{\"error\":[\"invalid search query: 'a[b'\"]}">>,
              ?assertEqual(true, IsMalformed),
              ?assertEqual(ErrorMsg, GotMsg),
              ?assert(meck:validate(wrq))
      end},

     {"missing all auth headers",
      fun() ->
              meck:expect(wrq, get_req_header, fun(_HName, req_mock) -> undefined end),
              meck:expect(wrq, set_resp_body, fun(Body, req_mock) -> Body end),
              {IsMalformed, GotMsg, _State} =
                  chef_rest_search_resource:malformed_request(req_mock, #state{}),
              ErrorMsg = list_to_binary("{\"error\":[\"missing required authentication header(s) "
                                        "'X-Ops-UserId', 'X-Ops-Timestamp', "
                                        "'X-Ops-Sign', 'X-Ops-Content-Hash'\"]}"),
              ?assertEqual({true, ErrorMsg}, {IsMalformed, GotMsg}),
              ?assert(meck:validate(wrq))
      end},

     {"missing X-Ops-Timestamp header",
      fun() ->
              meck:expect(wrq, get_req_header,
                          fun(HName, req_mock) ->
                                  Headers = lists:keydelete(<<"X-Ops-Timestamp">>, 1,
                                                            all_auth_headers()),
                                  proplists:get_value(HName, Headers)
                          end),
              meck:expect(wrq, set_resp_body, fun(Body, req_mock) -> Body end),
              {IsMalformed, GotMsg, _State} =
                  chef_rest_search_resource:malformed_request(req_mock, #state{}),
              ErrorMsg = list_to_binary("{\"error\":[\"missing required authentication header(s) "
                                        "'X-Ops-Timestamp'\"]}"),
              ?assertEqual({true, ErrorMsg}, {IsMalformed, GotMsg}),
              ?assert(meck:validate(wrq))
      end},


     {"time out of bounds",
      fun() ->
              SignedHeadersBin = get_signed_headers(),
              SignedHeadersBin1 = lists:keyreplace(<<"X-Ops-Timestamp">>, 1, SignedHeadersBin,
                                                   {<<"X-Ops-Timestamp">>, <<"2011-06-21T19:06:35Z">>}),
              meck:expect(wrq, get_req_header,
                          fun(HName, req_mock) -> proplists:get_value(HName, SignedHeadersBin1) end),
              meck:expect(wrq, path_info, fun(object_type, req_mock) -> "node" end),
              meck:expect(wrq, get_qs_value, fun("q", req_mock) ->
                                                     "myquery";
                                                ("start", req_mock) ->
                                                     "0";
                                                ("rows", req_mock) ->
                                                     "20";
                                                ("sort", req_mock) ->
                                                     "X_CHEF_id_CHEF_X+asc"
                                             end),
              meck:expect(wrq, set_resp_body, fun(Body, req_mock) -> Body end),

              {IsMalformed, GotMsg, _State} =
                  chef_rest_search_resource:malformed_request(req_mock, #state{}),
              ?assertEqual(true, IsMalformed),

              ErrorMsg = list_to_binary("{\"error\":[\"Failed to authenticate as alice."
                                        " Synchronize the clock on your host.\"]}"),
              ?assertEqual({true, ErrorMsg}, {IsMalformed, GotMsg}),
              ?assert(meck:validate(wrq))
      end}

    ]}.

get_signed_headers() ->
    {ok, KeyBin} = file:read_file("../test/akey.pem"),
    PrivateKey = chef_authn:extract_private_key(KeyBin),
    Time = httpd_util:rfc1123_date(),
    Path = "does-not-matter-comes-from-meck-mocks",
    chef_authn:sign_request(PrivateKey, <<"alice">>, <<"GET">>, Time, Path).


all_auth_headers() ->
    [
     {<<"X-Ops-Content-Hash">>, "1"},
     {<<"X-Ops-UserId">>, <<"2">>},
     {<<"X-Ops-Sign">>, <<"3">>},
     {<<"X-Ops-Timestamp">>, <<"4">>},
     {<<"X-Ops-Authorization-1">>, <<"10">>},
     {<<"X-Ops-Authorization-2">>, <<"20">>},
     {<<"X-Ops-Authorization-3">>, <<"30">>},
     {<<"X-Ops-Authorization-4">>, <<"40">>},
     {<<"X-Ops-Authorization-5">>, <<"50">>},
     {<<"X-Ops-Authorization-6">>, <<"60">>}].

