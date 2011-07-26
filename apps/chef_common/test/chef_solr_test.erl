-module(chef_solr_test).

-include_lib("eunit/include/eunit.hrl").

%% FIXME: copied from chef_solr
-record(chef_solr_query, {query_string :: string(),
                          filter_query :: string(),
                          start :: integer(),
                          rows :: integer(),
                          sort :: string()}).
%% /FIXME

expect_params(Params) ->
    meck:expect(wrq, get_qs_value, fun(Key, req_mock) ->
                                           proplists:get_value(Key, Params)
                                   end).

make_query_from_params_test_() ->
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
              meck:expect(wrq, path_info,
                          fun(object_type, req_mock) -> "node" end),
              Params = [{"q", "myquery"}, {"start", "2"}, {"rows", "5"}],
              expect_params(Params),
              Query = chef_solr:make_query_from_params(req_mock),
              Expect = #chef_solr_query{
                              query_string = "myquery",
                              filter_query = "+X_CHEF_type_CHEF_X:node",
                              sort = "X_CHEF_id_CHEF_X+asc",
                              start = 2,
                              rows = 5},
              ?assertEqual(Expect, Query),
              ?assert(meck:validate(wrq))
      end},

     {"default values",
      fun() ->
              meck:expect(wrq, path_info,
                          fun(object_type, req_mock) -> "node" end),
              expect_params([]),
              Query = chef_solr:make_query_from_params(req_mock),
              Expect = #chef_solr_query{
                              query_string = "*:*",
                              filter_query = "+X_CHEF_type_CHEF_X:node",
                              sort = "X_CHEF_id_CHEF_X+asc",
                              start = 0,
                              rows = 1000},
              ?assertEqual(Expect, Query),
              ?assert(meck:validate(wrq))
      end},

     {"bad query",
      fun() ->
              meck:expect(wrq, path_info,
                          fun(object_type, req_mock) -> "node" end),
              expect_params([{"q", "a[b"}]),
              ?assertThrow({bad_query, <<"a[b">>},
                           chef_solr:make_query_from_params(req_mock)),
              ?assert(meck:validate(wrq))
      end},

     {"bad start not integer",
      fun() ->
              meck:expect(wrq, path_info,
                          fun(object_type, req_mock) -> "node" end),
              expect_params([{"start", "abc"}]),
              ?assertThrow({bad_param, {"start", "abc"}},
                           chef_solr:make_query_from_params(req_mock)),
              ?assert(meck:validate(wrq))
      end},
     
     {"bad start negative",
      fun() ->
              meck:expect(wrq, path_info,
                          fun(object_type, req_mock) -> "node" end),
              expect_params([{"start", "-5"}]),
              ?assertThrow({bad_param, {"start", "-5"}},
                           chef_solr:make_query_from_params(req_mock)),
              ?assert(meck:validate(wrq))
      end},
     
     {"bad rows not integer",
      fun() ->
              meck:expect(wrq, path_info,
                          fun(object_type, req_mock) -> "node" end),
              expect_params([{"rows", "abc"}]),
              ?assertThrow({bad_param, {"rows", "abc"}},
                           chef_solr:make_query_from_params(req_mock)),
              ?assert(meck:validate(wrq))
      end},
     
     {"bad rows negative",
      fun() ->
              meck:expect(wrq, path_info,
                          fun(object_type, req_mock) -> "node" end),
              expect_params([{"rows", "-5"}]),
              ?assertThrow({bad_param, {"rows", "-5"}},
                           chef_solr:make_query_from_params(req_mock)),
              ?assert(meck:validate(wrq))
      end}
      
    ]}.

search_test_() ->
    {foreach,
     fun() ->
             meck:new(wrq),
             meck:new(ibrowse),
             application:set_env(chef_common, solr_url, "mock_solr_url")
     end,
     fun(_) ->
             meck:unload([wrq, ibrowse])
     end,
    [
     {"error if org filter not set",
      fun() ->
              meck:expect(ibrowse, send_req,
                          fun(Url, [], get) -> {ok, "200", [], Url} end),
              Query = #chef_solr_query{
                query_string = "*:*",
                %% note the missing org filter
                filter_query = "+X_CHEF_type_CHEF_X:node",
                sort = "X_CHEF_id_CHEF_X+asc",
                start = 0,
                rows = 1000},
              ?assertError(function_clause, chef_solr:search(Query)),
              ?assert(meck:validate(wrq)),
              %% ?assert(meck:validate(chef_solr)),
              ?assert(meck:validate(ibrowse))
      end}]}.
