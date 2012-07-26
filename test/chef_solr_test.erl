-module(chef_solr_test).

-include_lib("eunit/include/eunit.hrl").
-include("../include/chef_solr.hrl").

-ifdef(FIXME).
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
                sort = "X_CHEF_id_CHEF_X asc",
                start = 2,
                rows = 5,
                index = node},
              ?assertEqual(Expect, Query),
              ?assert(meck:validate(wrq))
      end},

     {"default values",
      %% TODO: currently, a missing 'q' param is mapped to "*:*". We'd
      %% like to change that to be a 400 in the future.
      fun() ->
              meck:expect(wrq, path_info,
                          fun(object_type, req_mock) -> "role" end),
              expect_params([]),
              Query = chef_solr:make_query_from_params(req_mock),
              Expect = #chef_solr_query{
                              query_string = "*:*",
                              filter_query = "+X_CHEF_type_CHEF_X:role",
                              sort = "X_CHEF_id_CHEF_X asc",
                              start = 0,
                              rows = 1000,
                              index = role},
              ?assertEqual(Expect, Query),
              ?assert(meck:validate(wrq))
      end},

     {"Present, but empty 'q' is a 400",
      fun() ->
              meck:expect(wrq, path_info, fun(object_type, req_mock) -> "role" end),
              expect_params([{"q", ""}]),
              ?assertThrow({bad_query, ""}, chef_solr:make_query_from_params(req_mock)),
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
      end},

     {"index type",
      fun() ->
              Tests = [{"node", node}, {"role", role}, {"client", client},
                       {"environment", environment},
                       {"adbag", {data_bag, <<"adbag">>}}],
              Types = [ T || {T, _} <- Tests],
              meck:sequence(wrq, path_info, 2, Types),
              expect_params([]),                % defaults
              lists:foreach(
                fun({_, Want}) ->
                        Query = chef_solr:make_query_from_params(req_mock),
                        ?assertEqual(Want, Query#chef_solr_query.index)
                end, Tests),
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
              Query = #chef_solr_query{
                query_string = "*:*",
                %% note the missing org filter
                filter_query = "+X_CHEF_type_CHEF_X:node",
                sort = "X_CHEF_id_CHEF_X asc",
                start = 0,
                rows = 1000},
              ?assertError(function_clause, chef_solr:search(Query))
      end},

     {"parse non-empty solr result",
      fun() ->
              Docs = [{[{<<"X_CHEF_id_CHEF_X">>, "d1"}]},
                      {[{<<"X_CHEF_id_CHEF_X">>, "d2"}]}],
              Solr = {[{<<"response">>,
                        {[{<<"start">>, 2},
                          {<<"numFound">>, 10},
                          {<<"docs">>, Docs}]}}]},
              SolrJson = ejson:encode(Solr),
              meck:expect(ibrowse, send_req,
                          fun(_Url, [], get) -> {ok, "200", [], SolrJson} end),
              Query0 = #chef_solr_query{
                query_string = "*:*",
                filter_query = "+X_CHEF_type_CHEF_X:node",
                sort = "X_CHEF_id_CHEF_X asc",
                start = 0,
                rows = 1000},
              Query1 = chef_solr:add_org_guid_to_query(Query0, <<"0123abc">>),
              ?assertEqual({ok, 2, 10, ["d1", "d2"]}, chef_solr:search(Query1)),
              ?assert(meck:validate(ibrowse))
     end}
    ]}.

add_org_guid_to_query_test() ->
    Query0 = #chef_solr_query{
      query_string = "*:*",
      filter_query = "+X_CHEF_type_CHEF_X:role",
      sort = "X_CHEF_id_CHEF_X asc",
      start = 0,
      rows = 1000,
      index = role},
    Query1 = chef_solr:add_org_guid_to_query(Query0, <<"0123abc">>),
    ?assertEqual("+X_CHEF_database_CHEF_X:chef_0123abc "
                 "+X_CHEF_type_CHEF_X:role",
                 Query1#chef_solr_query.filter_query).
-endif().
