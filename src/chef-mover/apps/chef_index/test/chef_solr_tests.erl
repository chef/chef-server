%% Copyright 2012-2018 Chef Software, Inc.
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

-module(chef_solr_tests).

-include_lib("eunit/include/eunit.hrl").
-include("chef_solr.hrl").

%expect_params(Params) ->
    %meck:expect(wrq, get_qs_value, fun(Key, req_mock) ->
                                           %proplists:get_value(Key, Params)
                                   %end).

make_query_from_params_test_() ->
    {foreach,
     fun() ->
         ok
     end,
     fun(_) ->
         ok
     end,
    [
     {"properly formed",
      fun() ->
          Query = chef_solr:make_query_from_params("node", "myquery", "2", "5"),
          Expect = #chef_solr_query{
            query_string = "myquery",
            filter_query = "+X_CHEF_type_CHEF_X:node",
            sort = "X_CHEF_id_CHEF_X asc",
            start = 2,
            rows = 5,
            index = node},
          ?assertEqual(Expect, Query)
      end},

    {"default values",
      %% TODO: currently, a missing 'q' param is mapped to "*:*". We'd
      %% like to change that to be a 400 in the future.
      fun() ->
          Query = chef_solr:make_query_from_params("role", undefined, undefined, undefined),
          Expect = #chef_solr_query{
            query_string = "*:*",
            filter_query = "+X_CHEF_type_CHEF_X:role",
            sort = "X_CHEF_id_CHEF_X asc",
            start = 0,
            rows = 1000,
            index = role},
          ?assertEqual(Expect, Query)
      end},

    {"Present, but empty 'q' is a 400",
      fun() ->
          ?assertThrow({bad_query, ""},
            chef_solr:make_query_from_params("role", "", undefined, undefined))
      end},

    {"bad query",
      fun() ->
          ?assertThrow({bad_query, <<"a[b">>},
            chef_solr:make_query_from_params("node", "a[b", undefined, undefined))
      end},

    {"bad start not integer",
      fun() ->
          ?assertThrow({bad_param, {"start", "abc"}},
            chef_solr:make_query_from_params("node", undefined, "abc", undefined))
      end},

    {"bad start negative",
      fun() ->
          ?assertThrow({bad_param, {"start", "-5"}},
            chef_solr:make_query_from_params("node", undefined, "-5", undefined))
      end},

    {"bad rows not integer",
      fun() ->
          ?assertThrow({bad_param, {"rows", "abc"}},
            chef_solr:make_query_from_params("node", undefined, undefined, "abc"))
      end},

    {"bad rows negative",
      fun() ->
          ?assertThrow({bad_param, {"rows", "-5"}},
            chef_solr:make_query_from_params("node", undefined, undefined, "-5"))
      end},

     {"index type",
      fun() ->
              Tests = [{"node", node}, {"role", role}, {"client", client},
                       {"environment", environment},
                       {"adbag", {data_bag, <<"adbag">>}}],
              lists:foreach(
                fun({Sent, Want}) ->
                        Query = chef_solr:make_query_from_params(Sent, "query", "2", "5"),
                        ?assertEqual(Want, Query#chef_solr_query.index)
                end, Tests)
      end}

    ]}.

search_test_() ->
    {foreach,
     fun() ->
             meck:new(ibrowse),
             application:set_env(chef_index, solr_url, "mock_solr_url")
     end,
     fun(_) ->
             meck:unload([ibrowse])
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
              SolrJson = jiffy:encode(Solr),
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
