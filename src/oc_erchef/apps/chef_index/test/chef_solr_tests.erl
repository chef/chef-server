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

search_test_() ->
    {foreach,
     fun() ->
             meck:new(chef_index_http)
     end,
     fun(_) ->
             meck:unload([chef_index_http])
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
              ?assertError({badmatch, _X}, chef_solr:search(Query))
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
              meck:expect(chef_index_http, request,
                          fun(_Url, get, []) -> {ok, "200", [], SolrJson} end),
              Query0 = #chef_solr_query{
                query_string = "*:*",
                filter_query = "+X_CHEF_type_CHEF_X:node",
                sort = "X_CHEF_id_CHEF_X asc",
                start = 0,
                rows = 1000},
              Query1 = chef_index:add_org_guid_to_query(Query0, <<"0123abc">>),
              ?assertEqual({ok, 2, 10, ["d1", "d2"]}, chef_solr:search(Query1)),
              ?assert(meck:validate(chef_index_http))
     end}
    ]}.
