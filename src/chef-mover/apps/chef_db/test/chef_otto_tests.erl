%% -*- erlang-indent-level: 4;indent-tabs-mode: nil; fill-column: 92-*-
%% ex: ts=4 sw=4 et
%% @author Christopher Maier <cm@chef.io>
%% Copyright 2012 Opscode, Inc. All Rights Reserved.
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

-module(chef_otto_tests).

-include_lib("eunit/include/eunit.hrl").
-include_lib("couchbeam/include/couchbeam.hrl").

%% @doc Given a list of EJson-encoded hashes, constructs a minimal
%% JSON binary string that can be used to mock a real CouchDB query
%% result.  It doesn't have all the details a real result would have,
%% but enough for what we use.
-spec make_fake_couchdb_results(list()) -> binary().
make_fake_couchdb_results(Items) when is_list(Items) ->
    %% The meat of a CouchDB document is stored under the "doc" key
    Wrapped = [{[{<<"doc">>, Item}]} || Item <- Items],
    %% Search results are available under a "rows" key
    jiffy:encode({[{<<"rows">>, Wrapped}]}).

%% Ensure that no non-tuple results from CouchDB "escape" into the
%% rest of the code through chef_otto:bulk_get/3.  The test mocks out
%% parts of the couchbeam infrastructure, but we're not interested in
%% testing CouchDB, but our response to what we get from CouchDB.
%%
%% Note that this is not intended to be a complete exercising of
%% chef_otto:bulk_get/3, just its behavior with respect to non-tuple
%% results that come back from CouchDB.
bulk_get_no_nulls_test_() ->
    MockedModules = [couchbeam],
    {
      foreachx,
      %% Setup
      fun(TestItems) ->
              JSON = make_fake_couchdb_results(TestItems),
              chef_db_test_utils:mock(MockedModules, [passthrough]),

              %% Mock with empty couchbeam server, view, and db
              %% records.  This is just to get us down to where
              %% couchbeam:db_request/6 runs.
              meck:expect(couchbeam, open_db, 3, {ok, #db{}}),
              meck:expect(couchbeam, all_docs, fun(Db, _Props) -> {ok, #view{db=Db}} end),

              meck:expect(couchbeam, db_request,
                          fun(_Method, _Url, _Expect, _Options, _Headers, _Body) ->
                                  %% status and headers are not
                                  %% important, and can be anything as
                                  %% far as chef_otto is concerned
                                  {ok, status, headers, JSON}
                          end)
      end,
      %% Teardown
      fun(_, _) ->
              chef_db_test_utils:unmock(MockedModules)
      end,
      [
       {Items,
        fun(_, _)->
                {Description,
                 fun() ->
                         %% In a sense, we're kind of "mocking"
                         %% chef_otto:bulk_get/3 here by using these
                         %% fake arguments, but they all have to do
                         %% with the connection to CouchDB and the
                         %% ultimate query that comes back.  They have
                         %% nothing to do with the processing of the
                         %% results, which is the true aim of these
                         %% tests
                         Actual = chef_otto:bulk_get(#server{}, <<"chef_deadbeefdeadbeefdeadbeefdeadbeef">>, []),
                         ?assertEqual(ExpectedResult, Actual)
                 end}
        end}

       || {Description, Items, ExpectedResult} <- [
                                             {"With no results, returns nothing",
                                              [], []},
                                             {"With all null results, returns nothing",
                                              [null, null, null], []},
                                             {"With real results, returns them all",
                                              [{[{<<"foo">>, <<"bar">>}]}],
                                              [{[{<<"foo">>, <<"bar">>}]}]},
                                             {"With real results and nulls, returns only non-nulls",
                                              [{[{<<"foo">>, <<"bar">>}]},
                                               null,
                                               {[{<<"baz">>, <<"quux">>}]},
                                               null],
                                              [{[{<<"foo">>, <<"bar">>}]},
                                               {[{<<"baz">>, <<"quux">>}]}]
                                             }
                                            ]
      ]
    }.
