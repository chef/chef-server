%% Copyright 2015 Chef Server, Inc. All Rights Reserved.
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

-module(chef_lucene_tests).
-include_lib("eunit/include/eunit.hrl").

% Note: chef_index_test_utils:set_provider sets the application env and a process dictionary
% entry to specify the provider module. This must be done within each test, because
% setup/teardown functions do not seem to execute within the same process as the test.

chef_lucene_solr_test_() ->
     [
      {"it transforms an arbitrary field search into a search on the content field",
       fun() ->
               chef_index_test_utils:set_provider(solr),
               io:fwrite("*** search_module: ~p~n", [get(search_module)]),
               ?assertEqual(chef_lucene:parse(<<"foo:bar">>), <<"content:foo__=__bar">>)
       end}
     ].
