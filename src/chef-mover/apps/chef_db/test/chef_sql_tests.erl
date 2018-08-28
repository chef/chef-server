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

-module(chef_sql_tests).

-include_lib("eunit/include/eunit.hrl").
-include("chef_types.hrl").

%% Used in testing flatten_record
-record(test_record, {'field1',
                      'field2'
                     }).

flatten_record_test_() ->
    [{"flatten record of simple record is ok",
        fun() ->
            R = #test_record{field1= <<"foo">>,
                         field2= <<"bar">> },
            Got = chef_sql:flatten_record(R),
            ?assertEqual([<<"foo">>, <<"bar">>], Got)
        end
      },
      {"throw on undefined as a default value",
        fun() ->
            R = #test_record{field1= <<"foo">>},
            ?assertError({undefined_in_record, R},
                         chef_sql:flatten_record(R))
        end
      },
      {"throw on explicit undefined used as a value",
        fun() ->
            R = #test_record{field1= <<"foo">>, field2= undefined},
            ?assertError({undefined_in_record, R},
                         chef_sql:flatten_record(R))
        end
      }
    ].

