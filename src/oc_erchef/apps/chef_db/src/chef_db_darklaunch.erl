%% -*- erlang-indent-level: 4;indent-tabs-mode: nil; fill-column: 92-*-
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
-module(chef_db_darklaunch).

-export([is_enabled/2]).


%% The darklaunch module used by chef_db can be set using this
%% define. The default included here ignores `OrgName' and answers
%% false to all couchdb_* features and true otherwise.
-ifndef(CHEF_DB_DARKLAUNCH).
is_enabled(<<"couchdb_", _Rest/binary>>, _) ->
    false;
is_enabled(_, _) ->
    true.
-else.
is_enabled(Feature, Darklaunch) ->
    ?CHEF_DB_DARKLAUNCH:is_enabled(Feature, Darklaunch).
-endif.




