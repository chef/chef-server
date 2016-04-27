%% -*- erlang-indent-level: 4;indent-tabs-mode: nil; fill-column: 92-*-
%% ex: ts=4 sw=4 et
%% @author James Casey <james@chef.io>
%%
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


-type regex_name() :: cookbook_name |
                      data_bag_item_id |
                      data_bag_name |
                      environment_name |
                      client_name |
                      node_name |
                      qualified_recipe |
                      qualified_role |
                      recipe_name |
                      role_name |
                      unqualified_recipe |
                      user_name |
                      policy_file_name |
                      policy_identifier.

-type re_regex() :: re:mp(). %%{re_pattern, integer(), integer(), binary()}.
%% FIXME: This type is not yet correct
%% I'd like to use binary() but dialyzer wants this more specific type
-type re_msg() :: <<_:64,_:_*8>>.

-type regex_pattern() :: [1..255,...].


