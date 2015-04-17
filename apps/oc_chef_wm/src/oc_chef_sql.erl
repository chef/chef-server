%% ex: ts=4 sw=4 et
%% @author Seth Falcon <seth@chef.io>
%% @author Mark Anderson <mark@chef.io>
%% @author Christopher Maier <cm@chef.io>
%% @author James Casey <james@chef.io>
%% @author Mark Mzyk <mmzyk@chef.io>
%% @author Seth Chisamore <schisamo@chef.io>
%% @author Ho-Sheng Hsiao
%% Copyright 2011-2014 Chef Software, Inc. All Rights Reserved.
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

-module(oc_chef_sql).

-export([ statements/1 ]).

%% @doc Chef Server queries. Combines base statements
%% from chef_sql with oc_chef_authz_db:statements.
%%
%% Referenced by sys.config in {sqerl, [{ prepared_staements...
statements(_) ->
    chef_sql:statements() ++ oc_chef_authz_db:statements(pgsql).



