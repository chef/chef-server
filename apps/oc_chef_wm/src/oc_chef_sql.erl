%% ex: ts=4 sw=4 et
%% @author Seth Falcon <seth@opscode.com>
%% @author Mark Anderson <mark@opscode.com>
%% @author Christopher Maier <cm@opscode.com>
%% @author James Casey <james@opscode.com>
%% @author Mark Mzyk <mmzyk@opscode.com>
%% @author Seth Chisamore <schisamo@opscode.com>
%% @author Ho-Sheng Hsiao <hosh@opscode.com>
%% Copyright 2011-2013 Opscode, Inc. All Rights Reserved.
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

-export([
         statements/1
     ]).

%% @doc Private Chef Server queries. Merges base statements with OPC statements
%% This is the function you for setting up sqerl in oc_erchef.config
statements(DbType) ->
    BaseStatements = statements(DbType, default),
    OPCStatements = statements(DbType, opc),

    %% Queries from the OPC statements file will override any
    %% identically-named queries used on the Open Source server
    Merged = dict:merge(fun(_K, _V1, V2) -> V2 end,
                        dict:from_list(BaseStatements),
                        dict:from_list(OPCStatements)),
    %% count_user_admins is not used in EC
    ChefStatements = dict:to_list(lists:foldl(fun dict:erase/2,
                                              Merged,
                                              [count_user_admins])),
    AuthzStatements = oc_chef_authz_db:statements(pgsql),
    AuthzStatements ++ ChefStatements.

%% @doc Return a proplist of the parameterized SQL queries needed for
%% chef_sql.  `Class' is used to distinguish between OPC and OSC
statements(pgsql, default) ->
    chef_sql:statements();
statements(DbType, Class) when Class =:= default;
                               Class =:= opc ->
    Suffix = case Class of
                 default ->
                     "_statements.config";
                 opc ->
                     "_statements_opc.config"
             end,
    File = atom_to_list(DbType) ++ Suffix,
    Path = filename:join([filename:dirname(code:which(?MODULE)), "..", "priv", File]),
    {ok, Statements} = file:consult(Path),
    Statements.
