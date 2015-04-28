%% -*- erlang-indent-level: 4;indent-tabs-mode: nil; fill-column: 92 -*-
%% ex: ts=4 sw=4 et
%% @author Mark Anderson <mark@opscode.com>
%% @author Marc Paradise <marc@getchef.com>
%% @doc authorization - Interface to the opscode authorization servize
%%
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
%%

-module(oc_chef_authz_db).

-include("../../include/oc_chef_types.hrl").
-include("../../include/server_api_version.hrl").

-export([fetch_container/3,
         make_global_admin_group_name/1,
         fetch_group/3,
         make_context/3,
         statements/1 ]).

%-ifdef(TEST).
-compile([export_all]).
%-endif.

-include("../../include/oc_chef_authz.hrl").
-include("oc_chef_authz_db.hrl").
-include_lib("sqerl/include/sqerl.hrl").

%% TODO Fix:
%% -include("../../include/chef_types.hrl").
%% can't include this because it also defines object_id
%% So copied this over for the short term.
-define(GLOBAL_PLACEHOLDER_ORG_ID, <<"00000000000000000000000000000000">>).

statements(_) ->
    Path = filename:join([filename:dirname(code:which(?MODULE)), "..", "priv", "pgsql_statements.config"]),
    {ok, Statements} = file:consult(Path),
    Statements.


-spec make_context(api_version(), binary(), term()) -> #oc_chef_authz_context{}.
make_context(ApiVersion, ReqId, Darklaunch) when is_binary(ReqId) ->
    #oc_chef_authz_context{server_api_version = ApiVersion,
                           reqid = ReqId,
                           darklaunch = Darklaunch}.

-spec fetch_container(oc_chef_authz_context(),
                      object_id() | undefined,
                      container_name()) -> #chef_container{} |
                                           not_found |
                                           {error, _}.
fetch_container(#oc_chef_authz_context{} = Ctx, undefined, ContainerName) ->
    fetch_container(Ctx, ?GLOBAL_PLACEHOLDER_ORG_ID, ContainerName);
fetch_container(#oc_chef_authz_context{reqid = ReqId}, OrgId, ContainerName) ->
    %% since ?FIRST uses record_info, it can't be placed within the fun.
    Transform = ?FIRST(chef_container),
    case stats_hero:ctime(ReqId,
                          %% aggregate perf timing with other sql queries
                          {chef_sql, fetch_container_sql},
                          fun() ->
                                  sqerl:select(find_container_by_orgid_name, [OrgId, ContainerName], Transform)
                          end) of
        {ok, #chef_container{} = C} ->
            C;
        {ok, none} ->
            not_found;
        {error, Error} ->
            {error, Error}
    end.

make_global_admin_group_name(OrgName) ->
  lists:flatten(io_lib:format("~s_global_admins", [OrgName])).

%% This provides a way to fetch a group from the DB without expanding it, which would happen
%% if we used chef_db:fetch.
fetch_global_admins(Ctx, OrgName) ->
    GlobalGroupName = make_global_admin_group_name(OrgName),
    fetch_group(Ctx, ?GLOBAL_PLACEHOLDER_ORG_ID, GlobalGroupName).

fetch_group(#oc_chef_authz_context{reqid = ReqId, server_api_version = ApiVersion}, OrgId, Name) ->
    case stats_hero:ctime(ReqId, {chef_sql, fetch},
                          fun() ->
                                  chef_object_default_callbacks:fetch(#oc_chef_group{
                                                                         server_api_version = ApiVersion,
                                                                         org_id = OrgId,
                                                                         name = Name},
                                                                      fun chef_sql:select_rows/1)
                          end) of
        #oc_chef_group{} = Group ->
            Group;
        not_found ->
            {not_found, authz_group};
        {error, _} = Error ->
            Error
    end.
