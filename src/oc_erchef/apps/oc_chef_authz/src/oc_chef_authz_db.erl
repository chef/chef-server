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

-export([container_record_to_authz_id/2,
         fetch_container/3,
         make_read_access_group_name/1,
         fetch_read_access_group/2,
         fetch_group_authz_id/3,
         fetch_global_group_authz_id/3,
         fetch_group/3,
         make_context/3,
         statements/1
        ]).

-ifdef(TEST).
-compile([export_all]).
-endif.

-include("../../include/oc_chef_authz.hrl").
-include("oc_chef_authz_db.hrl").
-include_lib("sqerl/include/sqerl.hrl").

%% TODO Fix:
%% -include("../../include/chef_types.hrl").
%% can't include this because it also defines object_id
%% So copied this over for the short term.
-define(GLOBAL_PLACEHOLDER_ORG_ID, <<"00000000000000000000000000000000">>).

-define(gv(Key, PList), proplists:get_value(Key, PList)).
-define(user_db, "opscode_account").
-define(auth_join_db, "opscode_account").

statements(_) ->
    Path = filename:join([filename:dirname(code:which(?MODULE)), "..", "priv", "pgsql_statements.config"]),
    {ok, Statements} = file:consult(Path),
    Statements.
%
% Opscode Chef_views.
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
fetch_container(Ctx, undefined, ContainerName) ->
    fetch_container(Ctx, ?GLOBAL_PLACEHOLDER_ORG_ID, ContainerName);
fetch_container(#oc_chef_authz_context{reqid = ReqId}, OrgId, ContainerName) ->
    %% TODO can we move this into chef_sql to avoid having to directly sqerl??
    %% since ?FIRST uses record_info, it can't be placed within the fun.
    Transform = ?FIRST(chef_container),
    case stats_hero:ctime(ReqId,
                          %% use chef_sql name to aggregate perf timing with other sql queries
                          %% Couch removal note: keeping the name fetch_container_sql the same to avoid throwing off stats
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

%% TODO: the only global groups are global admins groups and this should only be used for those
fetch_global_group_authz_id(Ctx, OrgName, GroupName) ->
    RealGroupName = list_to_binary(lists:flatten(io_lib:format("~s_~s", [OrgName, GroupName]))),
    fetch_group_authz_id(Ctx, ?GLOBAL_PLACEHOLDER_ORG_ID, RealGroupName).

%% @doc Retrieve the authz ID for a given group in an organaization.
%%
%% This is not `fetch_group`, because currently, we have no need for
%% complete group "objects" in the system.  This is only really used
%% to retrieve the clients group, so that we may add newly-created
%% clients to it (we could also start deleting clients from it, too).
-spec fetch_group_authz_id(Context :: oc_chef_authz_context(),
                           OrgId :: binary() | undefined,
                           GroupName :: binary()) ->  object_id() |
                                                      {not_found, authz_group} |
                                                      {error, _}.
fetch_group_authz_id(#oc_chef_authz_context{server_api_version = ApiVersion, reqid = ReqId}, OrgId, Name) ->
    case stats_hero:ctime(ReqId, {chef_sql, fetch},
                          fun() ->
                                  chef_object_default_callbacks:fetch(#oc_chef_group{
                                                    server_api_version = ApiVersion,
                                                    org_id = OrgId,
                                                    name = Name},
                                                    fun chef_sql:select_rows/1)
                          end) of
        #oc_chef_group{authz_id = AuthzId} ->
            AuthzId;
        not_found ->
            {not_found, authz_group};
        {error, _} = Error ->
            Error
    end.


-spec container_record_to_authz_id(any(), any()) -> object_id().
container_record_to_authz_id(#oc_chef_authz_context{}, #chef_container{authz_id = Id}) ->
    Id.



%% TODO: refactor, clean this up
%% We need a clean api for fetching a group w/o expansion of members
%% We need a clean api for fetching the global admins group
%% Look
%%  * in oc_chef_wm_associations and invites
%%  * in oc_chef_authz_groups
%%  * in oc_chef_organization_policy
%%
fetch_read_access_group(#oc_chef_authz_context{} = Ctx, OrgName) ->
    ReadAccessGroupName = make_read_access_group_name(OrgName),
    case fetch_global_group(Ctx, ReadAccessGroupName) of
        {not_found, authz_group} ->
            %% For compatibility during provisioning, we return the global_admins group if
            %% we don't find the read_access group This should allow the application to be
            %% deployed before the migration is run.
            GlobalAdminsGroupName = make_global_admins_group_name(OrgName),
            fetch_global_group(Ctx, GlobalAdminsGroupName);
        Other ->
            Other
    end.

fetch_global_group(#oc_chef_authz_context{} = Ctx, GroupName) ->
    fetch_group(Ctx, ?GLOBAL_PLACEHOLDER_ORG_ID, GroupName).

make_read_access_group_name(OrgName) ->
  lists:flatten(io_lib:format("~s_read_access_group", [OrgName])).

make_global_admins_group_name(OrgName) ->
  lists:flatten(io_lib:format("~s_global_admins", [OrgName])).

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
