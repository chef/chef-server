%% -*- erlang-indent-level: 4;indent-tabs-mode: nil; fill-column: 92 -*-
%% ex: ts=4 sw=4 et
%% @author Steven Danna <steve@chef.io>
%% @copyright 2015 Chef Software, Inc.
%%
%% The global_admins_rename callback renames ORGNAME_global_admins to
%% ORGNAME_read_access_group.
%%
%% The ORGNAME_global_admins group has a single purpose: providing
%% READ access to other user objects in ORGNAME. The rename in this
%% migrations makes the groups purpose more clear.
%%
-module(mover_global_admins_rename_callback).

-export([
         migration_init/0,
         migration_complete/0,
         migration_type/0,
         supervisor/0,
         migration_start_worker_args/2,
         migration_action/2,
         next_object/0,
         error_halts_migration/0,
         reconfigure_object/2,
         needs_account_dets/0
        ]).

-include("mover.hrl").
-record(group, {name,
                id,
                authz_id}).

migration_init() ->
    mover_transient_migration_queue:initialize_queue(?MODULE, all_global_admin_groups()).

migration_action(Group, _) ->
    rename_group(Group, new_groupname(Group)).

migration_complete() ->
    ok.

-spec all_global_admin_groups() -> [#group{}].
all_global_admin_groups() ->
    {ok, Groups} = sqerl:select(all_global_admin_groups_query(), [], rows_as_records, [group, record_info(fields, group)]),
    Groups.

-spec all_global_admin_groups_query() -> binary().
all_global_admin_groups_query() ->
    %%
    %% Selects all global admin groups by maching both on the global
    %% org_id and the name.
    %%
    <<"SELECT name, id, authz_id "
      "FROM groups "
      "WHERE org_id = '00000000000000000000000000000000'"
      "  AND name LIKE '%_global_admins'">>.

-spec new_groupname(#group{}) -> binary().
new_groupname(#group{name = GroupName}) ->
    re:replace(GroupName, "global_admins$", "read_access_group", [{return, binary}]).

-spec rename_group(#group{}, binary()) -> ok | {error, any()}.
rename_group(#group{id = GroupId} = Group, NewGroupName) ->
    case sqerl:execute(<<"UPDATE groups SET name = $1 WHERE id = $2">>, [NewGroupName, GroupId]) of
        {ok, 0} ->
            lager:warn("Group ~p not found, may have been deleted since the start of the migration", [Group#group.name]),
            ok;
        {ok, 1} ->
            ok;
        Error ->
            {error, Error}
    end.

%%
%% Generic mover callback functions for
%% a transient queue migration
%%
needs_account_dets() ->
    false.

migration_start_worker_args(Object, AcctInfo) ->
    [Object, AcctInfo].

next_object() ->
    mover_transient_migration_queue:next(?MODULE).

migration_type() ->
    <<"global_admins_rename">>.

supervisor() ->
    mover_transient_worker_sup.

error_halts_migration() ->
    false.

reconfigure_object(_ObjectId, _AcctInfo) ->
    ok.
