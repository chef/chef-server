-module(mover_status).

-export([org_by_name/1,
         migration_time/1,
         summarize_orgs/0,
         summarize_time/0,
         read_only_orgs/0,
         active_orgs/0,
         error_orgs/0,
         error_nodes/0,
         reset_orgs/1]).

-include("mover.hrl").

org_by_name(Name) ->
     Spec = #org{guid = '_',
                 name = Name,
                 preloaded = '_',
                 read_only = '_',
                 active = '_',
                 migrated = '_',
                 worker = '_',
                 time = '_'},
    ?fix_table(all_orgs, dets:match_object(all_orgs, Spec)).

reset_orgs(Orgs) ->
    [ dets:insert(all_orgs, Org#org{active = false,
                                    migrated = false,
                                    worker = undefined})
      || Org <- Orgs].

migration_time(#org{}=Org) ->
    [{total, time_diff(Org#org.time, start, stop)},
     {nodes, time_diff(Org#org.time, start, nodes_done)}];
migration_time(Name) when is_binary(Name) ->
     Spec = #org{guid = '_',
                 name = Name,
                 preloaded = '_',
                 read_only = '_',
                 active = '_',
                 migrated = '_',
                 worker = '_', 
                 time = '_'},
    case ?fix_table(all_orgs, dets:match_object(all_orgs, Spec)) of
        [Org] ->
            migration_time(Org);
        [] -> not_found;
        Error -> Error
    end.

read_only_orgs() ->
     Spec = #org{guid = '_',
                 name = '_',
                 preloaded = '_',
                 read_only = true,
                 active = '_',
                 migrated = '_',
                 worker = '_', 
                 time = '_'},
    ?fix_table(all_orgs, dets:match_object(all_orgs, Spec)).

active_orgs() ->
     Spec = #org{guid = '_',
                 name = '_',
                 preloaded = '_',
                 read_only = '_',
                 active = true,
                 migrated = '_',
                 worker = '_', 
                 time = '_'},
    ?fix_table(all_orgs, dets:match_object(all_orgs, Spec)).

error_orgs() ->
     Spec = #org{guid = '_',
                 name = '_',
                 preloaded = '_',
                 read_only = '_',
                 active = '_',
                 migrated = nodes_failed,
                 worker = '_', 
                 time = '_'},
    ?fix_table(all_orgs, dets:match_object(all_orgs, Spec)).

error_nodes() ->
    ?fix_table(error_nodes, dets:foldl(fun(E,A) -> [E|A] end, [], error_nodes)).

summarize_time() ->
    {Sum, Max, N} =
        ?fix_table(all_orgs,
                   dets:foldl(
                     fun(Org, {Sum, Max, N}) ->
                             OrgTime = time_diff(Org#org.time, start, stop),
                             case OrgTime of
                                 undefined ->
                                     {Sum, Max, N};
                                 _IsMax when OrgTime > Max ->
                                     {add_time(Sum, OrgTime), OrgTime, N + 1};
                                 _NotMax ->
                                     {add_time(Sum, OrgTime), Max, N + 1}
                             end
                     end, {0, 0, 0}, all_orgs)),
    [{avg, avg_time(Sum, N)}, {max, Max}, {count, N}].

add_time(Sum, {_H, _M, _S}=Time) when is_integer(Sum) ->
    Sum + calendar:time_to_seconds(Time).

avg_time(Sum, N) ->
    calendar:seconds_to_time(Sum div N).

time_diff(Times, Tag1, Tag2) ->
    T1 = proplists:get_value(Tag1, Times),
    T2 = proplists:get_value(Tag2, Times),
    case {T1, T2} of
        {U1, U2} when U1 =:= undefined orelse U2 =:= undefined ->
            undefined;
        _Found ->
            D1 = calendar:now_to_universal_time(T1),
            D2 = calendar:now_to_universal_time(T2),
            {_, Ans} = calendar:time_difference(D1, D2),
            Ans
    end.

summarize_orgs() ->
    Counts = ?fix_table(all_orgs,
                        dets:foldl(
                          fun(Org, {NTotal, NPreloaded, NReadOnly,
                                    NActive, NMigrated, NError}) ->
                                  {NTotal + 1,
                                   NPreloaded + preloaded_count(Org),
                                   NReadOnly + as_number(Org#org.read_only),
                                   NActive + as_number(Org#org.active),
                                   NMigrated + as_number(Org#org.migrated),
                                   NError + error_count(Org)}
                          end, {0, 0, 0, 0, 0, 0}, all_orgs)),
    Labels = [total, preloaded, read_only, active, migrated, error],
    lists:zip(Labels, tuple_to_list(Counts)).

preloaded_count(#org{preloaded=true, migrated = false}) ->
    1;
preloaded_count(#org{}) ->
    0.

error_count(#org{migrated = nodes_failed}) ->
    1;
error_count(#org{}) ->
    0.

as_number(true) ->
    1;
as_number(_) ->
    0.
