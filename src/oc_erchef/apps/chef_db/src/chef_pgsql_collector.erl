-module(chef_pgsql_collector).

-export([deregister_cleanup/1,
         collect_mf/2,
         collect_metrics/2]).

-import(prometheus_model_helpers, [create_mf/5,
                                   label_pairs/1,
                                   gauge_metrics/1,
                                   gauge_metric/1,
                                   gauge_metric/2,
                                   counter_metric/1,
                                   counter_metric/2]).

-behaviour(prometheus_collector).

-define(METRICS, [
                  {<<"seq_scan">>, {pg_stat_seq_scan, counter, "Number of sequential scans initiated on all tables", fun erlang:binary_to_integer/1}},
                  {<<"seq_tup_read">>, {pg_stat_seq_tup_read, counter, "Number of live rows fetched by sequential scans on all tables", fun erlang:binary_to_integer/1}},
                  {<<"idx_scan">>, {pg_stat_idx_scan, counter, "Number of index scans initiated on all tables", fun erlang:binary_to_integer/1}},
                  {<<"idx_tup_fetch">>, {pg_stat_tup_fetch, counter, "Number of live rows fetched by index scans on all tables", fun erlang:binary_to_integer/1}},
                  {<<"n_tup_ins">>, {pg_stat_n_tup_ins, counter, "Number of rows inserted on all tables", fun erlang:binary_to_integer/1}},
                  {<<"n_tup_upd">>, {pg_stat_n_tup_upd, counter, "Number of rows updated on all tables", fun erlang:binary_to_integer/1}},
                  {<<"n_tup_del">>, {pg_stat_n_tup_del, counter, "Number of rows deleted on all tables", fun erlang:binary_to_integer/1}},
                  {<<"n_live_tup">>, {pg_stat_n_live_tup, gauge, "Estimated number of live rows on all tables", fun erlang:binary_to_integer/1}},
                  {<<"n_dead_tup">>, {pg_stat_n_dead_tup, gauge, "Estimated number of dead rows on all tables", fun erlang:binary_to_integer/1}},
                  {<<"heap_blks_read">>, {pg_stat_heap_blocks_read, counter, "Number of disk blocks read on all tables", fun erlang:binary_to_integer/1}},
                  {<<"heap_blks_hit">>, {pg_stat_heap_blocks_hit, counter, "Number of buffer hits on all tables", fun erlang:binary_to_integer/1}},
                  {<<"idx_blks_read">>, {pg_stat_idx_blks_read, counter, "Number of disk blocks read from all indexes", fun erlang:binary_to_integer/1}},
                  {<<"idx_blks_hit">>, {pg_stat_idx_blks_hit, counter, "Number of buffer hits in all indexes", fun erlang:binary_to_integer/1}},
                  {<<"toast_blks_read">>, {pg_stat_toast_blks_read, counter, "Number of disk blocks read from TOAST tables", fun erlang:binary_to_integer/1}},
                  {<<"toast_blks_hit">>, {pg_stat_toast_blks_hit, counter, "Number of buffer hits in TOAST tables", fun erlang:binary_to_integer/1}},
                  {<<"tidx_blks_read">>, {pg_stat_tidx_blks_read, counter, "Number of disk blocks read from TOAST tables", fun erlang:binary_to_integer/1}},
                  {<<"tidx_blks_hit">>, {pg_stat_tidx_blks_hit, counter, "Number of buffer hits in TOAST table indexes", fun erlang:binary_to_integer/1}},
                  % I have no idea why only these two columns come back as numbers instead of as a binary...
                  {<<"n_active_conns">>, {pg_stat_n_active_conns, gauge, "Number of active connections to the database", fun(X) -> X end}},
                  {<<"n_conns">>, {pg_stat_n_conns, gauge, "Number of all connections to the database", fun(X) -> X end}}
                 ]).

%%====================================================================
%% Collector API
%%====================================================================

deregister_cleanup(_) -> ok.


collect_mf(_Registry, Callback) ->

    Stats = stats(),

    lists:foreach(fun({ColName, {StatName, StatType, StatHelp, Transform}}) ->
                          case proplists:get_value(ColName, Stats) of
                              undefined ->
                                  ok;
                              Value ->
                                  Callback(create_metric(StatName,
                                                         StatHelp,
                                                         StatType,
                                                         {StatType, Transform(Value)}))
                          end
                  end, ?METRICS),
    ok.

collect_metrics(_StatName, {counter, Value}) ->
    counter_metric(Value);
collect_metrics(_StatName, {gauge, Value}) ->
    gauge_metric(Value).


create_metric(Name, Help, Type, Data) ->
    create_mf(Name, Help, Type, ?MODULE, Data).


%%====================================================================
%% Private functions
%%====================================================================

-spec stats() -> [{binary(), binary()}].
stats() ->
    try
        case sqerl:select(stats, [], first, []) of
            {ok, Stats} -> Stats;
            _Else -> throw(error)
        end
    catch
        How:Why ->
            error_logger:error_report({chef_sql, stats, How, Why}),
            %% We don't want to propagate errors here. Doing so would mean
            %% an error would prevent us from getting any metrics.
            []
    end.

