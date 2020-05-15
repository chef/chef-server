-module(chef_wm_prometheus_json_format).

-export([
         content_type/0,
         format/0,
         format/1]).

-include_lib("prometheus/include/prometheus_model.hrl").

-define(PROCESS_DICT_STORAGE, chef_wm_prometheus_json_format_data).

-spec content_type() -> binary().
content_type() ->
    <<"application/json">>.

-spec format() -> binary().
format() ->
    format(default).

-spec format(Registry :: prometheus_registry:registry()) -> binary().
format(Registry) ->
    put(?PROCESS_DICT_STORAGE, []),
    Callback = fun (_, Collector) ->
                       registry_collect_callback(Registry, Collector)
               end,
    prometheus_registry:collect(Registry, Callback),
    case get(?PROCESS_DICT_STORAGE) of
        undefined ->
            <<"{}">>;
        Data ->
            %TODO(jaym) 08/27/17: It might be worthwhile making this defensive to failure... always cleanup
            erase(?PROCESS_DICT_STORAGE),
            jiffy:encode(Data)
    end.

registry_collect_callback(Registry, Collector) ->
  Callback = fun (MF) ->
                 Data = get(?PROCESS_DICT_STORAGE),
                 put(?PROCESS_DICT_STORAGE, [mf_to_erl(MF) | Data])
             end,
  prometheus_collector:collect_mf(Registry, Collector, Callback).

%% @private
mf_to_erl(#'MetricFamily'{name = Name, help = Help, type = Type, metric = Metrics}) ->
    {[
      {name, iolist_to_binary(Name)},
      {type, string_type(Type)},
      {help, list_to_binary(Help)},
      {metrics, [metric_to_erl(Metric) || Metric <- Metrics]}
     ]}.


metric_to_erl(#'Metric'{label=Labels} = Metric) ->
    {maybe_prepend_labels(Labels, emit_metric(Metric))}.

emit_metric(#'Metric'{counter=#'Counter'{value=Value}}) ->
    [
     {value, as_binary(Value)}
    ];
emit_metric(#'Metric'{gauge=#'Gauge'{value=Value}}) ->
    [
     {value, as_binary(Value)}
    ];
%as a bonus what is the json representation of a histogram?
emit_metric(#'Metric'{histogram=#'Histogram'{
                                   sample_sum = SampleSum,
                                   sample_count = SampleCount,
                                   bucket = Buckets}}) ->
    [
     {sample_sum, as_binary(SampleSum)},
     {sample_count, as_binary(SampleCount)},
     {bucket, emit_histogram_buckets(Buckets)}
    ];
emit_metric(#'Metric'{untyped=#'Untyped'{value=Value}}) ->
    [
     {value, as_binary(Value)}
    ].

emit_histogram_buckets(Buckets) ->
    Fun = fun (#'Bucket'{cumulative_count = CumulativeCount,
                         upper_bound = UpperBound}) ->
                  {[{cumulative_count, as_binary(CumulativeCount)},
                    {upper_bound, as_binary(UpperBound)}]}
          end,
    lists:map(Fun, Buckets).

%% TODO(jaym) 08/27/17: Summary types

maybe_prepend_labels([], EJson) ->
    EJson;
maybe_prepend_labels(Labels, EJson) ->
    [{labels, labels(Labels)} | EJson].

labels(Labels) ->
    Fun = fun (#'LabelPair'{name=Name, value=Value}) ->
                  {as_binary(Name), as_binary(Value)}
          end,
    {lists:map(Fun, Labels)}.

as_binary(infinity) ->
    <<"+Inf">>;
as_binary('-infinity') ->
    <<"-Inf">>;
as_binary(Value) when is_binary(Value) ->
    Value;
as_binary(Value) when is_list(Value) ->
    list_to_binary(Value);
as_binary(Value) when is_float(Value) ->
    list_to_binary(io_lib:format("~p", [Value]));
as_binary(Value) when is_integer(Value) ->
    integer_to_binary(Value).

string_type('COUNTER') ->
    <<"COUNTER">>;
string_type('GAUGE') ->
    <<"GAUGE">>;
string_type('HISTOGRAM') ->
    <<"HISTOGRAM">>;
string_type('UNTYPED') ->
    <<"UNTYPED">>.
