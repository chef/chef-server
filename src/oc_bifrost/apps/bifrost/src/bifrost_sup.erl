-module(bifrost_sup).

-behaviour(supervisor).

%% External exports
-export([
         start_link/0
        ]).

%% supervisor callbacks
-export([
         init/1
        ]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->

    Ip = envy:get(bifrost, ip, string),
    Port = envy:get(bifrost, port, integer),
    Ssl = envy:get(bifrost, ssl, false, boolean),
    SslOpts = lists:uniq([{verify, verify_none} | envy:get(bifrost, ssl_opts, [], list)]),
    {ok, Dispatch} = file:consult(filename:join([code:priv_dir(bifrost),
                                                 "dispatch.conf"])),

    WebConfig = [
                 {ip, Ip},
                 {port, Port},
                 {ssl, Ssl},
                 {ssl_opts, SslOpts},
                 {log_dir, "priv/log"},
                 {dispatch, add_dynamic_config(Dispatch)},
                 {nodelay, true} % TCP 'no delay' for latency
                ],

    Web = {webmachine_mochiweb,
           {webmachine_mochiweb, start, [WebConfig]},
           permanent, 5000, worker, dynamic},

    Processes = [Web],
    {ok, {{one_for_one, 10, 10}, Processes}}.


%% @doc We need to add some configuration to the resources that comes
%% from our sys.config file.  This is stuff that we can't include
%% directly in the dispatch.conf file, but that we don't want to look
%% up for each request.
%%
%% We'll append this information to the config that *is* in the
%% dispatch.conf file here.  We basically iterate through each
%% dispatch rule and append our additional config information.
add_dynamic_config(Dispatch) ->
    add_resource_init(Dispatch, dynamic_config(), []).

add_resource_init([Rule | Rest], AdditionalConfig, Acc) ->
    add_resource_init(Rest, AdditionalConfig, [add_init(Rule, AdditionalConfig) | Acc]);
add_resource_init([], _AdditionalConfig, Acc) ->
    lists:reverse(Acc).

add_init({Route, Guard, Module, Init}, AdditionalConfig) ->
    InitParams = Init ++ AdditionalConfig,
    {Route, Guard, Module, InitParams};
add_init({Route, Module, Init}, AdditionalConfig) ->
    InitParams = Init ++ AdditionalConfig,
    {Route, Module, InitParams}.

dynamic_config() ->
    superuser_config() ++ stats_hero_config().

superuser_config() ->
    {ok, SuperuserId} = chef_secrets:get(<<"oc_bifrost">>, <<"superuser_id">>),
    [{superuser_id, erlang:binary_to_list(SuperuserId)}].

stats_hero_config() ->
    {ok, MetricKey} = application:get_env(bifrost, root_metric_key),
    [{metrics_config, [
                       {root_metric_key, MetricKey},
                       {stats_hero_upstreams, bifrost_wm_base:stats_hero_upstreams()},
                       {stats_hero_label_fun, {bifrost_wm_base, stats_hero_label}}
                      ]}].
