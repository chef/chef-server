%% -*- erlang-indent-level: 4;indent-tabs-mode: nil; fill-column: 92 -*-
%% ex: ts=4 sw=4 et
%% @author Kevin Smith
%% @copyright 2011-2014 Chef Software Inc.

-module(oc_chef_wm_sup).

-behaviour(supervisor).

-include_lib("amqp_client/include/amqp_client.hrl").

%% External exports
-export([start_link/0, upgrade/0]).

%% supervisor callbacks
-export([init/1]).

-include("oc_chef_wm.hrl").

-define(QUEUE_MONITOR_SETTING(Key, Default), chef_wm_rabbitmq_management:get_rabbit_queue_monitor_setting(Key, Default)).

%% @spec start_link() -> ServerRet
%% @doc API for starting the supervisor.
start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% @spec upgrade() -> ok
%% @doc Add processes if necessary.
upgrade() ->
    {ok, {_, Specs}} = init([]),

    Old = sets:from_list(
            [Name || {Name, _, _, _} <- supervisor:which_children(?MODULE)]),
    New = sets:from_list([Name || {Name, _, _, _, _, _} <- Specs]),
    Kill = sets:subtract(Old, New),

    sets:fold(fun (Id, ok) ->
                      supervisor:terminate_child(?MODULE, Id),
                      supervisor:delete_child(?MODULE, Id),
                      ok
              end, ok, Kill),

    [supervisor:start_child(?MODULE, Spec) || Spec <- Specs],
    ok.

%% @spec init([]) -> SupervisorTree
%% @doc ervisor callback.
init([]) ->
    ok = load_ibrowse_config(),

    Action = envy:get(oc_chef_wm, enable_actions, false, boolean),

    Ip = envy:get(oc_chef_wm, ip, string),
    Port = envy:get(oc_chef_wm, port, pos_integer),
    WebConfig = [
                 {name, rest_erchef},
                 {dispatch_group, rest_erchef},
                 {ip, Ip},
                 {port, Port},
                 % TODO - why is this here, and not pulling from config?
                 {log_dir, "priv/log"},
                 {dispatch, dispatch_table()}],

    Web = {rest_erchef,
           {webmachine_mochiweb, start, [WebConfig]},
           permanent, 5000, worker, dynamic},

    KeyRing = {chef_keyring,
               {chef_keyring, start_link, []},
               permanent, brutal_kill, worker, [chef_keyring]},

    KeyGenWorkerSup = {chef_keygen_worker_sup,
                       {chef_keygen_worker_sup, start_link, []},
                       permanent, 5000, supervisor, [chef_keygen_worker_sup]},

    KeyCache = {chef_keygen_cache,
                {chef_keygen_cache, start_link, []},
                permanent, 5000, worker, [chef_keygen_cache]},

    Index = {chef_index_sup,
             {chef_index_sup, start_link, []},
             permanent, 5000, supervisor, [chef_index_sup]},

    {ok, { {one_for_one, 10, 10}, maybe_start_action(Action, [KeyRing,
                                                              Index,
                                                              KeyGenWorkerSup,
                                                              KeyCache,
                                                              Web])}}.


maybe_start_action(true, Workers) ->
    lager:info("Starting oc_chef_action", []),
    Vhost = ?QUEUE_MONITOR_SETTING(queue_length_monitor_vhost, "/analytics"),
    Queue = ?QUEUE_MONITOR_SETTING(queue_length_monitor_queue, "alaska"),
    QMEnabled = ?QUEUE_MONITOR_SETTING(queue_length_monitor_enabled, false),

    case QMEnabled of
        true ->
            chef_wm_rabbitmq_management:create_pool(),
            {MaxLength, CurrentLength} = check_queue_at_capacity(Vhost, Queue),
            ActionQueueMonitoringSpec = {chef_wm_actions_queue_monitoring,
                    {chef_wm_actions_queue_monitoring, start_link,
                     [Vhost, Queue, MaxLength, CurrentLength]},
                        permanent, 5000, worker, [chef_wm_actions_queue_monitoring]},
            lager:info("Starting actions queue monitoring for vhost ~p and queue ~p", [Vhost, Queue]),
            [ActionQueueMonitoringSpec | [ amqp_child_spec() | Workers]];
        false ->
            [ amqp_child_spec() | Workers]
    end;

maybe_start_action(false, Workers) ->
    lager:info("Not starting Actionlog supervisor since actionlog is disabled."),
    Workers.


check_queue_at_capacity(Vhost, Queue) ->
    PreventStartupOnCap = ?QUEUE_MONITOR_SETTING(prevent_erchef_startup_on_full_capacity, false),
    {MaxLength, CurrentLength, QueueAtCapacity} =
        chef_wm_rabbitmq_management:sync_check_queue_at_capacity(Vhost, Queue),
    case QueueAtCapacity of
        true ->
            case PreventStartupOnCap of
                true ->
                    lager:critical("Vhost ~p, queue ~p at capacity, cannot start", [Vhost, Queue]),
                    erlang:error(analytics_queue_at_capacity);
                false ->
                    lager:warning("Vhost ~p, queue ~p at capacity", [Vhost, Queue]),
                    {MaxLength, CurrentLength}
            end;
        false ->
            lager:info("Vhost ~p, queue ~p not at capacity or RabbitMQ Management Plugin unavailable",
                        [Vhost, Queue]),
            {MaxLength, CurrentLength}
    end.



load_ibrowse_config() ->
    %% FIXME: location of the ibrowse.config should be itself configurable. Also need to
    %% revisit what's in that config to ensure it is as useful as possible.
    ConfigFile = filename:absname(filename:join(["etc", "ibrowse", "ibrowse.config"])),
    lager:info("Loading ibrowse configuration from ~s~n", [ConfigFile]),
    ok = ibrowse:rescan_config(ConfigFile),
    ok.

dispatch_table() ->
    {ok, Dispatch} = file:consult(filename:join(
            [code:priv_dir(oc_chef_wm), "dispatch.conf"])),
    add_custom_settings(maybe_add_default_org_routes(Dispatch)).

maybe_add_default_org_routes(Dispatch) ->
    case oc_chef_wm_routes:default_orgname() of
       DefaultOrgName when is_binary(DefaultOrgName),
                           byte_size(DefaultOrgName) > 0->
           add_default_org_routes(Dispatch,DefaultOrgName);
       _ ->
           Dispatch
    end.

add_default_org_routes(OrigDispatch, DefaultOrgName) ->
    [Y || Y <- [map_to_default_org_route(X, DefaultOrgName) || X <- OrigDispatch], Y =/= undefined] ++ OrigDispatch.

%% Munges the matching routes into the default org equivalent.
map_to_default_org_route({["organizations", organization_id, Resource | R], Module, Args}, DefaultOrgName)
    when is_list(Resource) ->
    case lists:member(Resource, ?OSC11_COMPAT_RESOURCES) of
        true -> {[Resource] ++ R, Module, Args ++ [{organization_name, DefaultOrgName}]};
           _ -> undefined
    end;
map_to_default_org_route(_, _) ->
    undefined.

add_custom_settings(Dispatch) ->
    Dispatch1 = add_resource_init(Dispatch),
    case envy:get(oc_chef_wm, request_tracing, undefined, boolean) of
        true ->
            [{["_debug", "trace", '*'], wmtrace_resource, [{trace_dir, "/tmp"}]} | Dispatch1];
        _ ->
            Dispatch1
    end.

%% @doc Add default and module-specific init params to the `Dispatch' list. This is useful
%% for initializing resource modules with config that needs to be computed and isn't
%% ammenable to `file:consult'. For example, we use it to insert parameters derrived from
%% other application config as well as to provide access to compiled regular expressions.
%%
%% Each module can optionally export a `fetch_custom_init_params/1' function. This function
%% will be passed the proplist of default params and should return a new proplist
%% incorporating any custom parameters. This setup allows a module to override a default
%% value if desired.
add_resource_init(Dispatch) ->
    Defaults = default_resource_init(),
    add_resource_init(Dispatch, Defaults, []).

add_resource_init([Rule | Rest], Defaults, Acc) ->
    add_resource_init(Rest, Defaults, [add_init(Rule, Defaults) | Acc]);
add_resource_init([], _Defaults, Acc) ->
    lists:reverse(Acc).

%% Combine the statically defined init params with defaults and any custom params defined by
%% the module.
add_init({Route, Guard, Module, Init}, Defaults) ->
    InitParams = Init ++ fetch_custom_init_params(Module, Defaults),
    {Route, Guard, Module, InitParams};
add_init({Route, Module, Init}, Defaults) ->
    InitParams = Init ++ fetch_custom_init_params(Module, Defaults),
    {Route, Module, InitParams}.

%% If a resource module requires additional parameters be passed to its init function, it
%% should export `fetch_custom_init_params/1' which should return a proplist. The function
%% will be given the proplist of default params. The function should return a new list
%% containing both the defaults (possibly modified) and the additional params.
fetch_custom_init_params(Module, Defaults) ->
    Exports = proplists:get_value(exports, Module:module_info()),
    case lists:member({fetch_init_params, 1}, Exports) of
        true -> Module:fetch_init_params(Defaults);
        false -> Defaults
    end.

%% @doc Return a proplist of init parameters that should be passed to all resource modules.
default_resource_init() ->
    %% We will only have one release, until such time as we start doing live upgrades.  When
    %% and if that time comes, this will probably fail.
    [{ServerName, ServerVersion, _, _}] = release_handler:which_releases(permanent),

    Defaults = [{auth_skew, envy:get(oc_chef_wm, auth_skew, non_neg_integer)},
                {reqid_header_name, envy:get(oc_chef_wm, reqid_header_name, string)},
                %% These will be used to generate the X-Ops-API-Info header
                {otp_info, {ServerName, ServerVersion}},
                {server_flavor, envy:get(oc_chef_wm, server_flavor, string)},
                {api_version, envy:get(oc_chef_wm, api_version, string)},

                %% This is set if default_orgname mode is enabled
                {default_orgname, oc_chef_wm_routes:default_orgname()},

                %% metrics and stats_hero config. We organize these into a proplist which
                %% will end up in the base_state record rather than having a key for each of
                %% these in base state.
                {metrics_config,
                 [{root_metric_key, envy:get(oc_chef_wm, root_metric_key, string)},
                  %% the following two are hard-coded calls to oc_chef_wm. These could
                  %% be factored out into app config if we wanted ultimate flexibility. At
                  %% that point, we might want a label and upstream function to form a
                  %% behavior defined in stats_hero.
                  {stats_hero_upstreams, oc_chef_wm_base:stats_hero_upstreams()},
                  {stats_hero_label_fun, {oc_chef_wm_base, stats_hero_label}}]}
               ],
    case envy:get(oc_chef_wm, request_tracing, undefined, boolean) of
        true ->
            [{trace, true}|Defaults];
        _ ->
            Defaults
    end.

amqp_child_spec() ->
    Host = envy_parse:host_to_ip(oc_chef_wm, actions_host),
    Port = envy:get(oc_chef_wm, actions_port, non_neg_integer),
    User = envy:get(oc_chef_wm, actions_user, binary),
    Password = envy:get(oc_chef_wm, actions_password, binary),
    VHost = envy:get(oc_chef_wm, actions_vhost, binary),
    ExchgName = envy:get(oc_chef_wm, actions_exchange, binary),
    Exchange = {#'exchange.declare'{exchange=ExchgName,
                                    type= <<"topic">>,
                                    durable=true
                                   }
               },
    Network = {network, Host, Port, {User, Password}, VHost},
    lager:info("Chef Actions: Connecting to RabbitMQ at ~p:~p~s (exchange: ~p)", [Host, Port, VHost, ExchgName]),
    {oc_chef_action_queue, {bunnyc, start_link, [oc_chef_action_queue, Network, Exchange, []]},
      permanent, 5000, worker, dynamic}.
