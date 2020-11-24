%% -*- erlang-indent-level: 4;indent-tabs-mode: nil; fill-column: 92 -*-
%% ex: ts=4 sw=4 et
%% @author Kevin Smith
%% @copyright 2011-2014 Chef Software Inc.

-module(oc_chef_wm_sup).

-behaviour(supervisor).

%% External exports
-export([start_link/0]).

%% supervisor callbacks
-export([init/1,
         default_resource_init/0]).

-include("oc_chef_wm.hrl").
-define(VERSION_PATH, '/opt/opscode/version-manifest.txt').


%% @spec start_link() -> ServerRet
%% @doc API for starting the supervisor.
start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% @spec init([]) -> SupervisorTree
%% @doc ervisor callback.
init([]) ->
    ok = load_ibrowse_config(),

    Ip = envy:get(oc_chef_wm, ip, string),
    Port = envy:get(oc_chef_wm, port, pos_integer),
    Ssl = envy:get(oc_chef_wm, ssl, false, boolean),
    SslOpts = envy:get(oc_chef_wm, ssl_opts, [], list),

    WebConfig = [
                 {ip, Ip},
                 {port, Port},
                 {log_dir, "priv/log"},
                 {dispatch, dispatch_table()},
                 {ssl, Ssl},
                 {ssl_opts, SslOpts}],

    Web = {webmachine_mochiweb,
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

    {ok, { {one_for_one, 10, 10}, [KeyRing, Index, KeyGenWorkerSup,
                                   KeyCache, Web]}}.

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
                {version, get_version()},
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

get_version() ->
    {ok, Device} = file:open(?VERSION_PATH, [read]),
    %% Assuming that the first line of the file will have the version.
    Version =
        case file:read_line(Device) of
            {ok, Data} -> list_to_binary(lists:subtract(Data,"chef-server \n"));
            _ -> <<"error">>
        end,
    file:close(Device),
    Version.
