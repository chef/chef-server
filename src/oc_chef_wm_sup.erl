%% -*- erlang-indent-level: 4;indent-tabs-mode: nil; fill-column: 92 -*-
%% ex: ts=4 sw=4 et
%% @author Kevin Smith <kevin@opscode.com>
%% @copyright 2011-2012 Opscode Inc.

-module(oc_chef_wm_sup).

-behaviour(supervisor).

%% External exports
-export([start_link/0, upgrade/0]).

%% supervisor callbacks
-export([init/1]).

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
%% @doc supervisor callback.
init([]) ->
    ok = load_ibrowse_config(),
    ok = enable_org_cache(),
    {ok, Ip} = application:get_env(oc_chef_wm, ip),
    {ok, Port} = application:get_env(oc_chef_wm, port),
    {ok, Dispatch} = file:consult(filename:join(
                         [filename:dirname(code:which(?MODULE)),
                          "..", "priv", "dispatch.conf"])),
    WebConfig = [
                 {ip, Ip},
                 {port, Port},
                 {log_dir, "priv/log"},
                 {dispatch, add_custom_settings(Dispatch)}],

    Web = {webmachine_mochiweb,
           {webmachine_mochiweb, start, [WebConfig]},
           permanent, 5000, worker, dynamic},

    KeyRing = {chef_keyring,
               {chef_keyring, start_link, []},
               permanent, brutal_kill, worker, [chef_keyring]},

    Index = {chef_index_sup,
             {chef_index_sup, start_link, []},
             permanent, 5000, supervisor, [chef_index_sup]},

    EredisSup = {oc_chef_wm_eredis_sup,
                 {oc_chef_wm_eredis_sup, start_link, []},
                 permanent, 2000, supervisor, [oc_chef_wm_eredis_sup]},

    EredisStarter = {oc_chef_wm_eredis_starter,
                     {oc_chef_wm_eredis_starter, start_link, []},
                     transient, brutal_kill, worker, [oc_chef_wm_eredis_starter]},

    Processes = [KeyRing, EredisSup, EredisStarter, Index, Web],
    {ok, { {one_for_one, 10, 10}, Processes} }.


load_ibrowse_config() ->
    %% FIXME: location of the ibrowse.config should be itself configurable. Also need to
    %% revisit what's in that config to ensure it is as useful as possible.
    ConfigFile = filename:absname(filename:join(["etc", "ibrowse", "ibrowse.config"])),
    error_logger:info_msg("Loading ibrowse configuration from ~s~n", [ConfigFile]),
    ok = ibrowse:rescan_config(ConfigFile),
    ok.

enable_org_cache() ->
    %% FIXME: should this config live at the oc_chef_wm level?
    case application:get_env(chef_db, cache_defaults) of
        undefined ->
            error_logger:info_msg("Org guid cache disabled~n");
        {ok, _Defaults} ->
            chef_cache:init(org_guid),
            error_logger:info_msg("Org guid cache enabled~n")
    end,
    ok.

add_custom_settings(Dispatch) ->
    Dispatch1 = add_resource_init(Dispatch),
    case application:get_env(oc_chef_wm, request_tracing) of
        {ok, true} ->
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

    Defaults = [{auth_skew, get_env(oc_chef_wm, auth_skew)},
                {db_type, get_env(sqerl, db_type)},
                {reqid_header_name, get_env(oc_chef_wm, reqid_header_name)},

                %% These will be used to generate the X-Ops-API-Info header
                {otp_info, {ServerName, ServerVersion}},
                {server_flavor, get_env(oc_chef_wm, server_flavor)},
                {api_version, get_env(oc_chef_wm, api_version)},
                %% metrics and stats_hero config. We organize these into a proplist which
                %% will end up in the base_state record rather than having a key for each of
                %% these in base state.
                {metrics_config,
                 [{root_metric_key, get_env(oc_chef_wm, root_metric_key)},
                  %% the following two are hard-coded calls to ?BASE_RESOURCE. These could
                  %% be factored out into app config if we wanted ultimate flexibility. At
                  %% that point, we might want a label and upstream function to form a
                  %% behavior defined in stats_hero.
                  {stats_hero_upstreams, oc_chef_wm_base:stats_hero_upstreams()},
                  {stats_hero_label_fun, {oc_chef_wm_base, stats_hero_label}}]}
               ],
    case application:get_env(oc_chef_wm, request_tracing) of
        {ok, true} ->
            [{trace, true}|Defaults];
        _ ->
            Defaults
    end.

get_env(App, Key) ->
    {ok, Value} = application:get_env(App, Key),
    Value.
