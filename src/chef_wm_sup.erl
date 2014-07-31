%% -*- erlang-indent-level: 4;indent-tabs-mode: nil; fill-column: 92 -*-
%% ex: ts=4 sw=4 et
%% @author Kevin Smith <kevin@opscode.com>
%% Copyright 2011-2012 Opscode, Inc. All Rights Reserved.
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


-module(chef_wm_sup).

-behaviour(supervisor).

-include("chef_wm.hrl").

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
    Ip = envy:get(chef_wm, ip, string),
    Port = envy:get(chef_wm, port, positive_integer),
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

    Folsom = {folsom_sup,
              {folsom_sup, start_link, []},
              permanent, 5000, supervisor, [folsom_sup]},

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

    Processes = [Folsom, KeyRing, Index, KeyGenWorkerSup, KeyCache, Web],
    {ok, { {one_for_one, 10, 10}, Processes} }.

add_custom_settings(Dispatch) ->
    Dispatch1 = add_resource_init(Dispatch),
    case application:get_env(chef_wm, request_tracing) of
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

    Defaults = [{auth_skew, envy:get(chef_wm, auth_skew, positive_integer)},
                {reqid_header_name, envy:get(chef_wm, reqid_header_name, string)},

                %% These will be used to generate the X-Ops-API-Info header
                {otp_info, {ServerName, ServerVersion}},
                {server_flavor, envy:get(chef_wm, server_flavor, string)},
                {api_version, envy:get(chef_wm, api_version, string)},

                %% metrics and stats_hero config. We organize these into a proplist which
                %% will end up in the base_state record rather than having a key for each of
                %% these in base state.
                {metrics_config,
                 [{root_metric_key, envy:get(chef_wm, root_metric_key, string)},
                  %% the following two are hard-coded calls to ?BASE_RESOURCE. These could
                  %% be factored out into app config if we wanted ultimate flexibility. At
                  %% that point, we might want a label and upstream function to form a
                  %% behavior defined in stats_hero.
                  {stats_hero_upstreams, ?BASE_RESOURCE:stats_hero_upstreams()},
                  {stats_hero_label_fun, {?BASE_RESOURCE, stats_hero_label}}]}
               ],
    case envy:get(chef_wm, request_tracing, Defaults, boolean) of
        true ->
            [{trace, true} | Defaults];
        Defaults ->
            Defaults
    end.

