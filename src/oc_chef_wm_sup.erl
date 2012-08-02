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

%% The bulk_get implementation relies on hard-coded prepared queries for 1, 2, ..., N for a
%% batch size of N. For now, we just ensure that we don't configure something too large by
%% accident.
-define(MAX_BULK_GET_SIZE, 5).

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
    validate_bulk_fetch_batch_size(),
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
                 {dispatch, add_resource_init(Dispatch)}],

    Web = {webmachine_mochiweb,
           {webmachine_mochiweb, start, [WebConfig]},
           permanent, 5000, worker, dynamic},

    Folsom = {folsom_sup,
              {folsom_sup, start_link, []},
              permanent, 5000, supervisor, [folsom_sup]},

    KeyRing = {chef_keyring,
               {chef_keyring, start_link, []},
               permanent, brutal_kill, worker, [chef_keyring]},

    Index = {chef_index_sup,
             {chef_index_sup, start_link, []},
             permanent, 5000, supervisor, [chef_index_sup]},

    Processes = [Folsom, KeyRing, Web, Index],
    {ok, { {one_for_one, 10, 10}, Processes} }.


load_ibrowse_config() ->
    %% FIXME: location of the ibrowse.config should be itself configurable. Also need to
    %% revisit what's in that config to ensure it is as useful as possible.
    ConfigFile = filename:absname(filename:join(["etc", "ibrowse", "ibrowse.config"])),
    error_logger:info_msg("Loading ibrowse configuration from ~s~n", [ConfigFile]),
    ok = ibrowse:rescan_config(ConfigFile),
    ok.

enable_org_cache() ->
    %% FIXME: should this config live at the chef_rest level?
    case application:get_env(chef_db, cache_defaults) of
        undefined ->
            error_logger:info_msg("Org guid cache disabled~n");
        {ok, _Defaults} ->
            chef_cache:init(org_guid),
            error_logger:info_msg("Org guid cache enabled~n")
    end,
    ok.

validate_bulk_fetch_batch_size() ->
    %% Batch size for bulk_fetch is currently tightly coupled to hard-coded prepared SQL
    %% queries. We verify the value does not exceed the the predefined queries here while we
    %% are investigating a more elegant solution.
    {ok, BatchSize} = application:get_env(oc_chef_wm, bulk_fetch_batch_size),
    case BatchSize > ?MAX_BULK_GET_SIZE of
        true ->
            error_logger:error_msg("bulk_fetch_batch_size of ~p is larger than "
                                   "supported max of ~p~n",
                                   [BatchSize, ?MAX_BULK_GET_SIZE]),
            erlang:error({error, {bulk_fetch_batch_size_too_large, BatchSize}});
        false ->
            error_logger:info_msg("bulk_fetch_batch_size set to ~p ~n", [BatchSize]),
            ok
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
    [{batch_size, get_env(oc_chef_wm, bulk_fetch_batch_size)},
     {auth_skew, get_env(oc_chef_wm, auth_skew)},
     {db_type, get_env(sqerl, db_type)},
     {reqid_header_name, get_env(oc_chef_wm, reqid_header_name)}].

get_env(App, Key) ->
    {ok, Value} = application:get_env(App, Key),
    Value.
