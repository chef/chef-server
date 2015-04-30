%% -*- erlang-indent-level: 4;indent-tabs-mode: nil -*-
%% ex: ts=4 sw=4 et
%%
%% @author Marc Paradise <marc@chef.io>
%%
-module(user_default).
% required to expose the record definitions we load in from various includes
-compile(export_all).
%
% Invoke this from the erlang shell to reload a module in place
% for example, um(oc_chef_wm_base).
%
% May not always behave well if you're reload a module used
% by a running process, though it does attempt to unload and purge
% any existing instances before loading.
%
% Note that this does not update deployed beam files. If you stop the oc_erchef
% process you'll need to run make (or make compile_skip) to make sure all
% modules you've updated in the process of iterating get pulled in.
%
um(Mod) ->
    Info = Mod:module_info(),
    CompileInfo = proplists:get_value(compile, Info),
    Path = proplists:get_value(source, CompileInfo),
    Options = [{d, 'OC_CHEF'},
               {debug_info, debug_info},
               {d,'CHEF_WM_DARKLAUNCH',xdarklaunch_req},
               {d,'CHEF_DB_DARKLAUNCH',xdarklaunch_req},
               {parse_transform,lager_transform},
               {i, "include"},
               return,
               binary],

    case compile:file(Path, Options) of
        {ok, _Mod, Bin, []} ->
            Reload = reload(Mod, Path, Bin),
            {{reload, Reload}, ok};
        {ok, _Mod, Bin, Warnings} ->
            Reload = reload(Mod, Path, Bin),
            {{reload, Reload}, {warnings, Warnings}};
        {ok, _Mod, Bin} ->
            Reload = reload(Mod, Path, Bin),
            {{reload, Reload}, ok};
        Error ->
            {errors, Error}
    end.


reload(Mod, Path, Binary) ->
    code:delete(Mod),
    code:purge(Mod),
    code:load_binary(Mod, Path, Binary).

db_ctx() ->
    chef_db:make_context(<<"ABCD">>).

% You can use this from the shell to load up record definitions:
% [rr(Path) || Path <- headers()].
headers() ->
    ["/srv/piab/mounts/oc_erchef/include/chef_types.hrl","/srv/piab/mounts/oc_erchef/include/oc_chef_authz.hrl", "/srv/piab/mounts/oc_erchef/include/oc_chef_types.hrl"].
