%% -*- erlang-indent-level: 4;indent-tabs-mode: nil -*-
%% ex: ts=4 sw=4 et
%%
%% @author Marc Paradise <marc@chef.io>
%%
-module(user_default).
% required to expose the record definitions we load in from various includes
-compile(export_all).
% export_all triggers compilation warnings which are set to be treated as errors
-compile(nowarn_export_all).
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

um(Mod) ->
    Info = Mod:module_info(),
    CompileInfo = proplists:get_value(compile, Info),
    Path = proplists:get_value(source, CompileInfo),
    Options = [{d, 'OC_CHEF'},
               debug_info,
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
%headers() ->
    %    ["/host/oc_erchef/include/chef_types.hrl","/host/oc_erchef/include/oc_chef_authz.hrl", "/host/oc_erchef/include/oc_chef_types.hrl"].

cov_enable(ProjectPath, all, Deps) ->
    % We must stop instead of pause because otherwise sync doesn't listen and still recompiles shit...
    sync_action(stop),
    case Deps of
        true ->
            [cover:compile_beam_directory(Dir ) || Dir <- filelib:wildcard(filename:join(ProjectPath, "deps/*/ebin"))];
        _ ->
            noop
    end,
    [cover:compile_beam_directory(Dir) || Dir <- filelib:wildcard(filename:join(ProjectPath, "ebin"))],
    [cover:compile_beam_directory(Dir) || Dir <- filelib:wildcard(filename:join(ProjectPath, "apps/*/ebin"))],
    io:fwrite("Coverage enabled."),
    ok;
cov_enable(_ProjectPath, Module, _Deps) ->
    sync_action(stop),
    cover:compile_beam(Module),
    ok.

cov_report(OutputPath, all, NoHTML) ->
    [cov_report(OutputPath, Mod, NoHTML) || Mod <- cover:modules()];
cov_report(OutputPath, Mod, NoHTML) ->
    % TODO project-specific output paths?
    {Opts, FileName} = case NoHTML of
                           false -> {[html], filename:join(OutputPath, binary_to_list(iolist_to_binary([atom_to_list(Mod), ".COVER.html"])))};
                           true -> {[], filename:join(OutputPath, binary_to_list(iolist_to_binary([atom_to_list(Mod), ".COVER.out"])))}
                       end,
    cover:analyze_to_file(Mod, FileName, Opts).

cov_reset(all) ->
    cover:reset();
cov_reset(Mod) ->
    cover:reset(Mod).

cov_disable() ->
    cover:stop(),
    sync_action(go).

sync_action(Action) when Action =:= start;
                         Action =:= stop;
                         Action =:= pause;
                         Action =:= go ->
    case module_loaded(sync) of
        true -> apply(sync, Action, []);
        _ -> ok
    end.

