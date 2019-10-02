
-module(check_rebar).

-export([parse_check_list/1,
         generate_file_list/0,
         parse_file_list/1,
         check_files/2]).

parse_check_list(L) ->
    FL = generate_file_list(),
    D = parse_file_list(FL),
    check_files(D,L).


generate_file_list() ->
    %% hard coded now,
    ["../src/oc_erchef/apps/data_collector/rebar.lock",
     "../src/oc_erchef/rebar.lock",
     "../src/oc_bifrost/rebar.lock",
     "../src/chef-mover/rebar.lock",
     "../src/bookshelf/rebar.lock" ].

%% checking if rebar3 is the same
parse_file_list(Files) ->
    FileData = [{File, parse_rebar_lock(File)} || File <- Files],
    FileData.

%%
%%
%%
parse_rebar_lock(File) ->
    {ok, Terms} = file:consult(File),
%    io:format("~p", [Terms]),
    Data = lists:foldl(fun parse_lock_list_term/2, #{}, Terms),
    Data.

parse_lock_list_term({Vsn, Deps}, Data) when is_list(Vsn) andalso is_list(Deps) ->
    lists:foldl(fun parse_lock_list_item/2, Data, Deps);
parse_lock_list_term([{pkg_hash, Terms}], Data) when is_list(Terms) ->
    lists:foldl(fun parse_lock_list_hash/2, Data, Terms);
parse_lock_list_term(Deps, Data) when is_list(Deps) ->
    lists:foldl(fun parse_lock_list_item/2, Data, Deps).



parse_lock_list_item({Name, Source, _}, Data) when is_binary(Name) ->
    maps:put(Name, Source, Data);
parse_lock_list_item({Name, Source}, Data) when is_binary(Name) ->
    maps:put(Name, Source, Data);
parse_lock_list_item(Item, Data) ->
    io:format("Unparsed item: ~p", [Item]),
    Data.

parse_lock_list_hash({Name, Source}, Data) ->
    maps:put({Name, hash}, Source, Data).



%
%
%
check_files(Datafile, L) ->
    % first reindex the results
    Deps = by_dep_name(Datafile),
    check_deps(Deps, L).


by_dep_name(Datafile) ->
    Deps = #{},
    lists:foldl(fun fold_file_step/2, Deps, Datafile).


fold_file_step({File, Data}, Acc) ->
    FoldDep = fun(Name, Source, A) ->
                      L = maps:get(Name, A, #{}),
                      maps:put(Name, maps:put(Source, [File | maps:get(Source, L, [])], L), A)
              end,
    maps:fold(FoldDep, Acc, Data).

check_deps(Deps, L) ->
    [ check_dep(K, maps:get(K, Deps), L) || K <- lists:sort(maps:keys(Deps)) ],
%    maps:filter(fun(K,V) -> ok /= check_dep(K, V) end, Deps),
    1.

check_dep(Name, Deps, L) ->
    case maps:size(Deps) of
        N when N =< L ->
            [];
        N ->
            io:format("Name ~p (~p): ~p~n", [Name, N, Deps]),
            {Name, N}
    end.
