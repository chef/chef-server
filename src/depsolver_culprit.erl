%% -*- erlang-indent-level: 4; indent-tabs-mode: nil; fill-column: 80 -*-
%% ex: ts=4 sx=4 et
%%%-------------------------------------------------------------------
%%% @author Eric Merritt <ericbmerritt@gmail.com>
%%% @doc
%%%  This app does the culprit search for a failed solve. It searches
%%%  through the goals provided by the user trying to find the first one
%%%  that fails. It then returns that as the culprit along with the
%%%  unknown apps from the goal, the version constrained apps from the
%%%  goal, and the good apps (those not immediately constrained from
%%%  the goals).
%%% @end
-module(depsolver_culprit).

-export([search/3]).

-export_type([detail/0]).

%%============================================================================
%% Types
%%============================================================================
-type detail() :: {UnknownApps::[depsolver:constraint()],
                   VersionConstrained::[depsolver:constraint()],
                   GoodApps::[depsolver:constraint()]}.

%%============================================================================
%% Internal API
%%============================================================================
%% @doc start running the solver, with each run reduce the number of constraints
%% set as goals. At some point the solver should succeed.
-spec search(depsolver:dep_graph(), [depsolver:constraint()], [depsolver:constraint()])
            -> detail() | term().
search(State, ActiveCons=[Culprit | _], []) ->
    case depsolver:primitive_solve(State, ActiveCons) of
        fail ->
            format_culprit_error(State, Culprit, ActiveCons);
        _Success ->
            %% This should *never* happen. 'Culprit' above represents the last
            %% possible constraint that could cause things to fail. There for
            %% this should have failed as well.
            inconsistant_graph_state
    end;
search(State, ActiveCons=[PossibleCulprit | _], [NewCon | Constraints]) ->
    case depsolver:primitive_solve(State, ActiveCons) of
        fail ->
            format_culprit_error(State,
                                 PossibleCulprit,
                                 ActiveCons);
        _Success ->
            %% Move one constraint from the inactive to the active
            %% constraints and run again
            search(State, [NewCon | ActiveCons], Constraints)
    end.

%%============================================================================
%% Internal Functions
%%============================================================================

-spec format_culprit_error(depsolver:dep_graph(), depsolver:constraint(),
                           [depsolver:constraint()]) ->
                                   {unable_to_solve, depsolver:constraint(),
                                    detail()}.
format_culprit_error(State, Culprit, ActiveCons) ->
    Result = sort_constraints(State, ActiveCons),
    {unable_to_solve, Culprit, Result}.

%% @doc sort the constraints into the individual baskets that are contained
%% within the error through. That is unknown apps, known bad (version
%% constrained) and known good apps.
-spec sort_constraints(depsolver:dep_graph(), [depsolver:constraint()]) ->
                              detail().
sort_constraints(State, Cons) ->
    lists:foldl(fun(Con, {AccUnknown, AccBad, AccGood}) ->
                        case is_known(State, Con) of
                            false ->
                                {[Con | AccUnknown], AccBad, AccGood};
                            true ->
                                case is_bad(Con, Cons) of
                                    true ->
                                        {AccUnknown, [Con | AccBad], AccGood};
                                    false ->
                                        {AccUnknown, AccBad, [Con | AccGood]}
                                end
                        end
                end, {[], [], []}, Cons).

%% @doc check taht the specified constraint is in the dependency graph.
-spec is_known(depsolver:dep_graph(), depsolver:constraint()) ->
                      boolean().
is_known(State,  Con) ->
    {PkgName, Vsn} = dep_pkg_vsn(Con),
    case gb_trees:lookup(PkgName, State) of
        none ->
            false;
        {value, Vsns} ->
            case Vsn of
                unspecified ->
                    true;
                _ ->
                    case lists:keyfind(Vsn, 1, Vsns) of
                        false ->
                            false;
                        _ ->
                            true
                    end
            end
    end.

%% @doc check to see if the version constrained to not fit in the solution
%% space.
-spec is_bad(depsolver:constraint(), [depsolver:constraint()]) -> boolean().
is_bad(Con, Constraints) ->
    {PkgName, Vsn} = dep_pkg_vsn(Con),
    PkgCons = [Con1 || Con1 <- Constraints,
                       depsolver:dep_pkg(Con) == PkgName],
    not lists:all(fun (L) ->
                      depsolver:is_version_within_constraint(Vsn, L)
                  end,
                  PkgCons).

%% @doc this is basically the equivilent of dep_pkg. Except it returns the pkg
%% nad version if available or {pkg, unspecified} if not available.
-spec dep_pkg_vsn(depsolver:constraint()) -> {depsolver:pkg_name(), depsolver:vsn()} |
                                         {depsolver:pkg_name(), unspecified}.
dep_pkg_vsn(Pkg={_Pkg, _Vsn}) ->
    Pkg;
dep_pkg_vsn({Pkg, Vsn, _}) ->
    {Pkg, Vsn};
dep_pkg_vsn({Pkg, Vsn1, _Vsn2, _}) ->
    {Pkg, Vsn1};
dep_pkg_vsn(Pkg) when is_atom(Pkg) orelse is_binary(Pkg) ->
    {Pkg, unspecified}.
