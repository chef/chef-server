%%%-------------------------------------------------------------------
%%% @author Christopher Brown <cb@opscode.com>
%%% @copyright (C) 2011, Opscode, Inc.
%%%-------------------------------------------------------------------
-module(chef_rest_util).

-export([transmute_tuples/1,transmute/1,iso_8601_fmt/1,iso_8601_utc/0]).

transmute_tuples(T) ->
    [ { TName , transmute(TValue)} || { TName , TValue} <- T].

transmute(X) when is_atom(X) ->
    transmute(atom_to_list(X));
transmute(X) when is_binary(X) ->
    X;
transmute(X) when is_list(X) ->
    iolist_to_binary(X).

iso_8601_fmt(DateTime) ->
    [{{Year,Month,Day},{Hour,Min,Sec}}] = DateTime,
    io_lib:format("~s", [io_lib:format("~4.10.0B-~2.10.0B-~2.10.0B ~2.10.0B:~2.10.0B:~2.10.0B",
        [Year, Month, Day, Hour, Min, Sec])]).

iso_8601_utc() ->
    iso_8601_fmt(
      calendar:local_time_to_universal_time_dst(
        erlang:localtime())).
