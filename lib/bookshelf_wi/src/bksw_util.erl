%% -*- erlang-indent-level: 4;indent-tabs-mode: nil; fill-column: 92 -*-
%% ex: ts=4 sw=4 et
%% @author Eric B Merritt <ericbmerritt@gmail.com>
%% @copyright Copyright 2012 Opscode, Inc.
-module(bksw_util).

-export([file/1,
         to_string/1,
         to_binary/1]).

%%===================================================================
%% API functions
%%===================================================================
file(Path) ->
    filename:join(code:priv_dir(bookshelf_wi), Path).

-spec to_string(binary() | string()) -> string().
to_string(Val) when is_binary(Val) ->
    erlang:binary_to_list(Val);
to_string(Val) when is_list(Val) ->
    Val.

-spec to_binary(binary() | string()) -> binary().
to_binary(Val) when is_list(Val) ->
    erlang:list_to_binary(Val);
to_binary(Val) when is_binary(Val) ->
    Val.
