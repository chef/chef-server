%% -*- erlang-indent-level: 4;indent-tabs-mode: nil; fill-column: 92 -*-
%% ex: ts=4 sw=4 et
%% @author Eric B Merritt <ericbmerritt@gmail.com>
%% @copyright Copyright 2012 Opscode, Inc.
-module(bksw_util).

-export([get_bucket/1,
         get_object_and_bucket/1,
         file/1,
         to_integer/1,
         to_string/1,
         to_binary/1]).

%%===================================================================
%% API functions
%%===================================================================
file(Path) ->
    filename:join(code:priv_dir(bookshelf_wi), Path).

-spec to_integer(string() | binary()) -> integer().
to_integer(Val) ->
    erlang:list_to_integer(to_string(Val)).

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

-spec get_bucket(term()) -> term().
get_bucket(Req0) ->
    case wrq:path_info(bucket, Req0) of
        undefined ->
            %% We would through a bad match here but you cant have
            %% guards in a match which is really unfortunate
            erlang:error(bad_bucket_dep);
        GoodValue ->
            to_binary(GoodValue)
    end.

get_object_and_bucket(Rq0) ->
    case string:tokens(wrq:path(Rq0), "/") of
        [] ->
            {ok, <<"">>, <<"">>};
        [Bucket] ->
            {ok, bksw_util:to_binary(Bucket),
             <<"">>};
        [Bucket | Path] ->
            {ok, bksw_util:to_binary(Bucket),
             bksw_util:to_binary(filename:join(Path))};
        _ ->
            {error, invalid_path}
    end.
