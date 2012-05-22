%% -*- erlang-indent-level: 4;indent-tabs-mode: nil; fill-column: 92 -*-
%% ex: ts=4 sw=4 et
%% @author Eric B Merritt <ericbmerritt@gmail.com>
%% @copyright Copyright 2012 Opscode, Inc.
-module(bksw_util).

-export([file/1]).

%%===================================================================
%% API functions
%%===================================================================
file(Path) ->
    filename:join(code:priv_dir(bookshelf), Path).
