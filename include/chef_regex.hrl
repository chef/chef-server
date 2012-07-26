%% -*- erlang-indent-level: 4;indent-tabs-mode: nil; fill-column: 92-*-
%% ex: ts=4 sw=4 et
%% @author James Casey <james@opscode.com>
%% @copyright 2012 Opscode, Inc.
%%

-type regex_name() :: cookbook_name |
                      cookbook_version |
                      cookbook_version_constraint |
                      environment_name |
                      client_name |
                      qualified_recipe |
                      qualified_role |
                      recipe_name |
                      unqualified_recipe.

-type re_regex() :: {re_pattern, integer(), integer(), binary()}.
%% FIXME: This type is not yet correct
%% I'd like to use binary() but dialyzer wants this more specific type
-type re_msg() :: <<_:64,_:_*8>>.



