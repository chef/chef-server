%% -*- erlang-indent-level: 4;indent-tabs-mode: nil; fill-column: 92 -*-
%% ex: ts=4 sw=4 et
%% @author Seth Falcon <seth@chef.io>
%% Copyright 2011-2012 Opscode, Inc. All Rights Reserved.
%%
%% This file is provided to you under the Apache License,
%% Version 2.0 (the "License"); you may not use this file
%% except in compliance with the License.  You may obtain
%% a copy of the License at
%%
%%   http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing,
%% software distributed under the License is distributed on an
%% "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
%% KIND, either express or implied.  See the License for the
%% specific language governing permissions and limitations
%% under the License.
%%


-module(lucene_sexp).
-export([transform/3]).

-define(i2b(X), iolist_to_binary(X)).
-define(gv(X, L), proplists:get_value(X, L)).

% make sure these atoms are available.
-define(or_op1, 'OR').
-define(or_op2, '||').
-define(and_op1, 'AND').
-define(and_op2, '&&').
-define(not_op, 'NOT').
-define(not_bang_op, '!').

%% Add clauses to this function to transform syntax nodes
%% from the parser into semantic output.
transform('query', Node, _Index) ->
    [ N || N <- Node, N /= <<" ">> ];
transform('field_name', Node, _Index) ->
    ?i2b(Node);
transform('term', Node, _Index) ->
    ?i2b(Node);
transform(field_phrase, Node, _Index) ->
    P = ?i2b(?gv(str, Node)),
    [{field, ?gv(name, Node)}, {term, {phrase, P}}];
transform(field, Node, _Index) ->
    [{field, ?gv(name, Node)}, {term, ?gv(arg, Node)}];
transform('field_range', Node, _Index) ->
    case Node of
        [FieldName, <<":">>, [<<"[">>, S, <<" TO ">>, E, <<"]">>]] ->
            [{range, incl}, {field, FieldName},
             {start, ?i2b(S)}, {'end', ?i2b(E)}];
        [FieldName, <<":">>, [<<"{">>, S, <<" TO ">>, E, <<"}">>]] ->
                        [{range, excl}, {field, FieldName},
                         {start, ?i2b(S)}, {'end', ?i2b(E)}]
    end;
transform('space', Node, _Index) ->
    ?i2b(Node);
transform('group', Node, _Index) ->
    lists:keyfind(group, 1, Node);
transform('binary_op', Node, _Index) ->
    Op = list_to_existing_atom(binary_to_list(?gv(op, Node))),
    {Op, ?gv(lhs, Node), ?gv(rhs, Node)};
transform('not_op', Node, _Index) ->
    Op = list_to_existing_atom(binary_to_list(?gv(op, hd(Node)))),
    {Op, ?gv(arg, tl(Node))};
transform(Rule, Node, _Index)
  when Rule == 'required_op' orelse Rule == 'prohibited_op' ->
    {?gv(op, Node), ?gv(arg, Node)};
transform(string, Node, _Index) ->
    {phrase, ?i2b(?gv(str, Node))};
transform(boost_op, Node, _Index) ->
    Param = parse_param(?gv(param, Node)),
    {boost, ?gv(arg, Node), Param};
transform(fuzzy_op, Node, _Index) ->
    case ?gv(param, Node) of
        [] -> {fuzzy, ?gv(arg, Node)};
        Param -> {fuzzy, ?gv(arg, Node), parse_param(Param)}
    end;
transform(Symbol, Node, _Index) when is_atom(Symbol) ->
  Node.

parse_param(P) ->
    Str = binary_to_list(?i2b(P)),
    try erlang:list_to_float(Str)
        catch error:badarg ->
            erlang:list_to_integer(Str)
    end.

