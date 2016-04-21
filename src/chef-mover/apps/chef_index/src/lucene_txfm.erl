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


-module(lucene_txfm).
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

-include_lib("eunit/include/eunit.hrl").

%% Add clauses to this function to transform syntax nodes
%% from the parser into semantic output.
transform('query', Node, _Index) ->
    ?i2b(Node);
transform('field_name', Node, _Index) ->
    ?i2b(Node);
transform('term', Node, _Index) ->
    ?i2b(Node);
transform(field_phrase, Node, _Index) ->
    P = ?gv(str, Node),
    ?i2b([<<"content:\"">>, ?gv(name, Node), <<"__=__">>, P, <<"\"">>]);
transform(field, Node, _Index) ->
    ?i2b([<<"content:">>, ?gv(name, Node), <<"__=__">>, ?gv(arg, Node)]);
transform('field_range', Node, _Index) ->
    % FIXME: this needs a cleanup
    case Node of
        [FieldName, <<":">>, [<<"[">>, <<"*">>, <<" TO ">>, <<"*">>, <<"]">>]] ->
            ?i2b([<<"content:">>, FieldName, <<"__=__*">>]);
        [FieldName, <<":">>, [<<"{">>, <<"*">>, <<" TO ">>, <<"*">>, <<"}">>]] ->
            ?i2b([<<"content:">>, FieldName, <<"__=__*">>]);

        [FieldName, <<":">>, [<<"[">>, S, <<" TO ">>, <<"*">>, <<"]">>]] ->
            ?i2b([<<"content:[">>, FieldName, <<"__=__">>, S, <<" TO ">>,
                  FieldName, <<"__=__\\ufff0]">>]);
        [FieldName, <<":">>, [<<"{">>, S, <<" TO ">>, <<"*">>, <<"}">>]] ->
            ?i2b([<<"content:{">>, FieldName, <<"__=__">>, S, <<" TO ">>,
                  FieldName, <<"__=__\\ufff0}">>]);

        [FieldName, <<":">>, [<<"[">>, <<"*">>, <<" TO ">>, E, <<"]">>]] ->
            ?i2b([<<"content:[">>, FieldName, <<"__=__">>, <<" TO ">>,
                  FieldName, <<"__=__">>, E, <<"]">>]);
        [FieldName, <<":">>, [<<"{">>, <<"*">>, <<" TO ">>, E, <<"}">>]] ->
            ?i2b([<<"content:{">>, FieldName, <<"__=__">>, <<" TO ">>,
                  FieldName, <<"__=__">>, E, <<"}">>]);
        
        [FieldName, <<":">>, [<<"[">>, S, <<" TO ">>, E, <<"]">>]] ->
            ?i2b([<<"content:[">>, FieldName, <<"__=__">>, S, <<" TO ">>,
                  FieldName, <<"__=__">>, E, <<"]">>]);
        [FieldName, <<":">>, [<<"{">>, S, <<" TO ">>, E, <<"}">>]] ->
            ?i2b([<<"content:{">>, FieldName, <<"__=__">>, S, <<" TO ">>,
                  FieldName, <<"__=__">>, E, <<"}">>])
    end;
transform('space', Node, _Index) ->
    ?i2b(Node);
transform('group', Node, _Index) ->
    [<<"(">>, ?gv(group, Node), <<")">>];
transform('binary_op', Node, _Index) ->
    ?i2b([?gv(lhs, Node), <<" ">>, ?gv(op, Node), <<" ">>, ?gv(rhs, Node)]);
transform('not_op', Node, _Index) ->
    Op = ?gv(op, hd(Node)),
    Spc = case tl(hd(Node)) of
              [] -> <<"">>;
              [S] -> S
          end,
    [Op, Spc, ?gv(arg, tl(Node))];
transform(Rule, Node, _Index)
  when Rule == 'required_op' orelse Rule == 'prohibited_op' ->
    ?i2b([?gv(op, Node), ?gv(arg, Node)]);
transform(string, Node, _Index) ->
    ?i2b([<<"\"">>, ?gv(str, Node), <<"\"">>]);
transform(boost_op, Node, _Index) ->
    ?i2b([?gv(arg, Node), <<"^">>, ?gv(param, Node)]);
transform(fuzzy_op, Node, _Index) ->
    case ?gv(param, Node) of
        [] -> [?gv(arg, Node), <<"~">>];
        Param -> [?gv(arg, Node), <<"~">>, Param]
    end;
transform(Symbol, Node, _Index) when is_atom(Symbol) ->
  Node.
