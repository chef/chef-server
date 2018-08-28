%% -*- erlang-indent-level: 4;indent-tabs-mode: nil; fill-column: 92 -*-
%% ex: ts=4 sw=4 et
%% @author Seth Falcon <seth@chef.io>
%% Copyright 2011-2018 Chef Software, Inc.
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


-module(lucene).
-export([parse/1,file/1]).
-compile(nowarn_unused_vars).
-compile({nowarn_unused_function,[p/4, p/5, p_eof/0, p_optional/1, p_not/1, p_assert/1, p_seq/1, p_and/1, p_choose/1, p_zero_or_more/1, p_one_or_more/1, p_label/2, p_string/1, p_anything/0, p_charclass/1, p_attempt/4, line/1, column/1]}).



-spec file(file:name()) -> any().
file(Filename) -> {ok, Bin} = file:read_file(Filename), parse(Bin).

-spec parse(binary() | list()) -> any().
parse(List) when is_list(List) -> parse(list_to_binary(List));
parse(Input) when is_binary(Input) ->
  setup_memo(),
  Result = case 'query'(Input,{{line,1},{column,1}}) of
             {AST, <<>>, _Index} -> AST;
             Any -> Any
           end,
  release_memo(), Result.

'query'(Input, Index) ->
  p(Input, Index, 'query', fun(I,D) -> (p_choose([fun 'star_star'/2, p_zero_or_more(p_choose([fun 'expression'/2, fun 'space'/2]))]))(I,D) end, fun(Node, Idx) -> transform('query', Node, Idx) end).

'star_star'(Input, Index) ->
  p(Input, Index, 'star_star', fun(I,D) -> (p_string(<<"*:*">>))(I,D) end, fun(Node, Idx) -> transform('star_star', Node, Idx) end).

'expression'(Input, Index) ->
  p(Input, Index, 'expression', fun(I,D) -> (p_choose([fun 'operation'/2, fun 'group'/2, fun 'field_phrase'/2, fun 'field'/2, fun 'field_range'/2, fun 'term'/2, fun 'string'/2]))(I,D) end, fun(Node, Idx) -> transform('expression', Node, Idx) end).

'term'(Input, Index) ->
  p(Input, Index, 'term', fun(I,D) -> (p_choose([p_seq([fun 'keyword'/2, p_one_or_more(fun 'valid_letter'/2)]), p_seq([p_not(fun 'keyword'/2), p_one_or_more(fun 'valid_letter'/2)])]))(I,D) end, fun(Node, Idx) -> transform('term', Node, Idx) end).

'field'(Input, Index) ->
  p(Input, Index, 'field', fun(I,D) -> (p_seq([p_label('name', fun 'field_name'/2), p_string(<<":">>), p_label('arg', p_choose([fun 'fuzzy_op'/2, fun 'term'/2, fun 'group'/2, fun 'string'/2]))]))(I,D) end, fun(Node, Idx) -> transform('field', Node, Idx) end).

'field_phrase'(Input, Index) ->
  p(Input, Index, 'field_phrase', fun(I,D) -> (p_seq([p_label('name', fun 'field_name'/2), p_string(<<":">>), p_string(<<"\"">>), p_label('str', p_seq([fun 'term'/2, p_zero_or_more(p_seq([fun 'space'/2, fun 'term'/2]))])), p_string(<<"\"">>)]))(I,D) end, fun(Node, Idx) -> transform('field_phrase', Node, Idx) end).

'field_range'(Input, Index) ->
  p(Input, Index, 'field_range', fun(I,D) -> (p_seq([fun 'field_name'/2, p_string(<<":">>), p_choose([p_seq([p_string(<<"[">>), fun 'range_entry'/2, p_string(<<"\sTO\s">>), fun 'range_entry'/2, p_string(<<"]">>)]), p_seq([p_string(<<"{">>), fun 'range_entry'/2, p_string(<<"\sTO\s">>), fun 'range_entry'/2, p_string(<<"}">>)])])]))(I,D) end, fun(Node, Idx) -> transform('field_range', Node, Idx) end).

'field_name'(Input, Index) ->
  p(Input, Index, 'field_name', fun(I,D) -> (p_seq([p_not(fun 'keyword'/2), p_one_or_more(fun 'valid_letter'/2)]))(I,D) end, fun(Node, Idx) -> transform('field_name', Node, Idx) end).

'range_entry'(Input, Index) ->
  p(Input, Index, 'range_entry', fun(I,D) -> (p_choose([p_string(<<"*">>), p_one_or_more(fun 'valid_letter'/2)]))(I,D) end, fun(Node, Idx) -> transform('range_entry', Node, Idx) end).

'group'(Input, Index) ->
  p(Input, Index, 'group', fun(I,D) -> (p_seq([p_string(<<"(">>), p_label('group', fun 'query'/2), p_string(<<")">>)]))(I,D) end, fun(Node, Idx) -> transform('group', Node, Idx) end).

'operation'(Input, Index) ->
  p(Input, Index, 'operation', fun(I,D) -> (p_choose([fun 'binary_op'/2, fun 'unary_op'/2, fun 'fuzzy_op'/2, fun 'boost_op'/2]))(I,D) end, fun(Node, Idx) -> transform('operation', Node, Idx) end).

'binary_op'(Input, Index) ->
  p(Input, Index, 'binary_op', fun(I,D) -> (p_seq([p_label('lhs', p_choose([fun 'group'/2, fun 'field_phrase'/2, fun 'field'/2, fun 'field_range'/2, fun 'term'/2])), p_optional(fun 'space'/2), p_label('op', fun 'bool_op'/2), p_optional(fun 'space'/2), p_label('rhs', fun 'query'/2)]))(I,D) end, fun(Node, Idx) -> transform('binary_op', Node, Idx) end).

'bool_op'(Input, Index) ->
  p(Input, Index, 'bool_op', fun(I,D) -> (p_choose([p_string(<<"AND">>), p_string(<<"&&">>), p_string(<<"OR">>), p_string(<<"||">>)]))(I,D) end, fun(Node, Idx) -> transform('bool_op', Node, Idx) end).

'unary_op'(Input, Index) ->
  p(Input, Index, 'unary_op', fun(I,D) -> (p_choose([fun 'not_op'/2, fun 'required_op'/2, fun 'prohibited_op'/2]))(I,D) end, fun(Node, Idx) -> transform('unary_op', Node, Idx) end).

'not_op'(Input, Index) ->
  p(Input, Index, 'not_op', fun(I,D) -> (p_seq([p_choose([p_seq([p_label('op', p_string(<<"NOT">>)), fun 'space'/2]), p_seq([p_label('op', p_string(<<"!">>)), p_optional(fun 'space'/2)])]), p_label('arg', p_choose([fun 'group'/2, fun 'field'/2, fun 'field_range'/2, fun 'term'/2, fun 'string'/2]))]))(I,D) end, fun(Node, Idx) -> transform('not_op', Node, Idx) end).

'required_op'(Input, Index) ->
  p(Input, Index, 'required_op', fun(I,D) -> (p_seq([p_not(fun 'valid_letter'/2), p_label('op', p_string(<<"+">>)), p_label('arg', p_choose([fun 'term'/2, fun 'string'/2]))]))(I,D) end, fun(Node, Idx) -> transform('required_op', Node, Idx) end).

'prohibited_op'(Input, Index) ->
  p(Input, Index, 'prohibited_op', fun(I,D) -> (p_seq([p_not(fun 'valid_letter'/2), p_label('op', p_string(<<"-">>)), p_label('arg', p_choose([fun 'field'/2, fun 'field_range'/2, fun 'term'/2, fun 'string'/2]))]))(I,D) end, fun(Node, Idx) -> transform('prohibited_op', Node, Idx) end).

'boost_op'(Input, Index) ->
  p(Input, Index, 'boost_op', fun(I,D) -> (p_seq([p_label('arg', p_choose([fun 'term'/2, fun 'string'/2])), p_string(<<"^">>), p_label('param', fun 'fuzzy_param'/2)]))(I,D) end, fun(Node, Idx) -> transform('boost_op', Node, Idx) end).

'fuzzy_op'(Input, Index) ->
  p(Input, Index, 'fuzzy_op', fun(I,D) -> (p_seq([p_label('arg', p_choose([fun 'term'/2, fun 'string'/2])), p_string(<<"~">>), p_label('param', p_optional(fun 'fuzzy_param'/2))]))(I,D) end, fun(Node, Idx) -> transform('fuzzy_op', Node, Idx) end).

'fuzzy_param'(Input, Index) ->
  p(Input, Index, 'fuzzy_param', fun(I,D) -> (p_seq([p_one_or_more(p_charclass(<<"[0-9]">>)), p_optional(p_seq([p_string(<<".">>), p_charclass(<<"[0-9]">>)]))]))(I,D) end, fun(Node, Idx) -> transform('fuzzy_param', Node, Idx) end).

'string'(Input, Index) ->
  p(Input, Index, 'string', fun(I,D) -> (p_seq([p_string(<<"\"">>), p_label('str', p_seq([fun 'term'/2, p_zero_or_more(p_seq([fun 'space'/2, fun 'term'/2]))])), p_string(<<"\"">>)]))(I,D) end, fun(Node, Idx) -> transform('string', Node, Idx) end).

'keyword'(Input, Index) ->
  p(Input, Index, 'keyword', fun(I,D) -> (p_choose([p_string(<<"AND">>), p_string(<<"OR">>), p_string(<<"NOT">>)]))(I,D) end, fun(Node, Idx) -> transform('keyword', Node, Idx) end).

'valid_letter'(Input, Index) ->
  p(Input, Index, 'valid_letter', fun(I,D) -> (p_seq([p_one_or_more(fun 'start_letter'/2), p_zero_or_more(p_choose([p_charclass(<<"[a-zA-Z0-9*?@_+.\/-]">>), p_seq([p_string(<<"\\">>), fun 'special_char'/2])]))]))(I,D) end, fun(Node, Idx) -> transform('valid_letter', Node, Idx) end).

'start_letter'(Input, Index) ->
  p(Input, Index, 'start_letter', fun(I,D) -> (p_choose([p_charclass(<<"[a-zA-Z0-9_.*\/]">>), p_seq([p_string(<<"\\">>), fun 'special_char'/2])]))(I,D) end, fun(Node, Idx) -> transform('start_letter', Node, Idx) end).

'space'(Input, Index) ->
  p(Input, Index, 'space', fun(I,D) -> (p_one_or_more(p_charclass(<<"[\s\t]">>)))(I,D) end, fun(Node, Idx) -> transform('space', Node, Idx) end).

'special_char'(Input, Index) ->
  p(Input, Index, 'special_char', fun(I,D) -> (p_choose([p_string(<<"[">>), p_string(<<"]">>), p_string(<<"\\">>), p_charclass(<<"[!(){}^\"~*?:]">>)]))(I,D) end, fun(Node, Idx) -> transform('special_char', Node, Idx) end).


transform(Symbol,Node,Index) -> lucene_sexp:transform(Symbol, Node, Index).

p(Inp, Index, Name, ParseFun) ->
  p(Inp, Index, Name, ParseFun, fun(N, _Idx) -> N end).

p(Inp, StartIndex, Name, ParseFun, TransformFun) ->
  case get_memo(StartIndex, Name) of      % See if the current reduction is memoized
    {ok, Memo} -> %Memo;                     % If it is, return the stored result
      Memo;
    _ ->                                        % If not, attempt to parse
      Result = case ParseFun(Inp, StartIndex) of
        {fail,_} = Failure ->                       % If it fails, memoize the failure
          Failure;
        {Match, InpRem, NewIndex} ->               % If it passes, transform and memoize the result.
          Transformed = TransformFun(Match, StartIndex),
          {Transformed, InpRem, NewIndex}
      end,
      memoize(StartIndex, Name, Result),
      Result
  end.

setup_memo() ->
  put({parse_memo_table, ?MODULE}, ets:new(?MODULE, [set])).

release_memo() ->
  ets:delete(memo_table_name()).

memoize(Index, Name, Result) ->
  Memo = case ets:lookup(memo_table_name(), Index) of
              [] -> [];
              [{Index, Plist}] -> Plist
         end,
  ets:insert(memo_table_name(), {Index, [{Name, Result}|Memo]}).

get_memo(Index, Name) ->
  case ets:lookup(memo_table_name(), Index) of
    [] -> {error, not_found};
    [{Index, Plist}] ->
      case proplists:lookup(Name, Plist) of
        {Name, Result}  -> {ok, Result};
        _  -> {error, not_found}
      end
    end.

memo_table_name() ->
    get({parse_memo_table, ?MODULE}).

p_eof() ->
  fun(<<>>, Index) -> {eof, [], Index};
     (_, Index) -> {fail, {expected, eof, Index}} end.

p_optional(P) ->
  fun(Input, Index) ->
      case P(Input, Index) of
        {fail,_} -> {[], Input, Index};
        {_, _, _} = Success -> Success
      end
  end.

p_not(P) ->
  fun(Input, Index)->
      case P(Input,Index) of
        {fail,_} ->
          {[], Input, Index};
        {Result, _, _} -> {fail, {expected, {no_match, Result},Index}}
      end
  end.

p_assert(P) ->
  fun(Input,Index) ->
      case P(Input,Index) of
        {fail,_} = Failure-> Failure;
        _ -> {[], Input, Index}
      end
  end.

p_and(P) ->
  p_seq(P).

p_seq(P) ->
  fun(Input, Index) ->
      p_all(P, Input, Index, [])
  end.

p_all([], Inp, Index, Accum ) -> {lists:reverse( Accum ), Inp, Index};
p_all([P|Parsers], Inp, Index, Accum) ->
  case P(Inp, Index) of
    {fail, _} = Failure -> Failure;
    {Result, InpRem, NewIndex} -> p_all(Parsers, InpRem, NewIndex, [Result|Accum])
  end.

p_choose(Parsers) ->
  fun(Input, Index) ->
      p_attempt(Parsers, Input, Index, none)
  end.

p_attempt([], _Input, _Index, Failure) -> Failure;
p_attempt([P|Parsers], Input, Index, FirstFailure)->
  case P(Input, Index) of
    {fail, _} = Failure ->
      case FirstFailure of
        none -> p_attempt(Parsers, Input, Index, Failure);
        _ -> p_attempt(Parsers, Input, Index, FirstFailure)
      end;
    Result -> Result
  end.

p_zero_or_more(P) ->
  fun(Input, Index) ->
      p_scan(P, Input, Index, [])
  end.

p_one_or_more(P) ->
  fun(Input, Index)->
      Result = p_scan(P, Input, Index, []),
      case Result of
        {[_|_], _, _} ->
          Result;
        _ ->
          {fail, {expected, Failure, _}} = P(Input,Index),
          {fail, {expected, {at_least_one, Failure}, Index}}
      end
  end.

p_label(Tag, P) ->
  fun(Input, Index) ->
      case P(Input, Index) of
        {fail,_} = Failure ->
           Failure;
        {Result, InpRem, NewIndex} ->
          {{Tag, Result}, InpRem, NewIndex}
      end
  end.

p_scan(_, [], Index, Accum) -> {lists:reverse( Accum ), [], Index};
p_scan(P, Inp, Index, Accum) ->
  case P(Inp, Index) of
    {fail,_} -> {lists:reverse(Accum), Inp, Index};
    {Result, InpRem, NewIndex} -> p_scan(P, InpRem, NewIndex, [Result | Accum])
  end.

p_string(S) when is_list(S) -> p_string(list_to_binary(S));
p_string(S) ->
    Length = erlang:byte_size(S),
    fun(Input, Index) ->
      try
          <<S:Length/binary, Rest/binary>> = Input,
          {S, Rest, p_advance_index(S, Index)}
      catch
          error:{badmatch,_} -> {fail, {expected, {string, S}, Index}}
      end
    end.

p_anything() ->
  fun(<<>>, Index) -> {fail, {expected, any_character, Index}};
     (Input, Index) when is_binary(Input) ->
          <<C/utf8, Rest/binary>> = Input,
          {<<C/utf8>>, Rest, p_advance_index(<<C/utf8>>, Index)}
  end.

p_charclass(Class) ->
    {ok, RE} = re:compile(Class, [unicode, dotall]),
    fun(Inp, Index) ->
            case re:run(Inp, RE, [anchored]) of
                {match, [{0, Length}|_]} ->
                    {Head, Tail} = erlang:split_binary(Inp, Length),
                    {Head, Tail, p_advance_index(Head, Index)};
                _ -> {fail, {expected, {character_class, binary_to_list(Class)}, Index}}
            end
    end.

line({{line,L},_}) -> L;
line(_) -> undefined.

column({_,{column,C}}) -> C;
column(_) -> undefined.

p_advance_index(MatchedInput, Index) when is_list(MatchedInput) orelse is_binary(MatchedInput)-> % strings
  lists:foldl(fun p_advance_index/2, Index, unicode:characters_to_list(MatchedInput));
p_advance_index(MatchedInput, Index) when is_integer(MatchedInput) -> % single characters
  {{line, Line}, {column, Col}} = Index,
  case MatchedInput of
    $\n -> {{line, Line+1}, {column, 1}};
    _ -> {{line, Line}, {column, Col+1}}
  end.
