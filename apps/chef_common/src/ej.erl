% License: Apache License, Version 2.0
%
% Licensed under the Apache License, Version 2.0 (the "License");
% you may not use this file except in compliance with the License.
% You may obtain a copy of the License at
% 
%     http://www.apache.org/licenses/LICENSE-2.0
% 
% Unless required by applicable law or agreed to in writing, software
% distributed under the License is distributed on an "AS IS" BASIS,
% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
% See the License for the specific language governing permissions and
% limitations under the License.
%
%% @author Seth Falcon <seth@userprimary.net>
%% @copyright Copyright 2011 Seth Falcon
%%
%% @doc Tools for working with Erlang terms representing JSON.
%%
%% The ej module is intended to make it easy to work with the Erlang
%% structure used by `mochijson2' to represent JSON.  You can use
%% `ej:get' to walk an object and return a particular value, or
%% `ej:set' to update a value.
%%
%% @end
-module(ej).
-author('Seth Falcon <seth@userprimary.net').
-export([
         get/2,
         set/3,
         delete/2
         ]).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-type json_string() :: binary().
-type json_null()   :: null.
-type json_number() :: integer() | float().
-type json_array()  :: [json_term()].
-type json_plist()  :: [{json_string(), json_term()}].
-type json_object() :: {struct, json_plist()}.
-type json_term()   :: json_string() | json_number() | json_array() |
                       json_object() | json_null().

-type key_type()    :: binary() | integer() | first | last | new.
-type key_tuple()   :: {key_type()}.

%% @doc Extract a value from `Obj'
%%
%% `Keys' is a tuple specifying a path into the JSON structure.  Each
%% string or binary element of `Keys' will act like a Javascript
%% property lookup.  Elements of JSON arrays can be accessed by
%% including an integer as an element of `Keys'.  In addition, the
%% atoms `` 'first' '' and `` 'last' '' can be used to access the
%% first and last elements of a list, respectively.
%%
-spec(get(key_tuple(), json_object()) -> json_term()).

get(Keys, Obj) when is_tuple(Keys) ->
   get0(tuple_to_list(Keys), Obj).

get0([Key | Rest], Obj) ->
    case get_value(Key, Obj) of
        undefined -> undefined;
        AValue -> get0(Rest, AValue)
    end;
get0([], Value) ->
    Value.


get_value(Key, Obj) when is_list(Key) ->
    get_value(iolist_to_binary(Key), Obj);
get_value(Key, {struct, L}) when is_binary(Key) ->
    get_value(Key, L);
get_value(Key, {L}) when is_binary(Key) -> % alt form
    get_value(Key, L);
get_value(Key, PL=[{_, _}|_T]) when is_binary(Key) ->
    proplists:get_value(Key, PL);    
get_value(Key, [_H|_T]) when is_binary(Key) ->
    undefined;
get_value(first, [H|_T]) ->
    H;
get_value(last, List=[_H|_T]) ->
    lists:last(List);
get_value(Index, List=[_H|_T]) when is_integer(Index) ->
    lists:nth(Index, List);
get_value(Index, Obj) ->
    erlang:error({index_for_non_list, {Index, Obj}}).

as_binary(Key) when is_binary(Key) ->
    Key;
as_binary(Key) when is_list(Key) ->
    iolist_to_binary(Key);
as_binary(Key) when is_integer(Key) orelse is_atom(Key) ->
    Key.

%% @doc Set a value in `Obj'
%%
%% Replaces the value at the path specified by `Keys' with `Value' and
%% returns the new structure.  If `Value' is the atom `EJ_DELETE',
%% then the path specified by `Keys' is removed (but see `delete/2').
%%
-spec(set(key_tuple(), json_object(), json_term()) -> json_term()).
set(Keys, Obj, Value) when is_tuple(Keys) ->
    set0([ as_binary(X) || X <- tuple_to_list(Keys) ], Obj, Value).

set0([], _, Value) ->
    Value;
set0([Key | Rest], {struct, P}, Value)
  when is_binary(Key) orelse Key == 'EJ_DELETE' ->
    case {get_value(Key, P), length(Rest), Value} of
        {undefined, Len, _} when Len > 0 ->
            erlang:error({no_path, Key});
        {_, Len, 'EJ_DELETE'} when Len == 0 ->
            {struct, lists:keydelete(Key, 1, P)};
        {Downstream, _, _} ->
            {struct, lists:keystore(Key, 1, P,
                                    {Key, set0(Rest, Downstream, Value)})}
    end;
set0([Key | Rest], {P}, Value) % clean this up? alt form
  when is_binary(Key) orelse Key == 'EJ_DELETE' ->
    case {get_value(Key, P), length(Rest), Value} of
        {undefined, Len, _} when Len > 0 ->
            erlang:error({no_path, Key});
        {_, Len, 'EJ_DELETE'} when Len == 0 ->
            {lists:keydelete(Key, 1, P)};
        {Downstream, _, _} ->
            {lists:keystore(Key, 1, P,
                            {Key, set0(Rest, Downstream, Value)})}
    end;
set0([new | []], P, Value) when is_list(P) ->
    [Value|P];
set0([Idx | Rest], P, Value)
  when is_integer(Idx) orelse is_atom(Idx); is_list(P) ->
    case {get_value(Idx, P), length(Rest), Value} of
        {undefined, Len, _} when Len > 0 ->
            erlang:error({no_path, Idx});
        {_, Len, 'EJ_DELETE'} when Len == 0 ->
            set_nth(Idx, P, 'EJ_DELETE');
        {Downstream, _, _} ->
            set_nth(Idx, P, set0(Rest, Downstream, Value))
end.    

set_nth(first, [_H|T], 'EJ_DELETE') ->
    T;
set_nth(first, [_H|T], V) ->
    [V|T];
set_nth(last, L, 'EJ_DELETE') ->
    [_H|T] = lists:reverse(L),
    lists:reverse(T);
set_nth(last, L, V) ->
    [_H|T] = lists:reverse(L),
    lists:reverse([V|T]);
set_nth(N, L, 'EJ_DELETE') ->
    {L1, [_H|L2]} = lists:split(N - 1, L),
    lists:concat([L1, L2]);
set_nth(N, L, V) ->
    {L1, [_H|L2]} = lists:split(N - 1, L),
    lists:concat([L1, [V|L2]]).


% TODO: support setting list elements as well as a means to add new
% elements to a list.

%% @doc Remove the item specified by `Keys'.
-spec(delete(key_tuple(), json_object()) -> json_object()).

delete(Keys, Obj) when is_tuple(Keys) ->
    set0([ as_binary(X) || X <- tuple_to_list(Keys) ], Obj, 'EJ_DELETE').

-ifdef(TEST).

ej_test_() ->
{setup,
 fun() ->
         {ok, [Widget]} = file:consult("../test/widget.terms"),
         {ok, [Glossary]} = file:consult("../test/glossary.terms"),
         {ok, [Menu]} = file:consult("../test/menu.terms"),
         ObjList = {struct, [{<<"objects">>,
                              [ {struct, [{<<"id">>, I}]} ||
                                  I <- lists:seq(1, 5) ]}]},
         {Widget, Glossary, Menu, ObjList}
 end,
 fun({Widget, Glossary, Menu, ObjList}) ->
         [{"ej:get",
           [
            ?_assertMatch({struct, [{_, _}|_]}, ej:get({"widget"}, Widget)),
            ?_assertEqual(<<"1">>, ej:get({"widget", "version"}, Widget)),
            ?_assertEqual(250, ej:get({"widget", "image", "hOffset"}, Widget)),
            ?_assertEqual([1,2,3,4,5], ej:get({"widget", "values"}, Widget)),
            ?_assertEqual(2, ej:get({"widget", "values", 2}, Widget)),
            ?_assertEqual(4, ej:get({"widget", "values", 4}, Widget)),
            ?_assertEqual(1, ej:get({"widget", "values", first}, Widget)),
            ?_assertEqual(5, ej:get({"widget", "values", last}, Widget)),
            ?_assertEqual({struct, [{<<"id">>, 5}]},
                          ej:get({<<"objects">>, last}, ObjList)),
            ?_assertEqual({struct, [{<<"id">>, 1}]},
                          ej:get({<<"objects">>, first}, ObjList)),
            ?_assertEqual(undefined, ej:get({"fizzle"}, Widget)),
            ?_assertEqual(undefined, ej:get({"widget", "fizzle"}, Widget)),
            ?_assertEqual(undefined,
                          ej:get({"widget", "values", "fizzle"},Widget)),

            ?_assertEqual(<<"SGML">>,
                          ej:get({"glossary", "GlossDiv", "GlossList",
                                  "GlossEntry", "Acronym"}, Glossary)),

            ?_assertEqual(undefined,
                          ej:get({"glossary", "GlossDiv", "GlossList",
                                  "GlossEntry", "fizzle"}, Glossary)),

            ?_assertException(error, {index_for_non_list, _},
                              ej:get({"glossary", "GlossDiv", "GlossList",
                                      "GlossEntry", 1}, Glossary)),

            ?_assertException(error, {index_for_non_list, _},
                              ej:get({"glossary", "title", 1}, Glossary))]},

          {"ej:set, replacing existing value",
           fun() ->
                   Path = {"widget", "window", "name"},
                   CurrentValue = ej:get(Path, Widget),
                   NewValue = <<"bob">>,
                   ?assert(NewValue /= CurrentValue),
                   Widget1 = ej:set(Path, Widget, NewValue),
                   ?assertEqual(NewValue, ej:get(Path, Widget1)),
                   % make sure the structure hasn't been disturbed
                   Widget2 = ej:set(Path, Widget1, <<"main_window">>),
                   ?assertEqual(Widget, Widget2)
           end},

          {"ej:set, creating new value",
           fun() ->
                   Path = {"widget", "image", "newOffset"},
                   Value = <<"YYY">>,
                   ?assertEqual(undefined, ej:get(Path, Widget)),
                   Widget1 = ej:set(Path, Widget, Value),
                   ?assertEqual(Value, ej:get(Path, Widget1))
           end},

          {"ej:set, missing intermediate path",
           fun() ->
                   Path = {"widget", "middle", "nOffset"},
                   Value = <<"YYY">>,
                   ?assertEqual(undefined, ej:get(Path, Widget)),
                   ?assertException(error, {no_path, _},
                                    ej:set(Path, Widget, Value))
           end},

          {"ej:set top-level",
           fun() ->
                   OrigVal = ej:get({"widget", "version"}, Widget),
                   NewVal = <<"2">>,
                   NewWidget = ej:set({"widget", "version"}, Widget, NewVal),
                   ?assertEqual(NewVal, ej:get({"widget", "version"}, NewWidget)),
                   Reset = ej:set({"widget", "version"}, NewWidget, OrigVal),
                   ?assertEqual(Widget, Reset)
           end},

          {"ej:set nested",
           fun() ->
                   NewVal = <<"JSON">>,
                   Path = {"glossary", "GlossDiv", "GlossList", "GlossEntry",
                           "ID"},
                   Unchanged = ej:get({"glossary", "GlossDiv", "GlossList",
                                       "GlossEntry", "SortAs"}, Glossary),
                   Glossary1 = ej:set(Path, Glossary, NewVal),
                   ?assertEqual(NewVal, ej:get(Path, Glossary1)),
                   ?assertEqual(Unchanged, ej:get({"glossary", "GlossDiv",
                                                   "GlossList", "GlossEntry",
                                                   "SortAs"}, Glossary1)),
                   Reset = ej:set(Path, Glossary1, <<"SGML">>),
                   ?assertEqual(Glossary, Reset)
           end},

          {"ej:set list element",
           fun() ->
                   Orig = ej:get({"menu", "popup", "menuitem", 2}, Menu),
                   New = ej:set({"onclick"}, Orig, <<"OpenFile()">>),
                   Menu1 = ej:set({"menu", "popup", "menuitem", 2}, Menu, New),
                   ?assertEqual(New,
                                ej:get({"menu", "popup", "menuitem", 2}, Menu1)),
                   Reset = ej:set({"menu", "popup", "menuitem", 2}, Menu1, Orig),
                   ?assertEqual(Menu, Reset)
           end},

          {"ej:set list element path",
           fun() ->
                   Path = {"menu", "popup", "menuitem", 2, "onclick"},
                   Orig = ej:get(Path, Menu),
                   New = <<"OpenFile()">>,
                   Menu1 = ej:set(Path, Menu, New),
                   ?assertEqual(New, ej:get(Path, Menu1)),
                   Reset = ej:set(Path, Menu1, Orig),
                   ?assertEqual(Menu, Reset)
           end},

          {"ej:set list element path first, last",
           fun() ->
                   FPath = {"menu", "popup", "menuitem", first, "value"},
                   LPath = {"menu", "popup", "menuitem", last, "value"},
                   FMenu = ej:set(FPath, Menu, <<"create">>),
                   LMenu = ej:set(LPath, FMenu, <<"kill">>),
                   ?assertEqual(<<"create">>, ej:get(FPath, FMenu)),
                   ?assertEqual(<<"create">>, ej:get(FPath, LMenu)),
                   ?assertEqual(<<"kill">>, ej:get(LPath, LMenu))
           end},

          {"ej:set new list element",
           fun() ->
                   Path = {"menu", "popup", "menuitem", new},
                   Path1 = {"menu", "popup", "menuitem", first},
                   Menu1 = ej:set(Path, Menu, <<"first-item">>),
                   ?assertEqual(<<"first-item">>, ej:get(Path1, Menu1)),
                   List = ej:get({"menu", "popup", "menuitem"}, Menu1),
                   ?assertEqual(4, length(List))
           end},

          {"ej:remove",
           fun() ->
                   Path = {"glossary", "GlossDiv", "GlossList", "GlossEntry", "Abbrev"},
                   Orig = ej:get(Path, Glossary),
                   ?assert(undefined /= Orig),
                   Glossary1 = ej:delete(Path, Glossary),
                   ?assertEqual(undefined, ej:get(Path, Glossary1)),
                   % verify some structure
                   ?assertEqual(<<"SGML">>, ej:get({"glossary", "GlossDiv",
                                                    "GlossList", "GlossEntry",
                                                    "Acronym"}, Glossary1)),
                   ?assertEqual(<<"S">>, ej:get({"glossary", "GlossDiv",
                                                 "title"}, Glossary1))
           end}
         ]
 end
}.

-endif.

