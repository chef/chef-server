-module(ej_alt_test).

-include_lib("eunit/include/eunit.hrl").

ej_alt_test_() ->
{setup,
 fun() ->
         {ok, [Widget]} = file:consult("../test/widget.alt_terms"),
         {ok, [Glossary]} = file:consult("../test/glossary.alt_terms"),
         {ok, [Menu]} = file:consult("../test/menu.alt_terms"),
         ObjList = {[{<<"objects">>,
                      [ {[{<<"id">>, I}]} ||
                          I <- lists:seq(1, 5) ]}]},
         {Widget, Glossary, Menu, ObjList}
 end,
 fun({Widget, Glossary, Menu, ObjList}) ->
         [{"ej:get",
           [
            ?_assertMatch({[{_, _}|_]}, ej:get({"widget"}, Widget)),
            ?_assertEqual(<<"1">>, ej:get({"widget", "version"}, Widget)),
            ?_assertEqual(250, ej:get({"widget", "image", "hOffset"}, Widget)),
            ?_assertEqual([1,2,3,4,5], ej:get({"widget", "values"}, Widget)),
            ?_assertEqual(2, ej:get({"widget", "values", 2}, Widget)),
            ?_assertEqual(4, ej:get({"widget", "values", 4}, Widget)),
            ?_assertEqual(1, ej:get({"widget", "values", first}, Widget)),
            ?_assertEqual(5, ej:get({"widget", "values", last}, Widget)),
            ?_assertEqual({[{<<"id">>, 5}]},
                          ej:get({<<"objects">>, last}, ObjList)),
            ?_assertEqual({[{<<"id">>, 1}]},
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

