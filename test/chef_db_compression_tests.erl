-module(chef_db_compression_tests).

-include_lib("eunit/include/eunit.hrl").

-define(DATA, <<"This is a test of the emergency broadcast system. "
                "This is only a test.">>).

-define(TYPES, [chef_node, chef_role, chef_data_bag_item]).

mysql_always_compressed_test_() ->
    [ {"mysql compressed for: " ++ atom_to_list(Type),
       fun() ->
               assert_compressed(chef_db_compression:compress(mysql, Type, ?DATA))
       end} || Type <- ?TYPES ].

pgsql_sometimes_compressed_test_() ->
    Tests = [{chef_node, fun assert_not_compressed/1},
             {chef_role, fun assert_compressed/1},
             {chef_data_bag_item, fun assert_compressed/1}],
    [ fun() ->
              Assert(chef_db_compression:compress(pgsql, Type, ?DATA))
      end || {Type, Assert} <- Tests ].

assert_compressed(CData) ->
    ?assert(size(CData) =/= size(?DATA)),
    ?assertNot(?DATA =:= CData),
    ?assertEqual(?DATA, chef_db_compression:decompress(CData)).

assert_not_compressed(CData) ->
    ?assertEqual(?DATA, CData),
    ?assertEqual(?DATA, chef_db_compression:decompress(CData)).
