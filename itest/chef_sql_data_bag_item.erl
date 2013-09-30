-module(chef_sql_data_bag_item).

-compile([export_all]).

-include_lib("eunit/include/eunit.hrl").
-include_lib("chef_db/include/chef_db.hrl").
-include_lib("chef_objects/include/chef_types.hrl").

%%%======================================================================
%%% DATA BAG ITEMS
%%%======================================================================
make_data_bag_item(Prefix, BagName) ->
    Id = itest_util:make_id(Prefix),
    Name = <<"item_", Prefix/binary>>,
    #chef_data_bag_item{id= Id, org_id= itest_util:the_org_id(), item_name= Name, data_bag_name= BagName,
                        last_updated_by= itest_util:actor_id(),
                        created_at= {datetime, {{2011,10,1},{16,47,46}}},
                        updated_at= {datetime, {{2011,10,1},{16,47,46}}},
                        serialized_object= Prefix }.

data_bag_items() ->
    [
     %% NOTE: we delete data_bag_01 as part of the delete test, so put the items into other bags
     make_data_bag_item(<<"101">>, <<"data_bag_02">>),
     make_data_bag_item(<<"102">>, <<"data_bag_02">>),
     make_data_bag_item(<<"103">>, <<"data_bag_02">>)
    ].

insert_data_bag_item_data() ->
    Expected = lists:duplicate(length(data_bag_items()), {ok, 1}),
    Results = [create_record(Bag) || Bag <- data_bag_items() ],
    ?assertEqual(Expected, Results).

fetch_data_bag_items() ->
    DBS = data_bag_items(),
    Expected = [ Db#chef_data_bag_item.item_name || Db <- DBS, Db#chef_data_bag_item.org_id =:= itest_util:the_org_id(),
                                                    Db#chef_data_bag_item.data_bag_name =:= <<"data_bag_02">> ],
    Results = list_records(hd(DBS)),
    ?assertEqual(Expected, Results).

fetch_data_bag_item()->
    Item = hd(data_bag_items()),

    {ok, Got} = fetch_record(Item),

    ?assertEqual(Item, Got).

fetch_data_bag_item_ids() ->
    Expected = [ Db#chef_data_bag_item.id ||
                   Db <- data_bag_items(),
                   Db#chef_data_bag_item.org_id =:= itest_util:the_org_id(),
                   Db#chef_data_bag_item.data_bag_name =:= <<"data_bag_02">>],
    {ok, Results} = chef_sql:fetch_data_bag_item_ids(itest_util:the_org_id(), <<"data_bag_02">>),
    ?assertEqual(Expected,Results).

bulk_get_data_bag_items()-> ok.

update_data_bag_item()->
    [Old | _T] = [ Db ||
                     Db <- data_bag_items(),
                     Db#chef_data_bag_item.org_id =:= itest_util:the_org_id(),
                     Db#chef_data_bag_item.data_bag_name =:= <<"data_bag_02">>],
    NewData = <<"new object">>,
    New = Old#chef_data_bag_item{serialized_object= NewData},
    {ok, UResults} = update_record(New),
    ?assertEqual(1, UResults),
    {ok, FResults} = fetch_record(Old),
    ?assertEqual(NewData,
                 (FResults#chef_data_bag_item.serialized_object)).


delete_data_bag_item()->
    Item = hd(data_bag_items()),
    {ok, DResults} = delete_record(Item),
    ?assertEqual(1, DResults),
    {ok, FResults} = fetch_record(Item),
    ?assertEqual(not_found, FResults).

%% Generic functions

update_record(Record) ->
  chef_sql:do_update(chef_object:update_query(Record), chef_object:fields_for_update(Record)).

fetch_record(Record) ->
    chef_sql:fetch_object(
      chef_object:fields_for_fetch(Record),
      element(1, Record),
      chef_object:find_query(Record),
      chef_object:record_fields(Record)
          ).

create_record(Record) -> chef_sql:create_object(chef_object:create_query(Record), Record).

delete_record(Record) ->
    chef_sql:delete_object(chef_object:delete_query(Record), chef_object:id(Record)).


list_records(Record) ->
    chef_sql:fetch_object_names(Record).
