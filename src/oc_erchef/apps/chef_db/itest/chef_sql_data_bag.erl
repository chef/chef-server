-module(chef_sql_data_bag).

-include_lib("eunit/include/eunit.hrl").
-include("chef_db.hrl").
-include("chef_types.hrl").

%%%======================================================================
%%% DATA BAGS
%%%======================================================================
make_data_bag(Prefix) ->
    Id = chef_test_suite_helper:make_id(Prefix),
    AzId = chef_test_suite_helper:make_az_id(Prefix),
    OrgId = chef_test_suite_helper:the_org_id(),
    Name = <<"data_bag_", Prefix/binary>>,
    #chef_data_bag{server_api_version = ?API_MIN_VER, id = Id,
                   authz_id = AzId, org_id = OrgId,
                   name = Name,
                   last_updated_by = chef_test_suite_helper:actor_id(),
                   created_at = {datetime, {{2011,10,1},{16,47,46}}},
                   updated_at = {datetime, {{2011,10,1},{16,47,46}}} }.

data_bags() ->
    [make_data_bag(<<"01">>),
     make_data_bag(<<"02">>),
     make_data_bag(<<"03">>),
     (make_data_bag(<<"04">>))#chef_data_bag{org_id = chef_test_suite_helper:other_org_id()}].

insert_data_bag_data() ->
    Expected = lists:duplicate(length(data_bags()), {ok, 1}),
    Results = [itest_util:create_record(Bag) || Bag <- data_bags() ],
    ?assertEqual(Expected, Results).

fetch_data_bags() ->
    DBS = data_bags(),
    Expected = [ Db#chef_data_bag.name || Db <- DBS, Db#chef_data_bag.org_id =:= chef_test_suite_helper:the_org_id() ],
    Results = itest_util:list_records(hd(DBS)),
    ?assertEqual(Expected, Results).

fetch_data_bag() ->
    Db = hd(data_bags()),
    {ok, Got} = itest_util:fetch_record(Db),
    ?assertEqual(Db, Got).

delete_data_bag() ->
    First = hd(data_bags()),
    ?assertEqual({ok, 1}, itest_util:delete_record(First)),
    %% verify data is gone
    ?assertEqual({ok, not_found}, itest_util:fetch_record(First)),
    %% deleting a non existing data bag is OK
    ?assertEqual({ok, not_found}, itest_util:delete_record(First)).
