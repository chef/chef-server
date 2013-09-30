-module(chef_sql_users).

-compile([export_all]).

-include_lib("eunit/include/eunit.hrl").
-include_lib("chef_db/include/chef_db.hrl").
-include_lib("chef_objects/include/chef_types.hrl").

make_user(Prefix) ->
  AzId = itest_util:make_az_id(Prefix),
  chef_user_record(AzId, false).

make_admin_user(Prefix) ->
  AzId = itest_util:make_az_id(Prefix),
  chef_user_record(AzId, true).

chef_user_record(AzId, Admin) ->
  #chef_user{
    id = AzId,
    authz_id = AzId,
    username = AzId,
    email = AzId,
    public_key =
    <<"MIIBIjANBgkqhkiG9w0BAQEFAAOCAQ8AMIIBCgKCAQEAwxOFcrbsV7bEbqzOvW5u"
	      "W5lyB23qsenlUdIGyRttqzGEaki01s7X+PpYy4BLfmVVmA6A6FCbL38CzzTUFX1a"
	      "p6LYQR2Pb1tYjBjZZMUiVnjEgl12Zd1JF8dsPMj2BgPggx5GaGLvCOsajZ0YCDgW"
	      "WkoO/HAEbztFIx2jdSCyD0ZH0ep4fSGDjmkN+5XurS0dBH8J5qPeJjriA/s/RzUb"
	      "ULjr3gvfg49onHxr/kTKbhc78GBOfKSH1ftECCoWnidadW7/lfKbAZ3xiSjLsIxS"
	      "KxavHMeCuSgyReDZpsFOn2Saie26jvLxWrGyn870yIh36wMvCvWKwUQPnluSnstJ"
	      "xwIDAQAB">>,
    hashed_password = <<"secretHaxorz">>,
    salt = <<"kosher">>,
    hash_type = <<"bcrypt">>,
    last_updated_by = itest_util:actor_id(),
    created_at = {datetime, {{2011,10,1},{16,47,46}}},
    updated_at = {datetime, {{2011,10,1},{16,47,46}}},
    external_authentication_uid = <<"an open id of some kind">>,
    recovery_authentication_enabled = false,
    admin = Admin
  }.

%%%======================================================================
%%% USERS
%%%======================================================================

insert_user_data() ->
  Users = [make_user(<<"user01">>), make_user(<<"user02">>)],
  Expected = lists:duplicate(length(Users), {ok, 1}),
  Results = [create_record(User) || User <- Users],
  ?assertEqual(Expected, Results).



fetch_user_data() ->
  Expected = make_user(<<"user03">>),
  %% Make sure client create succeeds
  ?assertEqual({ok, 1}, create_record(Expected)),
  {ok, Result} = fetch_record(Expected),
  ?assertEqual(Expected, Result).

fetch_user_list() ->
  Users = [make_user(<<"user04">>), make_user(<<"user05">>)],
  CreatedResults = lists:duplicate(length(Users), {ok, 1}),
  Created = [create_record(User) || User <- Users ],
  ?assertEqual(CreatedResults, Created),
  Results = list_records(#chef_user{}),
  Expected = [ User#chef_user.username || User <- Users ],
  ?assertEqual(Expected, Results).

delete_user_data() ->
  User = make_user(<<"user06">>),
  ?assertEqual({ok, 1}, create_record(User)),
  Result = delete_record(User),
  ?assertEqual({ok, 1}, Result),
  {ok, Result1} = fetch_record(User),
  ?assertEqual(not_found, Result1).

update_user_data() ->
  User = make_user(<<"user07">>),
  ?assertEqual({ok, 1}, create_record(User)),

  % Is the user really a non-admin?
  {ok, CreatedUser} = fetch_record(User),
  ?assertEqual(false, CreatedUser#chef_user.admin),

  % Make user an admin
  UpdatedUserData = User#chef_user{ admin = true },
  Result = update_record(UpdatedUserData),
  ?assertEqual({ok, 1}, Result),

  % Is the user really an admin?
  {ok, PersistedUser} = fetch_record(User),
  ?assertEqual(true, PersistedUser#chef_user.admin),

  %% Cleanup admin user so count_admin_users() tests will work
  chef_sql:delete_object(chef_object:delete_query(UpdatedUserData), chef_object:id(UpdatedUserData)).

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

count_admin_users() ->
  User = make_admin_user(<<"user08">>),
  ?assertEqual({ok, 1}, create_record(User)),
  ?assertEqual({ok, 1}, chef_sql:count_user_admins()),
  User2 = make_admin_user(<<"user09">>),
  ?assertEqual({ok, 1}, create_record(User2)),
  ?assertEqual({ok, 2}, chef_sql:count_user_admins()).

delete_record(Record) ->
    chef_sql:delete_object(chef_object:delete_query(Record), chef_object:id(Record)).


list_records(Record) ->
    chef_sql:fetch_object_names(Record).
