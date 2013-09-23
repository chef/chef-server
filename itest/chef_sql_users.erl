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
  Results = [chef_sql:create_user(User) || User <- Users ],
  ?assertEqual(Expected, Results).


fetch_user_data() ->
  Expected = make_user(<<"user03">>),
  Username = Expected#chef_user.username,
  %% Make sure client create succeeds
  ?assertEqual({ok, 1}, chef_sql:create_user(Expected)),
  {ok, Result} = chef_sql:fetch_user(Username),
  ?assertEqual(Expected, Result).

fetch_user_list() ->
  Users = [make_user(<<"user04">>), make_user(<<"user05">>)],
  CreatedResults = lists:duplicate(length(Users), {ok, 1}),
  Created = [chef_sql:create_user(User) || User <- Users ],
  ?assertEqual(CreatedResults, Created),
  Results = chef_sql:fetch_users(),
  Expected = {ok, [ User#chef_user.username || User <- Users ]},
  ?assertEqual(Expected, Results).

delete_user_data() ->
  User = make_user(<<"user06">>),
  Username = User#chef_user.username,
  ?assertEqual({ok, 1}, chef_sql:create_user(User)),
  Result = chef_sql:delete_user(Username),
  ?assertEqual({ok, 1}, Result),
  Result1 = chef_sql:fetch_user(Username),
  ?assertEqual({ok, not_found}, Result1).

update_user_data() ->
  User = make_user(<<"user07">>),
  Username = User#chef_user.username,
  ?assertEqual({ok, 1}, chef_sql:create_user(User)),

  % Is the user really a non-admin?
  {ok, CreatedUser} = chef_sql:fetch_user(Username),
  ?assertEqual(false, CreatedUser#chef_user.admin),

  % Make user an admin
  UpdatedUserData = User#chef_user{ admin = true },
  Result = chef_sql:update_user(UpdatedUserData),
  ?assertEqual({ok, 1}, Result),

  % Is the user really an admin?
  {ok, PersistedUser} = chef_sql:fetch_user(Username),
  ?assertEqual(true, PersistedUser#chef_user.admin),

  %% Cleanup admin user so count_admin_users() tests will work
  chef_sql:delete_user(Username).

count_admin_users() ->
  User = make_admin_user(<<"user08">>),
  ?assertEqual({ok, 1}, chef_sql:create_user(User)),
  ?assertEqual({ok, 1}, chef_sql:count_user_admins()),
  User2 = make_admin_user(<<"user09">>),
  ?assertEqual({ok, 1}, chef_sql:create_user(User2)),
  ?assertEqual({ok, 2}, chef_sql:count_user_admins()).

