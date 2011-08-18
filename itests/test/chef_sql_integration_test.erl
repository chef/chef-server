-module(chef_sql_integration_test).

-include_lib("eunit/include/eunit.hrl").
-include_lib("chef_common/include/chef_sql.hrl").

fetch_user_test_() ->
    {setup,
     fun() ->
             application:set_env(chef_common, mysql_host, "localhost"),
             application:set_env(chef_common, mysql_port, 3306),
             application:set_env(chef_common, mysql_user, "dev"),
             application:set_env(chef_common, mysql_pass, "opensesame"),
             application:set_env(chef_common, mysql_db_name, "opscode_chef"),
             application:set_env(chef_common, mysql_pool_size, 5),
             application:start(crypto),
             application:start(emysql),
             Got = chef_sql:connect(),
             ?debugVal(Got),
             ok
     end,
     fun(_) -> cleanup end,
     [
      {"fetch_user found",
       fun() ->
               Got = chef_sql:fetch_user("clownco-org-admin"),
               ?debugVal(Got)
       end}
     ]}.

