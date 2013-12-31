define_upgrade do

  if Partybus.config.bootstrap_server

    must_be_data_master

    # Dependent OSS schema
    run_command("sqitch --db-user opscode-pgsql deploy --to-target @1.0.4",
                :cwd => "/opt/opscode/embedded/service/enterprise-chef-server-schema/deps/chef-server-schema")

    # Technically, only @2.1.0 contains the bcrypt schema change. However,
    # the later versions of omnibus are built with additional schema changes,
    # so we'll go ahead and upgrade thos as well.
    run_command("sqitch --db-user opscode-pgsql deploy --to-target @2.2.3",
                :cwd => "/opt/opscode/embedded/service/enterprise-chef-server-schema")


    ## Add chef-mover upgrade here
  end
end
