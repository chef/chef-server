define_upgrade do

  if Partybus.config.bootstrap_server

    must_be_data_master

    sqitch = "sudo -u #{Partybus.config.database_unix_user} PATH=/opt/opscode/embedded/bin:/sbin:/usr/sbin:/bin:/usr/bin /opt/opscode/embedded/bin/sqitch"

    # Dependent OSS schema
    run_command("#{sqitch} deploy --to-target @1.0.4",
                :cwd => "/opt/opscode/embedded/service/opscode-erchef/schema/baseline")

    # Technically, only @2.1.0 contains the bcrypt schema change. However,
    # the later versions of omnibus are built with additional schema changes,
    # so we'll go ahead and upgrade thos as well.
    run_command("#{sqitch} deploy --to-target @2.2.3",
                :cwd => "/opt/opscode/embedded/service/opscode-erchef/schema")
  end
end

define_check do
  check_sqitch('@2.2.3', 'oc_erchef')
end
