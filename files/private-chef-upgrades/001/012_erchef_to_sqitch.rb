define_upgrade do

  if Partybus.config.bootstrap_server

    must_be_data_master

    sqitch = "sudo -u #{Partybus.config.database_unix_user} PATH=/opt/opscode/embedded/bin:/sbin:/usr/sbin:/bin:/usr/bin /opt/opscode/embedded/bin/sqitch"

    # This command ensures Sqitch has all the metadata for changes to
    # the core schema up to version 1.0.1.  All these changes are
    # already in the schema.
    run_command("#{sqitch} deploy --log-only --to-target @1.0.1",
                :cwd => "/opt/opscode/embedded/service/opscode-erchef/schema/baseline")

    # There was a small change to the core schema that didn't make it
    # into the previous incarnation of the Enterprise schema; this is a
    # "real" deploy to pick up those changes.  This also removes the
    # Sequel version metadata table.
    run_command("#{sqitch} deploy --to-target @1.0.3",
                :cwd => "/opt/opscode/embedded/service/opscode-erchef/schema/baseline")

    # Finally, this loads up the change metadata for the Enterprise
    # schema.  There are no changes to it that are not already in the
    # existing schema, so it is '--log-only'.
    run_command("#{sqitch} deploy --log-only --to-target @2.0.0",
                :cwd => "/opt/opscode/embedded/service/opscode-erchef/schema")

  end
end
