define_upgrade do
  if Partybus.config.bootstrap_server
    must_be_data_master
    run_sqitch("@2.5.3", "@1.0.4")
    run_command("sudo -u #{Partybus.config.database_unix_user} /opt/opscode/embedded/bin/psql opscode_chef -c 'select migrate_keys();'")
  end
end
