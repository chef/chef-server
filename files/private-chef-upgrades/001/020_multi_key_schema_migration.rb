define_upgrade do
  if Partybus.config.bootstrap_server
    must_be_data_master
    run_command("make deploy",
                :cwd => "/opt/opscode/embedded/service/enterprise-chef-server-schema",
                :env => {"EC_TARGET" => "@2.5.2", "OSC_TARGET" => "@1.0.4", "DB_USER" => Partybus.config.database_unix_user}
                )
    run_command("sudo -u #{Partybus.config.database_unix_user} /opt/opscode/embedded/bin/psql opscode_chef -c 'select migrate_keys();'")
  end
end
