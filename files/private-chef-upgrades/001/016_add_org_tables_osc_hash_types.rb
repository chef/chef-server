define_upgrade do

  if Partybus.config.bootstrap_server

    must_be_data_master

    start_service('postgresql')

    # run 2.4.0 migrations to update db - includes adding org association/user tables
    # and adding the OSC password hash types to the password_hash_type_enum
    run_command("make deploy",
                :cwd => "/opt/opscode/embedded/service/enterprise-chef-server-schema",
                :env => {"EC_TARGET" => "@2.4.0", "OSC_TARGET" => "@1.0.4", "DB_USER" => "opscode-pgsql"}
                )

    stop_service('postgresql')
  end
end
