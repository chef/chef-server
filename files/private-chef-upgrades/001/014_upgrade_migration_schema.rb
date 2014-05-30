define_upgrade do

  if Partybus.config.bootstrap_server

    must_be_data_master

    # run 2.2.4 migration which includes schema upgrade for migration state
    run_command("make deploy",
                :cwd => "/opt/opscode/embedded/service/enterprise-chef-server-schema",
                :env => {"EC_TARGET" => "@2.2.4", "OSC_TARGET" => "@1.0.4", "DB_USER" => "opscode-pgsql"}
                )

  end
end
