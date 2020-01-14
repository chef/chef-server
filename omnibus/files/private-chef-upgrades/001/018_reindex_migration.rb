define_upgrade do
  if Partybus.config.bootstrap_server

    must_be_data_master

    # Make sure API is down
    stop_services(["nginx", "opscode-erchef"])

    force_restart_service("opscode-chef-mover")

    log "Migrating indexed search data..."
    run_command("/opt/opscode/embedded/bin/escript " \
                "/opt/opscode/embedded/service/opscode-chef-mover/scripts/migrate " \
                "mover_reindex_migration_callback normal")

    stop_services(["opscode-chef-mover"])

  end
end

