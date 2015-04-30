define_upgrade do
  if Partybus.config.bootstrap_server

    must_be_data_master

    # Make sure API is down
    stop_services(["nginx", "opscode-erchef"])

    # migrate global containers
    force_restart_service("opscode-chef-mover")

    log "Migrating global containers..."
    run_command("/opt/opscode/embedded/bin/escript " \
                "/opt/opscode/embedded/service/opscode-chef-mover/scripts/migrate " \
                "mover_global_containers_migration_callback normal mover_transient_queue_batch_migrator")

    stop_service("opscode-chef-mover")
  end
end
