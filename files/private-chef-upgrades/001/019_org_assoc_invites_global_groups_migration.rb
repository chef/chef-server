define_upgrade do
  if Partybus.config.bootstrap_server

    must_be_data_master

    # Make sure API is down
    stop_services(["nginx", "opscode-erchef"])

    force_restart_service("opscode-chef-mover")

    log "Migrating organizations..."
    run_command("/opt/opscode/embedded/bin/escript " \
                "/opt/opscode/embedded/service/opscode-chef-mover/scripts/migrate " \
                "mover_org_migration_callback normal mover_transient_queue_batch_migrator")

    log "Migrating organization user associations..."
    run_command("/opt/opscode/embedded/bin/escript " \
                "/opt/opscode/embedded/service/opscode-chef-mover/scripts/migrate " \
                "mover_org_user_association_migration_callback normal mover_transient_queue_batch_migrator")

    log "Migrating organization association requests..."
    run_command("/opt/opscode/embedded/bin/escript " \
                "/opt/opscode/embedded/service/opscode-chef-mover/scripts/migrate " \
                "mover_org_user_invites_migration_callback normal mover_transient_queue_batch_migrator")

    log "Migrating global groups..."
    run_command("/opt/opscode/embedded/bin/escript " \
                "/opt/opscode/embedded/service/opscode-chef-mover/scripts/migrate " \
                "mover_global_groups_migration_callback normal mover_transient_queue_batch_migrator")

    # TODO: add log parsing to ensure migration succeeded

    stop_service("opscode-chef-mover")

  end
end
