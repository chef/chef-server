# This is the chef-mover `phase_2_prep_migration` and `phase_2_migration`
# that migrates containers and groups from couchDB to postgreSQL

define_upgrade do
  if Partybus.config.bootstrap_server

    must_be_data_master

    # Make sure API is down
    stop_services(["nginx", "opscode-erchef"])

    clean_mover_logs

    ####
    #### perform a migration similar to what we did for hosted chef following this plan
    #### github.com/opscode/chef-mover/blob/024875c5545a0e7fb62c0852d4498d2ab7dd1c1d/docs/phase-2-migration-plan.md
    ####

    force_restart_service("opscode-chef-mover")

    # Run phase_2_migration
    log "Migrating containers and groups..."
    run_command("/opt/opscode/embedded/bin/escript " \
                "/opt/opscode/embedded/service/opscode-chef-mover/scripts/migrate mover_phase_2_migration_callback normal")

    stop_service("opscode-chef-mover")

    log "Containers and groups migration complete!"
  end
end
