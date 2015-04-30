define_upgrade do
  if Partybus.config.bootstrap_server
    must_be_data_master

    # We require the schema to at least contain the tag
    # "policyfile-api-tables"; That tag comes _before_ "@cbv-type", which we
    # upgraded to in 022_cbv_type_addition. Therefore we don't need to run any
    # sqitch migration here.

    # Make sure API is down
    stop_services(["nginx", "opscode-erchef"])

    start_services(["oc_bifrost", "postgresql"])

    force_restart_service("opscode-chef-mover")

    log "Creating default permissions for policyfile endpoints..."

    run_command("/opt/opscode/embedded/bin/escript " +
                "/opt/opscode/embedded/service/opscode-chef-mover/scripts/migrate " +
                "mover_policies_containers_creation_callback " +
                "normal " +
                "mover_transient_queue_batch_migrator")

    stop_services(["opscode-chef-mover"])
  end
end

