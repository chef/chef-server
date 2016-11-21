define_upgrade do
  if Partybus.config.bootstrap_server
    must_be_data_master
    # Make sure API is down
    stop_services(["nginx", "opscode-erchef"])

    start_services(["oc_bifrost", "postgresql"])
    force_restart_service("opscode-chef-mover")
    log "Creating public_key_read_access group for each existing organization"

    run_command("/opt/opscode/embedded/bin/escript " +
                "/opt/opscode/embedded/service/opscode-chef-mover/scripts/migrate " +
                "mover_actor_keys_access_group_callback " +
                "normal " +
                "mover_transient_queue_batch_migrator")

    stop_services(["opscode-chef-mover"])
  end
end
