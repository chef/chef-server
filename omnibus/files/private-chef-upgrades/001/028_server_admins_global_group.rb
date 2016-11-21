define_upgrade do
  if Partybus.config.bootstrap_server
    must_be_data_master
    # Make sure API is down
    stop_services(["nginx", "opscode-erchef"])

    start_services(["oc_bifrost", "postgresql"])
    force_restart_service("opscode-chef-mover")
    log "Creating global group server-admins with read and create access on users container"

    run_command("/opt/opscode/embedded/bin/escript " +
    		"/opt/opscode/embedded/service/opscode-chef-mover/scripts/migrate " +
		"mover_server_admins_global_group_callback " +
		"normal " +
		"mover_transient_queue_batch_migrator")

    stop_services(["opscode-chef-mover"])
  end
end
