define_upgrade do
  if Partybus.config.bootstrap_server

    must_be_data_master

    # Make sure API is down
    stop_services(["nginx", "opscode-erchef"])
    # Try to call chef-server-ctl reindex
    start_services(["opscode-chef-mover",
                    "elasticsearch",
                    "nginx",
                    "oc_bifrost",
                    "opscode-erchef",
                    "postgresql",
                    "redis_lb"])

    force_restart_service("opscode-chef-mover")

    sleep 30

    log "All orgs are in the 503 mode..."
    log "Migrating indexed search data..."
    run_command("chef-server-ctl reindex -a")

    stop_services(["opscode-chef-mover",
                    "elasticsearch",
                    "nginx",
                    "oc_bifrost",
                    "opscode-erchef",
                    "postgresql",
                    "redis_lb"])

  end
end

