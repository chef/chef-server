define_upgrade do
  if Partybus.config.bootstrap_server

    es = Partybus.config.running_server["private_chef"]["elasticsearch"]
    # Run these migrations only if elasticsearch has been enabled in the config
    # We do not want to run this if solr is enabled

    if es["enable"]

      must_be_data_master

      # Make sure API is down
      stop_services(["nginx", "opscode-erchef"])
      # Try to call chef-server-ctl reindex
      start_services(["elasticsearch",
                      "nginx",
                      "oc_bifrost",
                      "opscode-erchef",
                      "postgresql",
                      "redis_lb"])

      sleep 30

      log "All orgs are in the 503 mode..."
      log "Migrating indexed search data..."
      run_command("chef-server-ctl reindex -a -d -t -w")

      stop_services(["elasticsearch",
                     "nginx",
                     "oc_bifrost",
                     "opscode-erchef",
                     "postgresql",
                     "redis_lb"])
    end
  end
end

