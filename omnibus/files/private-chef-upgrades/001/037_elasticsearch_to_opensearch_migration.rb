define_upgrade do
    if Partybus.config.bootstrap_server
      require 'pathname'

      es = Partybus.config.running_server["private_chef"]["opensearch"]
      opscode_erchef = Partybus.config.running_server["private_chef"]["opscode-erchef"]
      embedded_dir = Partybus.config.running_server["private_chef"]["user"]["home"]
      # Return /var/opt/opscode
      var_opt = Pathname.new(Partybus.config.running_server["private_chef"]["upgrades"]["dir"]).parent.to_s
      version = File.read("#{embedded_dir}/opensearch/VERSION")
      # Run these migrations only if elasticsearch has been enabled in the config
      # We do not want to run this if solr is enabled

      if opscode_erchef['search_provider'] == 'opensearch' && !es["external"] && es["enable"] && Gem::Version.new(version) >= Gem::Version.new("1.2.4")

        must_be_data_master

        sleep 30

        log "Migrating indexed search data..."

        if Dir.exist?("#{var_opt}/elasticsearch/data/")
          # Make sure API is down
          stop_services(["nginx", "opscode-erchef", "opensearch"])
          log "Copying Elasticsearch data to OpenSearch..."
          run_command("rm -rf #{var_opt}/opensearch/data/")
          run_command("cp -r #{var_opt}/elasticsearch/data/ #{var_opt}/opensearch/")
          run_command("chmod 777 -R #{var_opt}/opensearch/data/")
          log "Finished Coping Elasticserch data to OpenSearch..."
          sleep 20
          run_command("rm -rf #{var_opt}/elasticsearch")
        else
          start_services(["opensearch",
            "nginx",
            "oc_bifrost",
            "opscode-erchef",
            "postgresql",
            "redis_lb"])
          run_command("chef-server-ctl reindex -a -d -t")
          # Not including postgres in services here
          # since external postgres cannot be managed
          # via chef-server-ctl
          stop_services(["opensearch",
            "nginx",
            "oc_bifrost",
            "opscode-erchef",
            "redis_lb"])
        end
      end
    end
  end
