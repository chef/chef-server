define_upgrade do
    if Partybus.config.bootstrap_server
  
      es = Partybus.config.running_server["private_chef"]["opensearch"]
      version = File.read("/opt/opscode/embedded/opensearch/VERSION")
      # Run these migrations only if elasticsearch has been enabled in the config
      # We do not want to run this if solr is enabled
  
      if es["enable"] && Gem::Version.new(version) == Gem::Version.new("1.2.4")
  
        must_be_data_master

        sleep 30
  
        log "Migrating indexed search data..."
        
        if Dir.exist?("/var/opt/opscode/elasticsearch/data/")
          # Make sure API is down
          stop_services(["nginx", "opscode-erchef", "opensearch"])
          log "Coping Elasticserch data to OpenSearch..."
          run_command("rm -rf /var/opt/opscode/opensearch/data/")
          run_command("cp -r /var/opt/opscode/elasticsearch/data/ /var/opt/opscode/opensearch/")
          run_command("chmod 777 -R /var/opt/opscode/opensearch/data/")
          log "Finished Coping Elasticserch data to OpenSearch..."
          sleep 20
          run_command("rm -rf /var/opt/opscode/elasticsearch")
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
  
  