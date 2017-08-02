
define_upgrade do
  if Partybus.config.bootstrap_server

    must_be_data_master
    config = Partybus.config.running_server['private_chef']
    # This reindex is only run against external elasticsearch -
    # we'll skip it for solr
    # TODO - for sanity, we should check the version. If a customer
    #        has
    #
    if config['opscode-erchef']['search_provider'] == 'elasticsearch' &&
       es_version > Gem::Version.new('5.0.0')

      stop_services(['nginx'])
      if config['opscode-erchef']['search_queue_mode'] == 'rabbitmq'
        start_services(['rabbitmq', 'opscode-expander'])
      end
      log 'Migrating indexed search data to Elasticsearch 5...'
      run_command('/opt/opscode/bin/chef-server-ctl reindex -a')
      stop_services(['opscode-expander', 'rabbitmq'])
    end

  end
end
