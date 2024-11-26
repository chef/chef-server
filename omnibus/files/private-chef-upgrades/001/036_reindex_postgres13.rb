define_upgrade do
  if Partybus.config.bootstrap_server
    must_be_data_master
    # Make sure API is down
    stop_services(%w(nginx opscode-erchef))
    start_services(['postgresql'])
    sleep 30
    # Run reindex for postgresql 13
    running_config = Partybus.config.running_server
    embedded_dir = running_config['private_chef']['user']['home']
    unless running_config['private_chef']['postgresql']['external']
      log 'Reindexing postgreSQL...'
      run_command("sudo -H -u  opscode-pgsql bash -c \'#{embedded_dir}/bin/reindexdb --all\'")
    end
  end
end
