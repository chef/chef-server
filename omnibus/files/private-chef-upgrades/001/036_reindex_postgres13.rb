define_upgrade do
  if Partybus.config.bootstrap_server
    must_be_data_master
    # Make sure API is down
    stop_services(%w(nginx opscode-erchef))
    start_services(['postgresql'])
    sleep 30
    # Run reindex for postgresql 13
    running_config = JSON.parse(File.read("/etc/opscode/chef-server-running.json"))
    unless running_config['private_chef']['postgresql']['external']
      log 'Reindexing postgreSQL...'
      run_command("sudo -H -u  opscode-pgsql bash -c \'/opt/opscode/embedded/bin/reindexdb --all\'")
    end
  end
end
